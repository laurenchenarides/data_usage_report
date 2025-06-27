# ======================================================================
# Title: OpenAlex Publications - Ingestion and Master Table Construction
# Author: Lauren Chenarides
# Last Updated: June 10, 2025
#
# Description:
# This script processes segmented OpenAlex seed corpus data (dataset01–dataset12),
# with dataset13 merged into dataset07. This is because the alias for these two are 
# derived from the same parent dataset and therefore can be merged.
# 
# For each dataset, this script:
#   - Reads and extracts main metadata and embedded subtables (e.g., authorships, locations, grants)
#   - Creates tidy, dataset-tagged tables for:
#       - publication metadata (master_openalex_{datasetnum})
#       - topics
#       - author–affiliation–institution information (author_clean)
#       - ROR–institution mappings
#       - annual citation counts (counts_by_year)
#       - grant funders and award info
#   - Binds all individual dataset tables into unified cross-dataset master tables
#   - Collapses duplicate rows by id–doi–ISSN triplets, preserving dataset indicators
#   - Adds derived counts of DOI appearance before and after collapsing
#
# Required Libraries:
#   - dplyr
#   - tidyr
#   - stringr
#   - readr
#   - tibble
#   - purrr
#
# Output Tables:
#   - master_openalex_seed: Combined metadata across all 12 datasets (with 13 merged into 07)
#   - openalex_seed_long: Expanded by multiple ISSNs per DOI
#   - openalex_seed_wide: One row per id–doi–ISSN with max dataset indicators
#   - openalex_seed_doi: DOIs and IDs from each dataset
#   - openalex_seed_topic: Topic, field, domain, and subfield metadata
#   - openalex_seed_author: Author–institution–affiliation cleaned and combined
#   - openalex_seed_ror: Distinct ROR–institution name mappings
#   - openalex_seed_counts_by_year: Yearly citation counts wide format
#   - openalex_all_grants: Funders per publication, wide by grant number
#   - doi_counts_before / doi_counts_after: Summary tables tracking duplication before and after collapse
# ======================================================================

# ============= OpenAlex Seed Corpus Approach Files ============= #

# === Loop through datasets 01 to 13 === #
for (i in 1:13) {
  gc()

  dataset_num <- str_pad(i, 2, pad = "0")

  # Read dataset normally
  seed_path <- file.path(base_path, paste0("flattened_dataset", dataset_num))
  main_file <- file.path(seed_path, paste0("dataset", dataset_num, "_main.csv"))
  main_df <- read_csv(main_file)
  
  # List all CSV files (for extracting prefix names)
  csv_files <- list.files(path = seed_path, pattern = "\\.csv$", full.names = FALSE)
  
  # === Extract embedded tables ===
  extracted_text <- str_extract(csv_files, paste0("(?<=dataset", dataset_num, "_).*(?=\\.csv)")) %>%
    c("locations", "keywords", "concepts", "mesh", "abstract_inverted", 
      "referenced_works", "related_works", "plain_text", "sustainable_development_goals") %>%
    unique()
  
  for (prefix in extracted_text) {
    cols_to_extract <- grep(paste0("^", prefix, "_"), names(main_df), value = TRUE)
    
    if (length(cols_to_extract) > 0) {
      sub_df <- main_df %>%
        select(id, doi, all_of(cols_to_extract)) %>%
        mutate(!!paste0("dataset", dataset_num) := 1)
      
      assign(paste0(sub("^_", "", prefix), dataset_num), sub_df, envir = .GlobalEnv)
      
      main_df <- main_df %>% 
        select(-all_of(cols_to_extract)) 
    }
  }
  
  # === Create master_openalex{dataset_num} ===
  assign(paste0("master_openalex", dataset_num), 
         main_df %>%
           select(id, doi, type, publication_year, cited_by_count) %>%
           rename(year = publication_year, 
                  citation_count = cited_by_count,
                  pub_type = type) %>%
           left_join(get(paste0("primary_location", dataset_num)) %>%
                       select(id,
                              primary_location_is_oa,
                              primary_location_source_display_name,
                              primary_location_source_issn_l,
                              primary_location_source_issn_0,
                              primary_location_source_issn_1,
                              primary_location_source_is_indexed_in_scopus),
                     by = "id") %>%
           mutate(
             is_oa = case_when(
               primary_location_is_oa == TRUE ~ 1,
               primary_location_is_oa == FALSE ~ 0,
               TRUE ~ NA_real_
             )
           ) %>%
           select(-primary_location_is_oa) %>%
           rename(
             indexed_in_scopus = primary_location_source_is_indexed_in_scopus,
             issn_l = primary_location_source_issn_l,
             issn_0 = primary_location_source_issn_0,
             issn_1 = primary_location_source_issn_1,
             journal_title = primary_location_source_display_name
           ) %>%
           mutate(
             !!paste0("dataset", dataset_num) := 1,
             doi = str_remove(doi, "^https://doi.org/")
           ) %>%
           arrange(year, doi),
         envir = .GlobalEnv)
  
  # --- Create openalex{dataset_num}_doi --- #
  assign(paste0("openalex", dataset_num, "_doi"),
         get(paste0("master_openalex", dataset_num)) %>%
           select(id, doi) %>%
           mutate(!!paste0("dataset", dataset_num) := 1),
         envir = .GlobalEnv)
  
  # --- Create openalex{dataset_num}_topic --- #
  assign(paste0("openalex", dataset_num, "_topic"),
         get(paste0("primary_topic", dataset_num)) %>%
           mutate(doi = str_remove(doi, "^https://doi.org/")) %>%
           select(id,
                  doi,
                  primary_topic_display_name,
                  primary_topic_subfield_display_name,
                  primary_topic_field_display_name,
                  primary_topic_domain_display_name) %>%
           rename(topic = primary_topic_display_name,
                  subfield = primary_topic_subfield_display_name,
                  field = primary_topic_field_display_name,
                  domain = primary_topic_domain_display_name) %>%
           mutate(!!paste0("dataset", dataset_num) := 1),
         envir = .GlobalEnv)

  # --- Create openalex{dataset_num}_author --- #
  authorships_df <- get(paste0("authorships", dataset_num))
  
  assign(paste0("openalex", dataset_num, "_author"),
         authorships_df %>%
           pivot_longer(
             cols = matches("^authorships_\\d+_"),
             names_to = c("author_number", ".value"),
             names_pattern = "^authorships_(\\d+)_(.*)"
           ) %>%
           mutate(author_number = as.integer(author_number)) %>%
           filter(!is.na(author_position)) %>%
           select(id, doi, author_number, author_position, author_id, author_display_name, author_orcid),
         envir = .GlobalEnv)
  
  # --- Create openalex{dataset_num}_institutions --- #
  assign(paste0("openalex", dataset_num, "_institutions"),
         authorships_df %>%
           select(id, doi, matches("^authorships_\\d+_institutions_\\d+_(id|display_name|type)")) %>%
           pivot_longer(
             cols = matches("^authorships_\\d+_institutions_\\d+_(id|display_name|type)"),
             names_to = c("author_number", "institution_number", "field"),
             names_pattern = "^authorships_(\\d+)_institutions_(\\d+)_(.*)"
           ) %>%
           filter(value != "funder") %>%
           rename(pub_id = id) %>%
           pivot_wider(
             names_from = field,
             values_from = value
           ) %>%
           rename(institution_id = id, id = pub_id) %>%
           mutate(author_number = as.integer(author_number)) %>%
           select(-institution_id, -type, -institution_number) %>%
           rename(institution_name = display_name),
         envir = .GlobalEnv)
  
  # --- Create openalex{dataset_num}_affils --- #
  assign(paste0("openalex", dataset_num, "_affils"),
         authorships_df %>%
           select(id, doi, matches("^authorships_\\d+_raw_affiliation_strings_\\d+")) %>%
           pivot_longer(
             cols = matches("^authorships_\\d+_raw_affiliation_strings_\\d+"),
             names_to = c("author_number", "affiliation_number"),
             names_pattern = "^authorships_(\\d+)_raw_affiliation_strings_(\\d+)"
           ) %>%
           rename(affiliation = value) %>%
           filter(!is.na(affiliation)) %>%
           select(-affiliation_number) %>%
           mutate(author_number = as.integer(author_number)),
         envir = .GlobalEnv) # df of id, doi, author_number, and affiliation
  
  # --- Create openalex{dataset_num}_author_clean --- #
  assign(paste0("openalex", dataset_num, "_author_clean"),
         get(paste0("openalex", dataset_num, "_author")) %>%
           left_join(get(paste0("openalex", dataset_num, "_affils")), 
                     by = c("id", "doi", "author_number")) %>%
           distinct() %>%
           group_by(id, doi, author_number) %>%
           mutate(first_affiliation = first(affiliation[!is.na(affiliation) & affiliation != ""])) %>%
           ungroup() %>%
           select(-affiliation) %>%
           distinct() %>%
           left_join(get(paste0("openalex", dataset_num, "_institutions")), 
                     by = c("id", "doi", "author_number")) %>%
           distinct() %>%
           group_by(id, doi, author_number) %>%
           mutate(first_inst = first(institution_name[!is.na(institution_name) & institution_name != ""])) %>%
           ungroup() %>%
           select(-institution_name) %>%
           distinct() %>%
           rename(affiliation = first_affiliation,
                  institution_name = first_inst) %>%
           mutate(!!paste0("dataset", dataset_num) := 1),
         envir = .GlobalEnv)
  
  # --- Create openalex{dataset_num}_ror --- #
  assign(paste0("openalex", dataset_num, "_ror"),
         authorships_df %>%
           select(id, doi, matches("^authorships_\\d+_institutions_\\d+_(ror|display_name)")) %>%
           pivot_longer(
             cols = matches("^authorships_\\d+_institutions_\\d+_(ror|display_name)"),
             names_to = c("author_number", "institution_number", "field"),
             names_pattern = "^authorships_(\\d+)_institutions_(\\d+)_(.*)"
           ) %>%
           pivot_wider(
             names_from = field,
             values_from = value
           ) %>%
           select(ror, display_name) %>%
           distinct() %>%
           filter(!is.na(ror)) %>%
           rename(institution_name = display_name) %>%
           mutate(!!paste0("dataset", dataset_num) := 1),
         envir = .GlobalEnv)
  
  message("Finished dataset ", dataset_num)
}


# --- Create counts_by_year_wide{dataset_num} --- #
for (i in 1:13) {
  dataset_num <- str_pad(i, 2, pad = "0")
  df_name <- paste0("counts_by_year", dataset_num)
  df <- get(df_name)
  
  df_long <- df %>%
    select(id, doi, matches("counts_by_year_\\d+_(year|cited_by_count)")) %>%
    pivot_longer(
      cols = -c(id, doi),
      names_to = c("index", "field"),
      names_pattern = "counts_by_year_(\\d+)_(.*)"
    ) %>%
    pivot_wider(
      names_from = field,
      values_from = value
    ) %>%
    filter(!is.na(year)) %>%
    mutate(
      year = as.integer(year),
      cited_by_count = as.integer(cited_by_count)
    )
  
  df_wide <- df_long %>%
    pivot_wider(
      id_cols = c(id, doi),
      names_from = year,
      values_from = cited_by_count,
      names_prefix = "citation_count_"
    ) %>%
    mutate(!!paste0("dataset", dataset_num) := 1)
  
  assign(paste0("counts_by_year_wide", dataset_num), df_wide, envir = .GlobalEnv)
}

# --- Create grants_funders{dataset_num} --- #
for (i in 1:13) {
  dataset_num <- str_pad(i, 2, pad = "0")
  df_name <- paste0("grants", dataset_num)
  df <- get(df_name)
  
  grants_long <- df %>%
    select(id, doi, matches("^grants_\\d+_(funder|funder_display_name)")) %>%
    pivot_longer(
      cols = starts_with("grants_"),
      names_to = c("grant_number", "field"),
      names_pattern = "grants_(\\d+)_(funder|funder_display_name)"
    ) %>%
    group_by(id, doi, grant_number, field) %>%
    summarize(value = paste(na.omit(value), collapse = "; "), .groups = "drop") %>%
    pivot_wider(
      names_from = field,
      values_from = value
    ) %>%
    filter(!is.na(funder), funder != "NA", funder != "") %>%
    mutate(!!paste0("dataset", dataset_num) := 1)
  
  assign(paste0("grants_funders", dataset_num), grants_long, envir = .GlobalEnv)
}

# Combine all grants_funders tables
grants_all <- bind_rows(
  lapply(1:13, function(i) get(paste0("grants_funders", str_pad(i, 2, pad = "0"))))
)

# Clean and reshape
openalex_all_grants <- grants_all %>%
  mutate(
    funder = str_remove(funder, "^https?://openalex.org/[^;]*;\\s*"),
    grant_number = as.integer(grant_number) + 1
  ) %>%
  arrange(id, doi, grant_number) %>%
  group_by(id, doi) %>%
  mutate(row_num = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(id, doi),
    names_from = row_num,
    values_from = funder,
    names_prefix = "funder_"
  )

# Preserve dataset indicators (max = 1 if present in any row)
dataset_flags <- grants_all %>%
  select(id, doi, starts_with("dataset")) %>%
  mutate(across(starts_with("dataset"), ~replace_na(.x, 0))) %>%
  group_by(id, doi) %>%
  summarize(across(everything(), ~ max(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(across(starts_with("dataset"), ~replace_na(.x, 0)))

# Join the two parts together
openalex_all_grants <- openalex_all_grants %>%
  left_join(dataset_flags, by = c("id", "doi"))


# ============== Clean up files ============== # 

# Check data types for bind
check_data_types <- map_dfr(1:13, function(i) {
  df <- get(paste0("master_openalex", str_pad(i, 2, pad = "0")))
  tibble(
    dataset = sprintf("%02d", i),
    column = names(df),
    coltype = sapply(df, class)
  )
}) %>%
  pivot_wider(names_from = dataset, values_from = coltype)

# === Merge dataset13 into dataset07 and de-duplicate === #

master_openalex07 <- bind_rows(master_openalex07, master_openalex13) %>%
  mutate(dataset07 = 1) %>%
  mutate(across(starts_with("dataset"), ~replace_na(.x, 0))) %>%
  arrange(id, doi) %>%
  distinct(id, doi, .keep_all = TRUE) %>%
  select(-dataset13)

openalex_seed_wide <- bind_rows(
  master_openalex01,
  master_openalex02,
  master_openalex03,
  master_openalex04,
  master_openalex05,
  master_openalex06,
  master_openalex07,
  master_openalex08,
  master_openalex09,
  master_openalex10,
  master_openalex11,
  master_openalex12
) 

openalex_seed_wide <- openalex_seed_wide %>%
  mutate(across(starts_with("dataset"), ~replace_na(.x, 0)))

# Number of times the doi appears in `openalex_seed_wide`, before reshaping by ISSN and collapsing by dataset
doi_counts_before <- openalex_seed_wide %>%
  count(doi, name = "doi_n_before") 

openalex_seed_long <- openalex_seed_wide %>%
  pivot_longer(
    cols = c(issn_l, issn_0, issn_1),
    names_to = "issn_source",
    values_to = "ISSN"
  ) %>%
  filter(!is.na(ISSN)) %>%         # remove NA values
  mutate(
    ISSN = str_replace_all(ISSN, "[^A-Za-z0-9]", "")  # remove any non-alphanumeric characters
  ) %>%
  distinct(id, doi, ISSN, 
           dataset01,
           dataset02,
           dataset03,
           dataset04,
           dataset05,
           dataset06,
           dataset07,
           dataset08,
           dataset09,
           dataset10,
           dataset11,
           dataset12,
           .keep_all = TRUE)     # N = 4,296

unique(openalex_seed_long$dataset07)

master_openalex_seed <- openalex_seed_long %>%
  group_by(id, doi, ISSN) %>%
  summarize(across(starts_with("dataset"), ~max(.x, na.rm = TRUE)), .groups = "drop") %>%
  left_join(
    openalex_seed_long %>% 
      select(id, doi, year, pub_type, citation_count, journal_title, ISSN, issn_source, indexed_in_scopus, is_oa) %>%
      distinct(),
    by = c("id", "doi", "ISSN")
  ) %>%
  relocate(issn_source, .after = ISSN) # N = 3,929

# Number of times the doi appears in `master_openalex_all_collapsed`, after reshaping by ISSN and collapsing by dataset
doi_counts_after <- master_openalex_seed %>%
  count(doi, name = "doi_n_after") 

unique(master_openalex_seed$dataset07)

# === Merge openalex07_doi and openalex13_doi === #
openalex07_doi <- bind_rows(openalex07_doi, openalex13_doi) %>%
  mutate(dataset07 = 1) %>%
  mutate(across(starts_with("dataset"), ~replace_na(.x, 0))) %>%
  distinct(id, doi, .keep_all = TRUE) %>%
  select(-dataset13)

# === Merge openalex07_topic and openalex13_topic === #
openalex07_topic <- bind_rows(openalex07_topic, openalex13_topic) %>%
  mutate(dataset07 = 1) %>%
  mutate(across(starts_with("dataset"), ~replace_na(.x, 0))) %>%
  distinct(id, doi, topic, subfield, field, domain, .keep_all = TRUE) %>%
  select(-dataset13)

# === Merge openalex07_author_clean and openalex13_author_clean === #
openalex07_author_clean <- bind_rows(openalex07_author_clean, openalex13_author_clean) %>%
  mutate(dataset07 = 1) %>%
  mutate(across(starts_with("dataset"), ~replace_na(.x, 0))) %>%
  distinct(id, doi, author_number, .keep_all = TRUE) %>%
  select(-dataset13)

# === Merge openalex07_ror and openalex13_ror === #
openalex07_ror <- bind_rows(openalex07_ror, openalex13_ror) %>%
  mutate(dataset07 = 1) %>%
  mutate(across(starts_with("dataset"), ~replace_na(.x, 0))) %>%
  distinct(ror, institution_name, .keep_all = TRUE) %>%
  select(-dataset13)

# === Merge counts_by_year_wide07 and counts_by_year_wide13 === #
counts_by_year_wide07 <- bind_rows(counts_by_year_wide07, counts_by_year_wide13) %>%
  mutate(dataset07 = 1) %>%
  mutate(across(starts_with("dataset"), ~replace_na(.x, 0)),
         across(starts_with("citation_count"), ~replace_na(.x, 0))) %>%
  distinct(id, doi, .keep_all = TRUE) %>%
  select(-dataset13)

# === Bind all openalex_doi tables === #
openalex_seed_doi <- bind_rows(
  lapply(1:12, function(i) get(paste0("openalex", str_pad(i, 2, pad = "0"), "_doi")))
) %>%
  mutate(across(starts_with("dataset"), ~replace_na(.x, 0)))

# === Bind all openalex_topic tables === #
openalex_seed_topic <- bind_rows(
  lapply(1:12, function(i) get(paste0("openalex", str_pad(i, 2, pad = "0"), "_topic")))
) %>%
  mutate(across(starts_with("dataset"), ~replace_na(.x, 0)))

# === Bind all openalex_author_clean tables === #
openalex_seed_author <- bind_rows(
  lapply(1:12, function(i) get(paste0("openalex", str_pad(i, 2, pad = "0"), "_author_clean")))
) %>%
  mutate(across(starts_with("dataset"), ~replace_na(.x, 0))) 

# === Bind all openalex_ror tables === #
openalex_seed_ror <- bind_rows(
  lapply(1:12, function(i) get(paste0("openalex", str_pad(i, 2, pad = "0"), "_ror")))
) %>%
  mutate(across(starts_with("dataset"), ~replace_na(.x, 0)))

# === Bind all counts_by_year_wide tables === #
openalex_seed_counts_by_year <- bind_rows(
  lapply(1:12, function(i) get(paste0("counts_by_year_wide", str_pad(i, 2, pad = "0"))))
) %>%
  mutate(across(starts_with("dataset"), ~replace_na(.x, 0))) %>%
  mutate(across(starts_with("citation_count"), ~replace_na(.x, 0))) %>%
  arrange(doi)

# === Update grants file to combine dataset07 and dataset13 === #
openalex_seed_grants <- openalex_all_grants %>%
  mutate(dataset07 = if_else(dataset07 == 0 & dataset13 == 1, 1L, dataset07)) %>%
  select(-dataset13)

# ============= OpenAlex Full-Text Search Files ============= #

# List and read all CSV files into a named list
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Read all files into a list of dataframes named by table
data_list <- setNames(
  lapply(csv_files, read_csv),
  nm = file_path_sans_ext(basename(csv_files))
)

# Shortcuts to key tables
pubs <- data_list$publication_main

ds_links <- data_list$publication_dataset_links
datasets <- data_list$datasets

auth_links <- data_list$publication_author_links
authors <- data_list$authors

inst_links <- data_list$publication_institution_links
institutions <- data_list$institutions
institutions_v3 <- data_list$institutions_v3

jrnl_links <- data_list$publication_journal_links
journals <- data_list$journals
journals_v3 <- data_list$journals_v3


# ============= Combine DOI list by dataset from OA Seed Corpus and OA Full Text ============= #

# Loop through master_openalex_seed to create a list of dois by dataset
for (i in 1:12) {
  
  # Dynamically construct the variable name
  ds_var <- sprintf("dataset%02d", i)
  
  # Filter and select only the relevant dataset column
  filtered_df <- master_openalex_seed %>%
    filter(.data[[ds_var]] == 1) %>%
    select(id, doi, all_of(ds_var)) %>%
    rename(
      publication_openalex_id = id
    ) %>%
    distinct() %>%
    mutate(method_seed = 1)
  
  # Assign to a new dataframe with suffix _seed
  assign(paste0(ds_var, "_seed"), filtered_df)
}

master_openalex_ft <- pubs %>%
  left_join(ds_links,
            by = "publication_openalex_id",
            relationship = "one-to-many") %>%
  filter(!is.na(dataset_id)) %>%
  mutate(dataset_id = if_else(dataset_id == 13, 7L, dataset_id),
         dataset_key = sprintf("dataset%02d", dataset_id))


# Create separate dataframes by dataset id from master_openalex_ft
for (i in 1:12) {
  ds_label <- sprintf("dataset%02d", i)
  
  filtered_df <- master_openalex_ft %>%
    filter(dataset_key == ds_label) %>%
    mutate(!!ds_label := 1) %>%
    select(publication_openalex_id, doi, all_of(ds_label)) %>%
    mutate(doi = str_remove(doi, "^https://doi.org/")) %>%
    distinct() %>%
    mutate(method_ft = 1)
  
  assign(paste0(ds_label, "_ft"), filtered_df)
}

# Combine and de-duplicate seed and full text results
for (i in 1:12) {
  ds_var <- sprintf("dataset%02d", i)
  
  df <- bind_rows(get(paste0(ds_var, "_seed")), 
                  get(paste0(ds_var, "_ft"))) %>%
    mutate(method_seed = replace_na(method_seed, 0),
           method_ft = replace_na(method_ft, 0)) %>%
    group_by(doi) %>%
    summarize(
      method_seed = max(method_seed, na.rm = TRUE),
      method_ft = max(method_ft, na.rm = TRUE),
      method_seed_ft = ifelse(max(method_seed, na.rm = TRUE) == 1 & 
                                max(method_ft, na.rm = TRUE) == 1, 1, 0),
      .groups = "drop"
    ) %>%
    arrange(-method_seed_ft, doi) %>%
    filter(!is.na(doi))
  
  assign(paste0(ds_var, "_openalex"), df)
}

# Pull consistent information from OpenAlexR
for (i in 1:12) {
  ds_var <- sprintf("dataset%02d_openalex", i)
  
  # Extract unique, non-missing DOIs
  pub_dois <- get(ds_var) %>%
    pull(doi) %>%
    na.omit() %>%
    unique()
  
  # Fetch OpenAlex metadata
  works_meta <- oa_fetch(entity = "works", doi = pub_dois, verbose = FALSE)
  
  # Select and rename the relevant metadata fields
  meta_df <- works_meta %>%
    select(
      doi,
      issn_l,
      publication_year,
      type,
      cited_by_count,
      source_display_name,
      is_oa
    ) %>%
    rename(
      issn = issn_l,
      year = publication_year,
      pub_type = type,
      citation_count = cited_by_count,
      journal_title = source_display_name
    ) %>%
    mutate(doi = str_remove(doi, "^https://doi.org/")) %>%
    left_join(get(ds_var) %>% 
                select(doi, starts_with("method_")),
              by = "doi")
  
  # Save as dataset-specific metadata object
  assign(paste0("metadata_dataset", sprintf("%02d", i)), meta_df)
}


# ================= Join Tables to Create Master OA Dataframe ================= #

# Initialize an empty list to collect processed metadata with dataset flags
metadata_list <- list()

for (i in 1:12) {
  ds_var <- sprintf("metadata_dataset%02d", i)
  df <- get(ds_var)
  
  # Add a column indicating membership in dataset_{num}
  df <- df %>%
    mutate(!!sprintf("dataset%02d", i) := 1) %>%
    select(doi, everything())
  
  metadata_list[[i]] <- df
}

# Merge all metadata with presence flags
master_openalex <- reduce(metadata_list, full_join, 
                          by = c("doi", "issn", "year", "pub_type", "citation_count", "journal_title", 
                                 "is_oa", "method_seed", "method_ft", "method_seed_ft"))

# Replace NAs in dataset columns with 0
for (i in 1:12) {
  colname <- sprintf("dataset%02d", i)
  master_openalex[[colname]] <- replace_na(master_openalex[[colname]], 0)
}

# Deduplicate by doi, taking the max across method_ and dataset columns
dataset_cols <- sprintf("dataset%02d", 1:12)

master_openalex <- master_openalex %>%
  group_by(doi) %>%
  summarize(
    across(c(method_seed, method_ft, method_seed_ft), ~max(.x, na.rm = TRUE)),
    across(all_of(dataset_cols), ~max(.x, na.rm = TRUE)),
    issn = first(issn),
    year = first(year),
    pub_type = first(pub_type),
    citation_count = first(citation_count),
    journal_title = first(journal_title),
    is_oa = first(is_oa),
    .groups = "drop"
  ) %>%
  arrange(doi)
  




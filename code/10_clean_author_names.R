# ========================================================================
# AUTHOR NAME DISAMBIGUATION ACROSS SCOPUS, DIMENSIONS, OPENALEX
# Author: Lauren Chenarides
# Last Updated: June 13, 2025
# ------------------------------------------------------------------------
# This script harmonizes and disambiguates author mentions across four
# bibliometric data sources (Scopus, Dimensions, OpenAlex Full Text,
# OpenAlex Seed). It applies canonical name cleaning, ORCID-based
# entity resolution, canopy-based blocking, and hierarchical clustering
# to identify unique authors across datasets.
#
# Inspired by the PatentsView disambiguation methodology, this process 
# implements their two-stage design: (1) canopy construction for 
# computational scalability and (2) clustering within canopies using 
# name similarity and hierarchical linkage.
# ========================================================================

# -------------------------
# 1. Helper function: Normalize author names
# -------------------------
normalize_name <- function(name) {
  name %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%          # Convert accented or special chars to ASCII
    str_replace_all("\\*", " ") %>%                            # Remove asterisks
    str_replace_all("[\u00A0\u200B]", " ") %>%                 # Replace non-breaking/invisible spaces with space
    str_to_lower() %>%
    str_replace_all("[^[:alnum:][:space:]'\\-\\.]", " ") %>%   # Replace strange characters with space
    str_replace_all("[[:punct:]]", " ") %>%                    # Remove punctuation
    str_squish() %>%                                           # Trim and collapse whitespace
    str_trim()                                                 # Final trim
}

# -------------------------
# 2. Process and clean each source
# -------------------------

# -------  Scopus ------ 
author_scopus_clean <- scopus_author %>%
  left_join(df_scopus %>% select(doi, publication_year),
            by = "doi",
            relationship = "many-to-many") %>%
  filter(publication_year > 2016 & publication_year < 2023) %>%
  mutate(
    given_name = str_replace_all(given_name, "^[^a-zA-Z]+", ""),  # remove leading non-letters
    family_name = str_squish(family_name),
    author_full_name = paste(given_name, family_name)
  ) %>%
  mutate(author_full_name = paste(given_name, family_name)) %>%
  mutate(
    full_name_clean = normalize_name(author_full_name),
    author_alias = str_squish(paste(given_name, family_name)),
    author_id = as.character(author_id),
    author_id = coalesce(author_id, full_name_clean),
    canonical = coalesce(full_name_clean),
    institution = institution_name
  ) %>%
  select(canonical, full_name_clean, institution, author_alias, doi) %>%
  mutate(source_scopus = TRUE) %>%
  arrange(full_name_clean) %>%
  filter(!is.na(canonical) & full_name_clean != "na na") %>%
  distinct()

nrow(author_scopus_clean) # N = 30,981
length(unique(author_scopus_clean$full_name_clean)) # N = 22,729

# ------ Dimensions ------ 
author_dim_clean <- dim_authors %>%
  left_join(df_dimensions %>% select(doi, publication_year),
            by = "doi",
            relationship = "many-to-many") %>%
  filter(publication_year > 2016 & publication_year < 2023) %>%
  mutate(author_full_name = paste(first_name, last_name)) %>%
  mutate(
    full_name_clean = normalize_name(author_full_name),
    author_alias = str_squish(author_full_name),
    author_id = coalesce(orcid, full_name_clean),
    canonical = coalesce(orcid, full_name_clean),
    institution = raw_affiliations
  ) %>%
  select(canonical, full_name_clean, institution, author_alias, doi) %>%
  mutate(source_dimensions = TRUE) %>%
  arrange(full_name_clean) %>%
  filter(!is.na(canonical)) %>%
  distinct()

nrow(author_dim_clean) # N = 31,469
length(unique(author_dim_clean$full_name_clean)) # N = 23,466

# ------ OpenAlex ------ 

# Extract DOIs for OA
oa_dois <- df_openalex %>% pull(doi) # N = 3,709

# Fetch OpenAlex metadata
works_meta <- oa_fetch(entity = "works", doi = oa_dois, verbose = FALSE)
colnames(works_meta)

# Parse OpenAlex Metadata
oa_author_df <- works_meta %>%
  select(doi, authorships) %>%
  unnest(authorships)

length(unique(oa_author_df$doi)) # N = 3,699

author_oa_clean <- oa_author_df %>%
  select(-affiliations) %>%
  mutate(doi = str_remove(doi, "^https://doi.org/"),
         orcid = str_remove(orcid, "^https://orcid.org/")) %>%
  mutate(
    author_full_name = str_replace_all(display_name, "\\*", ""),
    author_full_name = str_replace_all(display_name, "[\u00A0\u200B]", " "),
    author_alias = str_squish(author_full_name),
    full_name_clean = normalize_name(author_alias),
    author_id = coalesce(orcid, full_name_clean),
    canonical = coalesce(orcid, full_name_clean),
    # institution = affiliation_raw
    ) %>%
  select(canonical, full_name_clean, author_alias, doi) %>%
  mutate(source_oa = TRUE) %>%
  arrange(full_name_clean) %>%
  filter(!is.na(canonical)) %>%
  distinct()

nrow(author_oa_clean) # N = 18,360
length(unique(author_oa_clean$full_name_clean)) # N = 14,213
length(unique(author_oa_clean$doi)) # N = 3,699


# -------------------------
# 3. Replace canonical IDs with ORCID when matched by name + DOI
# -------------------------

# Create a list of all dois
all_dois <- df_scopus %>% select(doi) %>%
  bind_rows(df_openalex %>% select(doi)) %>%
  bind_rows(df_dimensions %>% select(doi)) %>%
  distinct() %>% # N = 11,917
  pull(doi)

# Use OA API to fetch ORCIDs, where available
all_works_meta <- oa_fetch(entity = "works", 
                       doi = all_dois, 
                       verbose = FALSE)

# Flatten authorship field and pull ORCIDs
author_meta <- all_works_meta %>%
  select(doi, authorships) %>%
  unnest(authorships) %>%
  distinct(orcid, .keep_all = TRUE) %>%
  select(-is_corresponding, -affiliations) %>%
  arrange(id) %>%
  mutate(doi = str_remove(doi, "^https://doi.org/"),
         orcid = str_remove(orcid, "^https://orcid.org/")) %>%
  arrange(doi)

length(unique(author_meta$doi)) # N = 8,936

# Create a list of just display names and ORCIDs
author_orcid <- author_meta %>%
  select(display_name, orcid) %>%
  distinct() # N = 26,437

nrow(author_orcid)

# Create a list of ORCIDs for authors that appear in Dimensions (for authors of articles not found using the OpenAlex API)
author_orcid_dim <- dim_authors %>%
  select(full_name, orcid) %>%
  rename(display_name = full_name) %>%
  distinct() # N = 37,025

nrow(author_orcid_dim)

# Bind rows for OA hits and Dimensions
author_orcid_all <- author_orcid %>%
  bind_rows(author_orcid_dim) %>%
  distinct() %>% 
  arrange(orcid) %>% # N = 54,081 authors
  mutate(
    full_name_clean = normalize_name(display_name)
  ) %>%
  distinct() 

nrow(author_orcid_all) # N = 54,081

# Create a lookup file for authors by ORCID
author_orcid_lookup <- author_orcid_all %>%
  left_join(author_meta %>%
              select(orcid, doi, display_name), 
            by = c("orcid", "display_name")) %>%
  distinct() %>%
  select(orcid, full_name_clean, doi) %>%
  rename(orcid_lookup = orcid) %>%
  distinct(full_name_clean, doi, .keep_all = TRUE) %>% # N = 53,079
  filter(!is.na(orcid_lookup)) # N = 35,543

nrow(author_orcid_lookup)

# Helper to drop single-letter middle names or initials
clean_middle_initials <- function(name) {
  words <- str_split(name, "\\s+")[[1]]
  if (length(words) <= 2) return(name)  # Do nothing if only 1 or 2 words
  # Remove single-letter words that are not first or last
  cleaned <- c(
    words[1],
    words[2:(length(words)-1)][nchar(words[2:(length(words)-1)]) > 1],
    words[length(words)]
  )
  str_to_lower(str_trim(paste(cleaned, collapse = " ")))
}

replace_with_orcid <- function(df) {
  df %>%
    left_join(author_orcid_lookup, 
              by = c("full_name_clean", "doi"), 
              relationship = "many-to-many") %>%
    mutate(canonical = coalesce(orcid_lookup, canonical)) %>%
    mutate(full_name_clean = map_chr(full_name_clean, clean_middle_initials)) %>%
    select(-orcid_lookup)
}

author_scopus_clean <- replace_with_orcid(author_scopus_clean)
author_dim_clean    <- replace_with_orcid(author_dim_clean)
author_oa_clean   <- replace_with_orcid(author_oa_clean)


# -------------------------
# 4. Generate canopy IDs (blocking key: first initial + last name)
# -------------------------

generate_canopy <- function(df) {
  df %>%
    mutate(
      canopy = str_c(
        str_sub(word(full_name_clean, 1), 1, 1),
        "_",
        word(full_name_clean, -1)
      )
    )
}

author_scopus_clean <- generate_canopy(author_scopus_clean)
author_dim_clean <- generate_canopy(author_dim_clean)
author_oa_clean <- generate_canopy(author_oa_clean)

# -------------------------
# 5. Bind and consolidate all author mentions
# -------------------------

author_master_mentions <- bind_rows(
  author_scopus_clean,
  author_dim_clean,
  author_oa_clean
) %>%
  distinct() %>% # final clean long table: one row per author mention
  arrange(canopy, doi) %>%
  select(canopy, canonical, full_name_clean, author_alias, institution, doi, everything())

# -------------------------
# 6. Within each canopy+DOI, promote ORCID to canonical if available
# -------------------------

author_master_mentions <- author_master_mentions %>%
  group_by(canopy, doi) %>%
  mutate(
    canonical = ifelse(
      any(str_detect(canonical, "^\\d{4}-\\d{4}-\\d{4}-\\d{4}$")), # ORCID present
      canonical[str_detect(canonical, "^\\d{4}-\\d{4}-\\d{4}-\\d{4}$")][1], # use it
      canonical # otherwise keep existing
    )
  ) %>%
  ungroup()

# -------------------------
# 7. Propagate source_* flags across all mentions within the same DOI
# -------------------------

author_master_mentions <- author_master_mentions %>%
  group_by(doi) %>%
  mutate(
    source_scopus     = any(source_scopus, na.rm = TRUE),
    source_dimensions = any(source_dimensions, na.rm = TRUE),
    source_oa         = any(source_oa, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  distinct()


# -------------------------
# Step 1: Filter to Canopy Groups Without ORCID Canonicals
# -------------------------

# Identify ORCID format
is_orcid <- function(x) str_detect(x, "^\\d{4}-\\d{4}-\\d{4}-\\d{4}$")

# Filter only non-ORCID mentions
cluster_input <- author_master_mentions %>%
  filter(!is_orcid(canonical)) %>%
  select(canopy, canonical, full_name_clean, author_alias, institution, doi) %>%
  distinct()

# -------------------------
# Step 2: Cluster Within Each Canopy Using Name Similarity
# -------------------------

# Function to cluster one canopy group
cluster_canopy_group <- function(df, threshold = 0.2) {
  if (nrow(df) <= 1) {
    df$cluster_id <- 1
    return(df)
  }
  
  # Compute string distance matrix
  dist_matrix <- stringdistmatrix(df$full_name_clean, df$full_name_clean, method = "jw")
  hc <- hclust(as.dist(dist_matrix), method = "average")
  
  # Determine number of clusters using threshold
  df$cluster_id <- cutree(hc, h = threshold)
  return(df)
}

# Apply per canopy
clustered_results <- cluster_input %>%
  group_by(canopy) %>%
  group_split() %>%
  map_dfr(cluster_canopy_group)

# -------------------------
# Step 3: Assign Canonical IDs Within Each Cluster
# -------------------------

clustered_results <- clustered_results %>%
  group_by(canopy, cluster_id) %>%
  mutate(
    clustered_canonical = full_name_clean[1]  # or create a synthetic ID like paste0("cluster_", canopy, "_", cluster_id)
  ) %>%
  ungroup()

# -------------------------
# Step 4: Merge Clustered Canonical Back Into Full Mentions Table
# -------------------------

clustered_results_filtered <- clustered_results %>%
  filter(!str_detect(canonical, "^\\d{4}-\\d{4}-\\d{4}-\\d{4}$"))

author_clustered <- author_master_mentions %>%
  left_join(
    clustered_results_filtered %>% 
      select(canonical, doi, author_alias, clustered_canonical),
    by = c("canonical", "doi", "author_alias"), 
    relationship = "many-to-many"
  ) %>%
  mutate(
    final_canonical = coalesce(clustered_canonical, canonical)
  ) %>%
  select(-clustered_canonical)

# -------------------------
# 8. Pivot institutions wide: one row per author per DOI, with institution_1, institution_2, ...
# -------------------------

author_master_wide <- author_clustered %>%
  # Keep relevant columns
  select(
    canopy, final_canonical, full_name_clean, author_alias, doi,
    institution,
    source_scopus, source_dimensions, source_oa
  ) %>%
  distinct() %>%
  # Assign a numbered row to each institution per author-doi
  group_by(canopy, final_canonical, full_name_clean, author_alias, doi) %>%
  mutate(inst_num = row_number()) %>%
  ungroup() %>%
  # Pivot institutions wide
  pivot_wider(
    names_from = inst_num,
    values_from = institution,
    names_prefix = "institution_"
  ) %>%
  distinct()

author_master_final <- author_master_wide %>%
  arrange(doi, canopy) %>%
  mutate(
    institution_primary = coalesce(institution_1, institution_2, institution_3)
  ) %>%
  select(-institution_1, -institution_2, -institution_3)

length(unique(author_master_final$doi)) # N = 10,585 publications 

# -------------------------
# ALIGNMENT WITH PATENTSVIEW METHODOLOGY
# -------------------------

# This approach implements core principles of the PatentsView disambiguation pipeline:

# 1. **Canopy construction (blocking)**:
#    - Authors are grouped using a blocking key based on first initial + last name.
#    - This reduces the space of pairwise comparisons and mirrors the canopy logic in PatentsView’s inventor disambiguation.

# 2. **Cross-source consolidation**:
#    - Mentions from Scopus, Dimensions, and OpenAlex are harmonized using cleaned names and associated DOIs.
#    - Canonical identifiers are resolved using ORCID when available, similar to the use of PermID for assignees in PatentsView.

# 3. **Hierarchical clustering within canopies**:
#    - For authors without ORCID, we compute string-based name similarity (Jaro-Winkler distance).
#    - We then apply hierarchical agglomerative clustering (average linkage) to form clusters within each canopy.
#    - Canonical IDs are reassigned based on cluster-level resolution (e.g., first mention in a cluster), following PatentsView's "similarity + merge" model.

# 4. **Cluster-wide propagation**:
#    - Once clusters are defined, canonical IDs and source coverage are propagated across mentions sharing the same DOI.
#    - This enables accurate downstream aggregation, comparison, and visualization of author-level metadata.

# The resulting data structure provides both long-format traceability (author mentions) and wide-format integration 
# (institutional affiliations per author per DOI), enabling scalable, cross-platform author disambiguation.

# -------------------------
# Author Summary Tables
# -------------------------

author_canopy_lookup <- author_master_final %>%
  select(canopy, final_canonical) %>%
  distinct() # N = 37,901

nrow(author_canopy_lookup)

# Identify final_canonical IDs with multiple canopies
duplicate_canopies <- author_canopy_lookup %>%
  group_by(final_canonical) %>%
  filter(n_distinct(canopy) > 1) %>%
  arrange(final_canonical, canopy)

# View the full set
View(duplicate_canopies)
# Duplicates canopies are due to accents for some letters, or slight variation in name, but the ORCID is the main canonical. 

author_canopy_lookup <- author_canopy_lookup %>%
  group_by(final_canonical) %>%
  summarize(canopy = first(canopy), .groups = "drop") %>%
  filter(final_canonical != "") # N = 37,858

nrow(author_canopy_lookup)

# Count distinct authors by source
distinct_author_counts <- author_master_final %>%
  mutate(source = case_when(
    source_scopus ~ "scopus",
    source_dimensions ~ "dimensions",
    source_oa ~ "openalex"
  )) %>%
  distinct(source, final_canonical) %>%
  count(source, name = "n_distinct_authors")

print(distinct_author_counts)

# Count distinct authors per source group

# Step 1: Drop DOI and deduplicate author identities
author_lookup_clean <- author_master_final %>%
  select(final_canonical, source_scopus, source_dimensions, source_oa) %>%
  group_by(final_canonical) %>%
  summarise(
    scopus_yes     = any(source_scopus, na.rm = TRUE),
    dim_yes        = any(source_dimensions, na.rm = TRUE),
    oa_yes         = any(source_oa, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Assign group labels (mutually exclusive categories)
author_lookup_clean <- author_lookup_clean %>%
  mutate(
    group = case_when(
      scopus_yes & !oa_yes & !dim_yes ~ "Scopus only",
      !scopus_yes & oa_yes & !dim_yes ~ "OpenAlex only",
      !scopus_yes & !oa_yes & dim_yes ~ "Dimensions only",
      scopus_yes & oa_yes & !dim_yes ~ "Scopus ∩ OpenAlex",
      scopus_yes & !oa_yes & dim_yes ~ "Scopus ∩ Dimensions",
      !scopus_yes & oa_yes & dim_yes ~ "OpenAlex ∩ Dimensions",
      scopus_yes & oa_yes & dim_yes ~ "Scopus ∩ OpenAlex ∩ Dimensions",
      TRUE ~ "Other/Unclassified"
    )
  )

# Step 3: Count distinct authors per group
author_group_counts <- author_lookup_clean %>%
  count(group, name = "n_distinct_authors") %>%
  arrange(desc(n_distinct_authors))

print(author_group_counts)

# ------ Topic Summary Tables (Distinct Author Counts): Disaggregated as in Treemaps -------

# ======= Distinct Author Counts by Topic and Source Overlap Group ======= #

# ---- Prerequisite: Create an author-DOI lookup ---- #
author_doi_lookup <- author_master_final %>%
  select(final_canonical, doi) %>%
  distinct()

# ---- Helper: Extract author-topic pairs per dataset and source ---- #
extract_topic_authors <- function(df, prefix, source_flag) {
  topic_cols <- grep(paste0("^", prefix, "_topic_"), names(df), value = TRUE)
  
  if (length(topic_cols) == 0) {
    message(paste("No topic columns found for prefix:", prefix))
    return(tibble(topic = character(), final_canonical = character()))
  }
  
  df %>%
    filter(.data[[source_flag]] == 1) %>%
    select(doi, all_of(topic_cols)) %>%
    pivot_longer(cols = all_of(topic_cols), names_to = "col", values_to = "topic") %>%
    filter(!is.na(topic)) %>%
    mutate(topic = str_to_title(trimws(topic))) %>%
    distinct(topic, doi) %>%
    left_join(author_doi_lookup, by = "doi") %>%
    distinct(topic, final_canonical)
}

dataset_titles <- c(
  "dataset01_pubs" = "ARMS",
  "dataset02_pubs" = "CPS Food Security Supplement",
  "dataset03_pubs" = "Farm to School Census",
  "dataset04_pubs" = "IRI InfoScan",
  "dataset05_pubs" = "Census of Agriculture",
  "dataset06_pubs" = "Rural-Urban Continuum Code",
  "dataset07_pubs" = "The TOTAL Survey",
  "dataset08_pubs" = "Food Access Research Atlas",
  "dataset09_pubs" = "Food Acquisition and Purchase Survey",
  "dataset10_pubs" = "Household Food Security Survey Module",
  "dataset11_pubs" = "Local Food Marketing Practices Survey",
  "dataset12_pubs" = "Quarterly Food at Home Price Database"
)

# ---- Loop through each dataset ---- #
for (i in 1:12) {
  ds_id <- sprintf("dataset%02d", i)
  df <- get(paste0(ds_id, "_topics"))
  label <- dataset_titles[paste0(ds_id, "_pubs")]
  
  # Step 1: Extract author-topic pairs by source
  scopus_df   <- extract_topic_authors(df, "s", "scopus_yes")
  oa_df       <- extract_topic_authors(df, "oa", "oa_yes")
  dim_df      <- extract_topic_authors(df, "dim", "dim_yes")
  
  # Step 2: Add source flags
  scopus_df$scopus_yes <- 1; scopus_df$oa_yes <- 0; scopus_df$dim_yes <- 0
  oa_df$scopus_yes <- 0; oa_df$oa_yes <- 1; oa_df$dim_yes <- 0
  dim_df$scopus_yes <- 0; dim_df$oa_yes <- 0; dim_df$dim_yes <- 1
  
  # Step 3: Combine and group by author
  combined <- bind_rows(scopus_df, oa_df, dim_df) %>%
    group_by(topic, final_canonical) %>%
    summarize(across(c(scopus_yes, oa_yes, dim_yes), max), .groups = "drop") %>%
    mutate(group = case_when(
      scopus_yes == 1 & oa_yes == 0 & dim_yes == 0 ~ "Scopus only",
      scopus_yes == 0 & oa_yes == 1 & dim_yes == 0 ~ "OpenAlex only",
      scopus_yes == 0 & oa_yes == 0 & dim_yes == 1 ~ "Dimensions only",
      scopus_yes == 1 & oa_yes == 1 & dim_yes == 0 ~ "Scopus ∩ OpenAlex",
      scopus_yes == 1 & oa_yes == 0 & dim_yes == 1 ~ "Scopus ∩ Dimensions",
      scopus_yes == 0 & oa_yes == 1 & dim_yes == 1 ~ "OpenAlex ∩ Dimensions",
      scopus_yes == 1 & oa_yes == 1 & dim_yes == 1 ~ "Scopus ∩ OpenAlex ∩ Dimensions",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(group))
  
  # Step 4: Count authors by topic and group
  group_levels <- c(
    "Scopus only",
    "OpenAlex only",
    "Dimensions only",
    "Scopus ∩ OpenAlex",
    "Scopus ∩ Dimensions",
    "OpenAlex ∩ Dimensions",
    "Scopus ∩ OpenAlex ∩ Dimensions"
  )
  
  count_by_group <- combined %>%
    count(topic, group, name = "count") %>%
    mutate(group = factor(group, levels = group_levels)) %>%
    complete(topic, group = group_levels, fill = list(count = 0)) %>%
    pivot_wider(
      names_from = group,
      values_from = count,
      values_fill = 0
    )
  
  total_counts <- combined %>%
    group_by(topic) %>%
    summarize(
      count_scopus   = sum(scopus_yes),
      count_oa       = sum(oa_yes),
      count_dim      = sum(dim_yes),
      .groups = "drop"
    )
  
  true_total <- combined %>%
    count(topic, name = "total_authors")
  
  # Merge and save
  all_topics <- count_by_group %>%
    left_join(true_total, by = "topic") %>%
    arrange(desc(total_authors))
  
  safe_title <- gsub("[^A-Za-z0-9]+", "_", label) %>%
    tolower() %>%
    trimws()
  
  write_csv(all_topics, file.path(output_dir_tables, paste0(safe_title, "_authors_long_table.csv")))
}


all_cite_counts <- all_works_meta %>%
  select(doi, cited_by_count) %>%
  unnest(cited_by_count) %>%
  mutate(doi = str_remove(doi, "^https://doi.org/"))

length(unique(all_cite_counts$doi)) # N = 11,293
length(unique(author_master_final$doi)) # N = 10,585

# --------- Top Authors by Source --------

for (i in 1:12) {
  ds_id <- sprintf("dataset%02d", i)
  df <- get(paste0(ds_id, "_df"))
  dataset_label <- dataset_titles[[paste0(ds_id, "_pubs")]]
  
  # Merge in author and citation data
  merged <- df %>%
    select(doi, scopus_yes, oa_yes, dim_yes) %>%
    distinct() %>%
    left_join(author_master_final, by = "doi") %>%
    left_join(all_cite_counts, by = "doi")  
  
  # Function to summarize authors for a given source
  summarize_authors <- function(data, source_flag) {
    data %>%
      filter(.data[[source_flag]] == 1) %>%
      group_by(final_canonical) %>%
      summarize(
        author_name = first(author_alias),
        n_publications = n_distinct(doi),
        total_citations = sum(cited_by_count, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(final_canonical))
  }
  
  # Create summary tables
  scopus_summary <- summarize_authors(merged, "scopus_yes")
  oa_summary     <- summarize_authors(merged, "oa_yes")
  dim_summary    <- summarize_authors(merged, "dim_yes")
  
  # Rank top 50 authors by each metric
  top_scopus_pubs <- scopus_summary %>% arrange(desc(n_publications), desc(total_citations)) %>% slice_head(n = 50)
  top_scopus_cite <- scopus_summary %>% arrange(desc(total_citations), desc(n_publications)) %>% slice_head(n = 50)
  
  top_oa_pubs <- oa_summary %>% arrange(desc(n_publications), desc(total_citations)) %>% slice_head(n = 50)
  top_oa_cite <- oa_summary %>% arrange(desc(total_citations), desc(n_publications)) %>% slice_head(n = 50)
  
  top_dim_pubs <- dim_summary %>% arrange(desc(n_publications), desc(total_citations)) %>% slice_head(n = 50)
  top_dim_cite <- dim_summary %>% arrange(desc(total_citations), desc(n_publications)) %>% slice_head(n = 50)
  
  # Create Excel workbook
  wb <- createWorkbook()
  addWorksheet(wb, "Scopus - Top by Pubs")
  addWorksheet(wb, "Scopus - Top by Cites")
  addWorksheet(wb, "OpenAlex - Top by Pubs")
  addWorksheet(wb, "OpenAlex - Top by Cites")
  addWorksheet(wb, "Dimensions - Top by Pubs")
  addWorksheet(wb, "Dimensions - Top by Cites")
  
  writeData(wb, "Scopus - Top by Pubs", top_scopus_pubs)
  writeData(wb, "Scopus - Top by Cites", top_scopus_cite)
  writeData(wb, "OpenAlex - Top by Pubs", top_oa_pubs)
  writeData(wb, "OpenAlex - Top by Cites", top_oa_cite)
  writeData(wb, "Dimensions - Top by Pubs", top_dim_pubs)
  writeData(wb, "Dimensions - Top by Cites", top_dim_cite)
  
  # Save file
  safe_title <- gsub("[^A-Za-z0-9]+", "_", dataset_label) %>%
    tolower() %>%
    trimws()
  
  saveWorkbook(
    wb,
    file = file.path(output_dir_tables, paste0(safe_title, "_top_50_authors.xlsx")),
    overwrite = TRUE
  )
}

# -------- Count Distinct Authors by Source and Dataset --------

# Initialize empty list to collect results
author_counts_list <- list()

for (i in 1:12) {
  ds_id <- sprintf("dataset%02d", i)
  df <- get(paste0(ds_id, "_df"))
  dataset_label <- dataset_titles[[paste0(ds_id, "_pubs")]]
  
  # Join with author data
  merged <- df %>%
    select(doi, scopus_yes, oa_yes, dim_yes) %>%
    distinct() %>%
    left_join(author_master_final, by = "doi") %>%
    filter(!is.na(final_canonical))
  
  # Count distinct authors using each source
  counts <- tibble(
    dataset = dataset_label,
    authors_scopus    = n_distinct(merged$final_canonical[merged$scopus_yes == 1]),
    authors_openalex  = n_distinct(merged$final_canonical[merged$oa_yes == 1]),
    authors_dimensions = n_distinct(merged$final_canonical[merged$dim_yes == 1])
  )
  
  author_counts_list[[i]] <- counts
}

# Combine all rows into one table
author_counts_summary <- bind_rows(author_counts_list)

print(author_counts_summary)

# View or write to CSV
# View(author_counts_summary)
# write_csv(author_counts_summary, file.path(output_dir_tables, "distinct_author_counts_by_source.csv"))

# -------------------
# Visualizations
# -------------------

# Preprocess Author Labels
author_labels_clean <- author_master_final %>%
  left_join(author_canopy_lookup, by = c("final_canonical" = "canopy")) %>%
  filter(!is.na(full_name_clean)) %>%
  group_by(final_canonical) %>%
  summarize(
    author_name = str_to_title(first(full_name_clean)),
    .groups = "drop"
  ) %>%
  mutate(author_name = iconv(author_name, from = "UTF-8", to = "ASCII//TRANSLIT")) %>%
  mutate(author_name = gsub("[^[:alnum:][:space:]'\\-\\.]", " ", author_name)) %>%
  mutate(author_name = gsub("\\s+", " ", author_name) %>% trimws())


for (i in 1:12) {
  ds_id <- sprintf("dataset%02d", i)
  df <- get(paste0(ds_id, "_df"))
  dataset_label <- dataset_titles[[paste0(ds_id, "_pubs")]]
  
  # Step 1: Merge in author + citation data
  merged <- df %>%
    select(doi, scopus_yes, oa_yes, dim_yes) %>%
    distinct() %>%
    left_join(author_master_final, by = "doi") %>%
    left_join(all_cite_counts, by = "doi")  
  
  # Step 2: Summarize authors
  summarize_authors <- function(data, source_flag) {
    data %>%
      filter(.data[[source_flag]] == 1) %>%
      group_by(final_canonical) %>%
      summarize(
        n_publications = n_distinct(doi),
        total_citations = sum(cited_by_count, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(final_canonical)) 
  }
  
  scopus_summary <- summarize_authors(merged, "scopus_yes")
  oa_summary     <- summarize_authors(merged, "oa_yes")
  dim_summary    <- summarize_authors(merged, "dim_yes")
  
  # Step 3: Create top 20 list
  top_lists <- bind_rows(
    scopus_summary %>% arrange(desc(n_publications)) %>% slice_head(n = 20) %>% mutate(source = "Scopus"),
    oa_summary     %>% arrange(desc(n_publications)) %>% slice_head(n = 20) %>% mutate(source = "OpenAlex"),
    dim_summary    %>% arrange(desc(n_publications)) %>% slice_head(n = 20) %>% mutate(source = "Dimensions")
  )
  
  # Step 4: Identify overlap by counting source appearances
  top_overlap <- top_lists %>%
    select(final_canonical, source) %>%
    distinct() %>%
    mutate(present = 1) %>%
    pivot_wider(names_from = source, values_from = present, values_fill = 0) %>%
    mutate(n_sources = Scopus + OpenAlex + Dimensions)

  # Step 5: Prepare plot data
  plot_data <- top_overlap %>%
    left_join(author_labels_clean, by = "final_canonical") %>%
    pivot_longer(cols = c("Scopus", "OpenAlex", "Dimensions"), names_to = "source", values_to = "present") %>%
    filter(present == 1)
  
  # Step 6: Get distinct author counts from the summary table to be used in the caption
  counts <- author_counts_summary %>% filter(dataset == dataset_label)
  
  # Create a dynamic caption
  dynamic_caption <- glue::glue(
    "This figure shows the top 20 authors by publication count for each \nsource. Differences in author rankings reflect how platform-specific \nindexing affects who appears as a leading user of a dataset—that is, \nresearchers who most frequently publish work referencing or using it. \nAccording to Scopus, this dataset has been used by {counts$authors_scopus} distinct \nauthors; OpenAlex identifies {counts$authors_openalex} distinct users; \nand Dimensions includes {counts$authors_dimensions}."
  )
  
  # Step 7: Plot
  p <- ggplot(plot_data, aes(x = source, y = fct_reorder(author_name, n_sources))) +
    geom_tile(aes(fill = source), color = "white") +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = dataset_label,
      subtitle = "Top 20 Authors by Source",
      caption = dynamic_caption,
      x = "Source",
      y = "Author (ordered by overlap)"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 6),
      plot.caption = element_text(hjust = 0),
      plot.caption.position = "plot"
    )
  
  # Save plot
  safe_title <- gsub("[^A-Za-z0-9]+", "_", dataset_label) %>% tolower()
  CairoPNG(filename = file.path(output_dir_vis, paste0(safe_title, "_top_20_author_overlap.png")), 
           width = 800, height = 1000, res = 150)
  
  print(p)
  dev.off()
}

# === Save full author-level tables for each source === #
all_author_summaries <- list()

for (i in 1:12) {
  ds_id <- sprintf("dataset%02d", i)
  df <- get(paste0(ds_id, "_df"))
  dataset_label <- dataset_titles[[paste0(ds_id, "_pubs")]]
  
  merged <- df %>%
    select(doi, scopus_yes, oa_yes, dim_yes) %>%
    distinct() %>%
    left_join(author_master_final, by = "doi") %>%
    left_join(all_cite_counts, by = "doi")
  
  summarize_authors <- function(data, source_flag, source_name) {
    data %>%
      filter(.data[[source_flag]] == 1) %>%
      group_by(final_canonical) %>%
      summarize(
        dataset = dataset_label,
        source = source_name,
        n_publications = n_distinct(doi),
        total_citations = sum(cited_by_count, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  scopus <- summarize_authors(merged, "scopus_yes", "Scopus")
  oa     <- summarize_authors(merged, "oa_yes", "OpenAlex")
  dim    <- summarize_authors(merged, "dim_yes", "Dimensions")
  
  all_author_summaries[[ds_id]] <- bind_rows(scopus, oa, dim)
}

# Combine all results
author_summary_combined <- bind_rows(all_author_summaries) %>%
  left_join(author_labels_clean, by = "final_canonical") %>%
  select(dataset, source, author_name, final_canonical, n_publications, total_citations) %>%
  arrange(dataset, source, desc(n_publications)) %>%
  filter(final_canonical!="NA")

# Write to CSV
write_csv(
  author_summary_combined,
  file = file.path(output_dir_vis, "all_author_summaries_by_source.csv")
)

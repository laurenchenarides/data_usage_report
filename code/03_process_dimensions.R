# ============================================================================
# Title: Dimensions XLSX Files - Ingestion and Master Table Construction
# Author: Lauren Chenarides
# Last Updated: May 17, 2025
#
# Description:
# This script reads in Dimensions Excel files containing publication metadata
# for USDA-related datasets. It processes four worksheets from each file 
# ("Publications", "Authors", "Concepts", "Affiliations") and merges them into 
# unified data frames with standardized dataset identifiers.
#
# Steps:
#   - Normalize file names to snake_case for matching
#   - Match each file to its dataset_id using `dataset_reference_table`
#   - Manually merge dataset13 into dataset07
#   - Read each sheet and tag rows with the dataset_id
#   - Combine all data into four master data frames:
#       * `publications`, `authors`, `concepts`, `affiliations`
#   - Process the `publications` sheet:
#       * Filter to type == "article"
#       * Split multiple ISSNs into long format
#       * Drop unneeded variables and rename journal fields
#
# Required Libraries:
#   - dplyr, tidyr, stringr, readr, tools, tibble, purrr, readxl
#
# Output Tables:
#   - `dim_publications`: one row per (DOI × ISSN × dataset_id) for article-type records
#   - `dim_authors`, `dim_concepts`, `dim_affiliations`: long-format tagged metadata tables
#   - `master_dimensions`: one row per (DOI x ISSN) with columns for dataset_ids (binary)
#
# Notes:
#   - Make sure dataset_reference_table includes filenames that match XLSX files.
#   - Script assumes consistent worksheet structure across all files.
# ============================================================================

# ============= Dimensions Files ============= #

# Read in the crosswalk between parent_alias_id (from Scopus) and dataset_id (from OpenAlex)
dataset_reference_table <- read_csv(file.path(base_path, "dataset_reference_table_parent_alias.csv"))

# === Normalize function to snake_case filenames ===
to_snake <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("_json$", "") %>%
    str_replace_all("^_|_$", "")
}

# === Prepare dataset_reference_table ===
dataset_reference_table <- dataset_reference_table %>%
  mutate(filename_snake = to_snake(filename))

# === Get .xlsx files ===
xlsx_files <- list.files(input_folder, pattern = "\\.xlsx$", full.names = TRUE)

# Initialize empty data frames
publications <- tibble()
authors <- tibble()
concepts <- tibble()
affiliations <- tibble()

# === Loop over files ===
for (file in xlsx_files) {
  
  file_snake <- to_snake(file_path_sans_ext(basename(file)))
  
  matched_row <- dataset_reference_table %>%
    filter(str_detect(file_snake, filename_snake)) %>%
    slice_head(n = 1)
  
  dataset_id <- ifelse(nrow(matched_row) > 0, matched_row$dataset_id, NA_character_)
  
  # Force merge of dataset13 into dataset07
  if (dataset_id == "dataset13") dataset_id <- "dataset07"
  
  if (is.na(dataset_id)) {
    warning(paste("No dataset_id match found for:", file_snake))
    next
  }
  
  # Read each worksheet with tagging
  pub_df <- read_excel(file, sheet = "Publications") %>%
    mutate(dataset_id = dataset_id)
  
  aut_df <- read_excel(file, sheet = "Authors") %>%
    mutate(dataset_id = dataset_id)
  
  con_df <- read_excel(file, sheet = "Concepts") %>%
    mutate(dataset_id = dataset_id)
  
  aff_df <- read_excel(file, sheet = "Affiliations") %>%
    mutate(dataset_id = dataset_id)
  
  # Append
  publications <- bind_rows(publications, pub_df)
  authors <- bind_rows(authors, aut_df)
  concepts <- bind_rows(concepts, con_df)
  affiliations <- bind_rows(affiliations, aff_df)
}

dim_authors <- authors %>%
  left_join(publications %>%
              select(id, doi), 
            by = c("publication_id" = "id"),
            relationship = "many-to-many") %>%
  distinct(id, .keep_all = TRUE) %>%
  arrange(publication_id, id)

dim_concepts <- concepts %>%
  left_join(publications %>%
              select(id, doi), 
            by = c("publication_id" = "id"),
            relationship = "many-to-many") %>%
  distinct(publication_id, concept, .keep_all = TRUE) %>%
  arrange(publication_id, -relevance)

# Clean and split ror_list column
dim_affiliations <- affiliations %>%
  left_join(dim_authors %>%
              select(id, doi, publication_id), 
            by = c("author_id" = "id"),
            relationship = "many-to-many") %>%
  mutate(ror_list = str_extract_all(ror_list, "'([^']+)'")) %>%  # extract everything between single quotes
  unnest_longer(ror_list) %>%                                    # turn list column into long format
  mutate(ror_list = str_remove_all(ror_list, "'")) %>%           # remove leftover single quotes if any
  select(-ror) %>%
  distinct(org_id, id, author_id, publication_id, .keep_all = TRUE) %>%
  arrange(publication_id, author_id)

dim_publications <- publications %>%
  # Remove leading/trailing whitespace from ISSN field
  mutate(issn = str_trim(issn)) %>%
  
  # Split ISSNs on commas and reshape to long
  separate_rows(issn, sep = ",\\s*") %>%
  
  # Keep only type="articles"
  # filter(type=="article") %>%
  rename(pub_type = type) %>%
  
  # Drop vars not needed
  select(-abstract, -dimensions_url, -linkout, -isbn, -journal_lists) %>%
  
  # Rename some vars
  rename(journal_id = journal.id,
         journal_title = journal.title) %>%
  distinct() # N = 11,961
  
master_dimensions <- dim_publications %>%
  mutate(has_dataset = 1) %>%
  
  # Deduplicate first to avoid repeated rows for the same doi × issn × dataset_id
  distinct(doi, issn, dataset_id, .keep_all = TRUE) %>%
  arrange(dataset_id, doi, issn) %>%
  
  # Reshape so that each dataset_id becomes a column with binary flags
  pivot_wider(
    id_cols = c(doi, issn),
    names_from = dataset_id,
    values_from = has_dataset,
    values_fill = list(has_dataset = 0)
  ) %>%
  
  # Join back metadata, taking the most complete (or most frequent) info
  left_join(
    dim_publications %>%
      group_by(doi, issn, pub_type) %>%
      summarise(
        id = first(id),
        title = first(title),
        year = max(year, na.rm = TRUE),
        journal_title = first(journal_title),
        .groups = "drop"
      ),
    by = c("doi", "issn")
  ) %>%
  
  relocate(id, title, year, journal_title, .after = issn) %>%
  distinct() %>%
  mutate(
    issn = str_replace_all(issn, "[^A-Za-z0-9]", "")  # remove any non-alphanumeric characters
  ) %>%
  
  relocate(pub_type, .after = doi)



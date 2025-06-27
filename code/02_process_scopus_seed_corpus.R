# ======================================================================
# Title: Scopus - Dataset Ingestion and Master Table Construction
# Author: Lauren Chenarides
# Last Updated: May 17, 2025
#
# Description:
# This script processes Scopus export data to construct a harmonized master
# dataset aligned with OpenAlex identifiers. It performs the following:
#   - Loads all raw Scopus CSV files and assigns them to dataframes
#   - Builds a cleaned alias crosswalk from Scopus (parent_alias_id) to OpenAlex (dataset_id)
#   - Joins publication–dataset dyads with models and metadata for alignment
#   - Filters only where `pub_type` = "Article (from publication dataframe)
#   - Merges dataset13 into dataset07 based on overlapping alias structure
#   - Constructs wide-format tables with one row per publication–DOI
#   - Adds journal metadata (title, ISSN, CiteScore)
#   - Creates cleaned and structured tables:
#       - `master_scopus`: Raw filtered metadata for articles
#       - `master_scopus_wide`: Dataset flags across the 12 harmonized USDA datasets
#       - `scopus_doi`: Publication ID–DOI mapping
#       - `scopus_author`: Author affiliations and positions
#       - `scopus_topic`: ASJC field codes reshaped wide per publication
#   - Final step includes a test match of Scopus to OpenAlex (dataset01) via DOI–ISSN
#
# Note:
# Dataset13 is a subset of Dataset07 based on search term specificity. To avoid double-counting,
# records flagged under dataset13 are folded into dataset07 prior to final dataset construction.
#
# Required Libraries:
#   - readr
#   - dplyr
#   - tidyr
#   - stringr
#   - tools
#
# Output Tables:
#   - `master_scopus`: Filtered Scopus article metadata
#   - `scopus_pubs`: One row per DOI with dataset01–dataset12 flags
#   - `dyad_model_dataset`: Crosswalk between Scopus aliases and OpenAlex datasets
#   - `scopus_author`: Author affiliations and location metadata
#   - `scopus_doi`: Unique publication ID–DOI pairs
#   - `scopus_topic`: Wide-format ASJC topic labels by DOI
# ======================================================================

# ============= Scopus Files ============= #

# List all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Loop over files and assign each one to a separate dataframe
for (file in csv_files) {
  # Create a clean dataframe name by removing folder path and ".csv"
  df_name <- file_path_sans_ext(basename(file))
  
  # Read the CSV
  data <- read_csv(file)
  
  # Assign it to the global environment
  assign(df_name, data)
}

gc()

# Read in the crosswalk between parent_alias_id (from Scopus) and dataset_id (from OpenAlex)
dataset_reference_table <- read_csv(file.path(base_path, "dataset_reference_table_parent_alias.csv"))

# Create a distinct table of all parent_alias_ids. Need this to create a crosswalk between Scopus and OpenAlex. 
distinct_alias_table <- dataset_alias %>%
  select(parent_alias_id, alias_id, alias) %>%
  distinct() %>%
  arrange(parent_alias_id, alias, alias_id) 

dyad_model_dataset <- dyad %>%
  select(id, publication_id, dataset_alias_id, alias_id) %>%
  left_join(dyad_model %>%
              select(-run_id, -last_updated_date), 
            by = c("id" = "dyad_id")) %>%
  rename(dyad_id = id.y) %>%
  left_join(model %>%
              select(id, name), 
            by = c("model_id" = "id")) %>%
  left_join(dataset_alias %>%
              select(alias_id, parent_alias_id, alias),
            by = "alias_id") %>%
  arrange(publication_id, parent_alias_id) %>%
  left_join(dataset_reference_table, 
            by = "parent_alias_id") %>%
  filter(!is.na(dataset_id)) %>%
  distinct() %>%
  left_join(publication %>%
              select(id, doi),
            by = c("publication_id" = "id")) %>%
  filter(!is.na(doi))

length(unique(dyad_model_dataset$publication_id)) # N = 9,106

scopus_doi <- dyad_model_dataset %>%
  select(publication_id, doi) %>%
  distinct()
  
length(scopus_doi$publication_id) # N = 9,106

# ------ Generate publication file ------
scopus_pubs <- publication %>% # N = 25,538
  select(-tested_expressions, -last_updated_date, -external_id) %>% # N = 25,538
#  filter(pub_type == "Article") %>% # N = 21,033 
  arrange(id) %>% 
  distinct() %>% 
  left_join(dyad %>% 
              select(publication_id, alias_id), 
            by = c("id" = "publication_id")) %>% 
  distinct() %>% # 
  left_join(dataset_alias %>% 
              select(parent_alias_id, alias_id), 
            by = c("alias_id" = "alias_id")) %>%
  select(-alias_id) %>%
  left_join(dataset_reference_table %>% 
              select(parent_alias_id, dataset_id), 
            by = "parent_alias_id") %>%
  select(doi, pub_type, year, citation_count, journal_id, dataset_id) %>%
  arrange(doi, dataset_id) %>%
  filter(!is.na(dataset_id)) %>% # N = 10,886, Remove datasets that are not part of the 13 USDA datasets
  mutate(dataset_id = ifelse(dataset_id == "dataset13", "dataset07", dataset_id)) %>%
  distinct() %>% # N = 9,825
  arrange(year, doi) 

sum(is.na(scopus_pubs$doi) == TRUE) # N = 367

scopus_pubs <- scopus_pubs %>%
  filter(!is.na(doi)) 

unique(scopus_pubs$dataset_id)  

master_scopus <- scopus_pubs %>%
  mutate(has_dataset = 1) %>%  # Temporary column to indicate presence
  pivot_wider(
    id_cols = c(doi, pub_type, year, citation_count, journal_id),            # Keep one row per publication id
    names_from = dataset_id, # Each dataset_id becomes a column
    values_from = has_dataset, # Values are 1 where a link exists
    values_fill = list(has_dataset = 0) # Fill missing ones with 0
  ) %>%
  select(doi, pub_type, year, citation_count, journal_id, 
         dataset01, dataset02, dataset03, dataset04, dataset05, dataset06, 
         dataset07, dataset08, dataset09, dataset10, dataset11, dataset12) %>%
  arrange(year, doi) %>%
  left_join(issn %>%
              select(journal_id, ISSN),
            by = "journal_id",
            relationship = "many-to-many") %>%
  left_join(journal %>%
              select(id, publisher_id, title, cite_score),
            by = c("journal_id" = "id")) %>%
  select(doi, pub_type, year, citation_count, journal_id, title, cite_score, ISSN, 
         dataset01, dataset02, dataset03, dataset04, dataset05, dataset06, 
         dataset07, dataset08, dataset09, dataset10, dataset11, dataset12)

length(unique(master_scopus$doi)) # N = 9,102 

scopus_author <-  publication %>% 
  select(-tested_expressions, -last_updated_date, -external_id) %>%
#  filter(pub_type == "Article") %>% # N = 21,033 
  arrange(id) %>% 
  distinct() %>% 
  left_join(publication_author %>% 
              select(publication_id, author_id, author_position), 
            by = c("id" = "publication_id"), 
            relationship = "many-to-many") %>%
  left_join(author %>% select(-run_id, -external_id, -last_updated_date), 
            by = c("author_id" = "id")) %>%
  left_join(publication_affiliation %>% 
              select(-run_id, -external_id, -last_updated_date), 
            by = c("id" = "publication_id", "author_position" = "sequence_number"), 
            relationship = "many-to-many") %>%
  arrange(id, author_position) %>%
  left_join(author_affiliation %>% 
              select(-run_id, -last_updated_date), 
            by = c("id.y" = "publication_affiliation_id")) %>%
  rename(publication_affiliation_id = id.y) %>%
  select(doi, pub_type, year, author_id, author_position, given_name, family_name, 
         institution_name, address, city, state, country_code, postal_code, 
         publication_affiliation_id, publication_author_id) %>%
  arrange(year, doi, pub_type, author_position) %>% # N = 151,068
  inner_join(scopus_doi, 
            by = "doi",
            relationship = "many-to-many") %>%
  distinct(doi, author_id, author_position, .keep_all = TRUE)
 
length(unique(scopus_author$doi)) # N = 9,102 ==> This is the same as scopus_pubs
  

keywords_split <- topic %>%
  mutate(keywords = str_trim(keywords)) %>%                # Trim whitespace
  separate_rows(keywords, sep = "\\|") %>%                 # Split into long format
  mutate(keyword = str_trim(keywords)) %>%                 # Clean again if needed
  group_by(id) %>%                                         # Use appropriate ID
  mutate(keyword_num = paste0("keyword", str_pad(row_number(), 2, pad = "0"))) %>%
  ungroup() %>%
  select(id, external_topic_id, keyword_num, keyword) %>%
  pivot_wider(
    names_from = keyword_num,
    values_from = keyword
  )

scopus_topic <- scopus_pubs %>%
  select(doi) %>%
  left_join(publication %>% 
              select(doi, id, pub_type), 
            by = "doi",
            relationship = "many-to-many") %>%
  left_join(publication_asjc %>% 
              select(publication_id, asjc_id), 
            by = c("id" = "publication_id"), 
          relationship = "many-to-many") %>% # N = 19,946
  left_join(asjc %>%
              select(id, label),
            by = c("asjc_id" = "id")) %>% # N = 19,946
  select(doi, id, pub_type, label) %>%  # Drop asjc_id
  group_by(doi, id) %>%       # Group by publication
  mutate(topic_num = row_number()) %>%  # Number each topic within each doi/id
  ungroup() %>%
  pivot_wider(
    names_from = topic_num,         # Each topic becomes a column
    values_from = label,             # Fill with topic label
    names_prefix = "asjc_topic_"     # Column names: asjc_topic_01, asjc_topic_02, ...
  ) %>%
  left_join(topics %>%
              select(eid_topic, publication_id), 
            by = c("id" = "publication_id")) %>%
  left_join(keywords_split %>% 
              select(keyword01, keyword02, keyword03, external_topic_id),
            by = c("eid_topic" = "external_topic_id"))
  
length(unique(scopus_topic$doi)) # N = 9,102 ==> This is the same as master_scopus_wide





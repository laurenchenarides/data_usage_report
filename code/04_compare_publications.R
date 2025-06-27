# ======================================================================
# Title: Source Overlap Matching and Dataset Publication Summary
# Author: Lauren Chenarides
# Last Updated: May 19, 2025
#
# Description:
# This script prepares publication-level comparisons across four citation 
# sources—Scopus, OpenAlex (Seed and Full Text), and Dimensions—for twelve 
# USDA-related datasets. Dataset13 ("Transition of Agricultural Land Survey") 
# is merged into Dataset07 ("TOTAL Survey") due to conceptual and source overlap.
#
# Notes:
#   - Only publications with pub_type = "article" are included.
#   - Year range is limited to publications from 2017–2023.
#   - Each dataset must be mentioned (datasetXX == 1) to be retained.
#
# Main Tasks:
#   - Clean and filter source-specific publication data
#   - Match publications by DOI and ISSN across the four sources
#   - Create source indicator flags (e.g., scopus_yes, oa_seed_yes)
#   - Collapse overlapping dataset fields and publication years
#   - Generate cleaned publication-level and journal-level tables
#   - Store final data in the global environment:
#       - `{datasetXX}_df`: publication-level merged data
#       - `{datasetXX}_pubs`: publication-level summary with source flags
#       - `{datasetXX}_journals`: journal-level summary by source
#
# Inputs:
#   - master_scopus: Long-format Scopus data with one row per DOI × ISSN
#   - master_openalex: OpenAlex full-text and seed corpus deduplicated search results
#   - master_dimensions: Dimensions data extracted by dataset mention
#   - dataset_titles: Named vector of human-readable dataset names
#
# Output:
#   - Merged dataset-level objects in memory (not saved to file)
#   - Used downstream in visualizations (Sankey, dot plots, topic bubbles)
#   - Treemaps have been moved to a separate script
#
# Required Libraries:
#   - dplyr, tidyr, stringr, readr, tools, purrr, Cairo, ggplot2
# ======================================================================

# ===============================================================
# Input Object Descriptions:
#
# master_scopus:
#   A long-format Scopus dataset with one row per (DOI × ISSN)
#   combination. Dataset mentions are represented in wide format, 
#   where each dataset ID appears as a binary indicator column 
#   (e.g., dataset_01 = 1 if mentioned). DOIs may repeat if the 
#   same article is associated with multiple ISSNs. 
#
# master_openalex:
#   A long-format OpenAlex dataset constructed from the full-text 
#   search corpus. Each row represents a unique (DOI × ISSN) 
#   combination, of which there could be multiple though some 
#   records may have missing ISSNs, which could indicate pre-prints 
#   or unindexed materials. Dataset mentions are encoded as binary 
#   indicator columns (e.g., dataset_01, dataset_02), with IDs zero-padded 
#   for consistency. Additional columns note open access status and 
#   whether the publication is indexed in Scopus.
# 
# dim_publications:
#   A long-format Dimensions dataset constructed by combining multiple 
#   Excel exports from Dimensions. Each row represents a unique (DOI × ISSN) 
#   combination. Dataset mentions are inferred from filename associations 
#   and tagged via `dataset_id`. ISSNs are split to long format if multiple 
#   values exist. Includes basic metadata such as title, journal, citation counts, 
#   and publication year.
# ===============================================================

scopus_cols <- colnames(master_scopus)
oa_cols <- colnames(master_openalex)
dim_cols <- colnames(dim_publications)

print(scopus_cols)
print(oa_cols)
print(dim_cols)

# =============  Match ALL ============= #

# Prep source flags
df_scopus <- master_scopus %>%
  filter(pub_type == "Article") %>%  # Only selecting articles
  filter(year > 2016 & year < 2024) %>%  # Published between 2017-2023
  mutate(scopus_yes = 1) %>%
  select(doi, 
         year, 
         journal_id, 
         title, 
         ISSN, 
         cite_score, 
         citation_count, 
         scopus_yes, 
         everything()) %>%
  rename(journal_title = title,
         publication_year = year) %>%
  arrange(publication_year, doi) %>% # N = 13,111
  clean_names() %>%
  distinct() # N = 11,680

length(df_scopus$doi)

df_openalex <- master_openalex %>%
  filter(pub_type == "article") %>%   # Only selecting articles
  filter(year > 2016 & year < 2024) %>%  # Published between 2017-2023
  mutate(oa_yes = 1) %>%
  rename(publication_year = year) %>%
  select(doi, 
         publication_year,
         journal_title, 
         issn,
         oa_yes, 
         everything()) %>%
  clean_names() %>%
  distinct() %>% # N = 3,709
  arrange(doi)

length(df_openalex$doi)

df_dimensions <- master_dimensions %>%
  filter(pub_type == "article") %>%   # Only selecting articles
  filter(year > 2016 & year < 2024) %>%  # Published between 2017-2023
  mutate(dim_yes = 1) %>%
  rename(dim_id = id,
         publication_year = year,
         article_title = title) %>%
  select(doi, 
         publication_year,
         article_title,
         journal_title, 
         issn,
         dim_yes, 
         everything()) %>%
  arrange(publication_year, doi) %>%
  clean_names() %>%
  distinct() # N = 13,181

length(df_dimensions$doi)

# Get list of dataset variables
dataset_vars <- sprintf("dataset%02d", 1:12)

# Loop through each dataset and create a clean data frame
for (ds_var in dataset_vars) {
  
  print(paste0("Processing dataset: ", ds_var))
  
  # Columns to exclude from renaming
  oa_exclude <- c("doi", "issn", "is_oa", "oa_yes", ds_var)
  scopus_exclude <- c("doi", "issn", "scopus_yes", ds_var)
  dim_exclude <- c("doi", "issn", "dim_yes", ds_var)
  
  # Filter to rows where the dataset is mentioned in each source (openalex, scopus, dimensions)
  oa_filt <- df_openalex %>%
    filter(!!sym(ds_var) == 1) %>%
    select(doi, 
           oa_yes, 
           all_of(ds_var), 
           everything()) %>%
    
    # Drop non-focal dataset columns
    select(-any_of(setdiff(grep("^dataset\\d{2}$", names(.), value = TRUE), ds_var)))  %>%
    rename_with(~ paste0(., "_oa"), .cols = setdiff(names(.), oa_exclude))
  
  scopus_filt <- df_scopus %>%
    filter(!!sym(ds_var) == 1) %>%
    select(doi, 
           scopus_yes, 
           all_of(ds_var), 
           everything()) %>%
    
    # Drop non-focal dataset columns
    select(-any_of(setdiff(grep("^dataset\\d{2}$", names(.), value = TRUE), ds_var))) %>%
    rename_with(~ paste0(., "_s"), .cols = setdiff(names(.), scopus_exclude))
  
  dim_filt <- df_dimensions %>%
    filter(!!sym(ds_var) == 1) %>%
    select(doi, 
           dim_yes, 
           all_of(ds_var), 
           everything()) %>%
    select(-any_of(setdiff(grep("^dataset\\d{2}$", names(.), value = TRUE), ds_var))) %>%
    rename_with(~ paste0(., "_dim"), .cols = setdiff(names(.), dim_exclude))

  # Join in specified order
  joined_df <- oa_filt %>%
    full_join(scopus_filt, 
              by = c("doi", "issn"), 
              relationship = "many-to-many") %>%
    full_join(dim_filt, 
              by = c("doi", "issn"), 
              relationship = "many-to-many") %>%
    mutate(across(c(scopus_yes, oa_yes, dim_yes), ~replace_na(.x, 0))) %>%
    distinct() %>%
    select(doi, oa_yes, scopus_yes, dim_yes, 
           starts_with("dataset"),
           starts_with("publication_year"),
           starts_with("article_title"),
           starts_with("issn"),
           starts_with("journal"),
           starts_with("citation_count"),
           starts_with("cite_score"),
           everything()) 

  # Identify dataset columns to collapse: e.g., "dataset01", "dataset01.x", "dataset01.y"
  dataset_cols <- grep(paste0("^", ds_var, "($|\\.)"), names(joined_df), value = TRUE)
  
  # Collapse only if there are multiple dataset variants
  if (length(dataset_cols) > 1) {
    joined_df <- joined_df %>%
      rowwise() %>%
      mutate(
        !!ds_var := if (all(is.na(c_across(all_of(dataset_cols)))))
          NA_real_
        else
          max(c_across(all_of(dataset_cols)), na.rm = TRUE)
      ) %>%
      ungroup() %>%
      select(-setdiff(dataset_cols, ds_var))  # <-- Only drop extras, keep main column
  }

  # ---- Collapse Publication Year Columns ----
  
  year_cols <- grep("^publication_year($|_oa|_s|_dim)$", names(joined_df), value = TRUE)
  
  if (length(year_cols) > 1) {
    joined_df <- joined_df %>%
      rowwise() %>%
      mutate(
        publication_year = if (all(is.na(c_across(all_of(year_cols)))))
          NA_real_
        else
          max(c_across(all_of(year_cols)), na.rm = TRUE)
      ) %>%
      ungroup() %>%
      select(-all_of(setdiff(year_cols, "publication_year")))  # drop only auxiliary year cols
  }
  
  # Create collapsed version at the publication (doi) level
  pubs <- joined_df %>%
    group_by(doi) %>%
    summarize(
      oa_yes      = max(oa_yes, na.rm = TRUE),
      scopus_yes  = max(scopus_yes, na.rm = TRUE),
      dim_yes     = max(dim_yes, na.rm = TRUE),
      !!ds_var    := max(.data[[ds_var]], na.rm = TRUE),
      publication_year = max(publication_year, na.rm = TRUE),
      .groups = "drop"
    )

  journals <- joined_df %>%
    filter(.data[[ds_var]] == 1) %>%
    mutate(
      journal_title = coalesce(journal_title_s, journal_title_oa, journal_title_dim)
    ) %>%
    group_by(issn, journal_title) %>%
    summarize(
      doi_count_oa   = n_distinct(doi[oa_yes == 1], na.rm = TRUE),
      doi_count_s    = n_distinct(doi[scopus_yes == 1], na.rm = TRUE),
      doi_count_dim  = n_distinct(doi[dim_yes == 1], na.rm = TRUE),
      oa_yes         = as.integer(any(oa_yes == 1)),
      scopus_yes     = as.integer(any(scopus_yes == 1)),
      dim_yes        = as.integer(any(dim_yes == 1)),
      !!ds_var       := 1,
      .groups = "drop"
    ) %>%
    mutate(journal_title = journal_title %>%
             str_replace_all("[^\\x20-\\x7E]", "") %>%
             str_remove_all(",") %>%
             str_squish()) %>%
    arrange(journal_title) %>%
    filter(!is.na(issn)) %>%
    group_by(journal_title) %>%
    summarize(
      issn_count     = n_distinct(issn, na.rm = TRUE),
      doi_count_oa   = sum(doi_count_oa),
      doi_count_s    = sum(doi_count_s),
      doi_count_dim  = sum(doi_count_dim),
      oa_yes         = as.integer(any(oa_yes == 1)),
      scopus_yes     = as.integer(any(scopus_yes == 1)),
      dim_yes        = as.integer(any(dim_yes == 1)),
      !!ds_var       := 1,
      .groups = "drop"
    ) %>%
    arrange(journal_title)
  
  # Assign to global environment with cleaned name
  assign(paste0(ds_var, "_df"), joined_df, envir = .GlobalEnv)
  assign(paste0(ds_var, "_pubs"), pubs, envir = .GlobalEnv)
  assign(paste0(ds_var, "_journals"), journals, envir = .GlobalEnv)
  
}

rm(dim_filt, oa_filt, scopus_filt, joined_df, pubs, journals)

# ---------------------------------
# Resulting dataframes by dataset:
# - dataset{num}_df
# - dataset{num}_pubs
# - dataset{num}_journals
# 
# Go to: 
#   (1) construct_treemaps.R for publication coverage graphic
#   (2) construct_sankey_plots.R for journal comparison - across sources
#   (3) construct_bubble_charts.R for topic comparison
#   (4) construct_word_clouds.R for sub-topics within food security-tagged literature
# ---------------------------------




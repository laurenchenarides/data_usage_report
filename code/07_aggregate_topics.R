################################################################################
# Title: Aggregate Topic Labels for DOIs across Citation Sources
# Author: Lauren Chenarides
# Last Updated: June 9, 2025
#
# Description:
# This script consolidates topic metadata from all four citation
# sources into a unified structure to support topic-level analysis.
# Each dataset is reshaped and merged to create a master topic
# dataframe by DOI. The final output can be used for word clouds,
# bubble charts, or cross-source topic coverage comparisons.
#
# Output:
# - combined_topics_by_doi (long-format)
# - optional export to CSV or RDS if desired
################################################################################

# ------------ LOOP -------------- #
for (i in 1:12) {
  ds_id <- sprintf("dataset%02d", i)
  message("Processing: ", ds_id)
  
  # Extract dataset title
  dataset_title <- dataset_reference_table %>%
    filter(dataset_id == ds_id) %>%
    pull(dataset_title) %>%
    tools::file_path_sans_ext() %>%
    str_trim()
  
  print(dataset_title)
  
  # Extract DOIs for OA
  pub_df <- get(paste0(ds_id, "_pubs"))   
  # Pulls in dataset{num}_pubs which are filtered as type = "article" published between 2017-2023
  
  oa_dois <- pub_df %>% filter(oa_yes == 1) %>% pull(doi)
  
  # Fetch OpenAlex metadata
  works_meta <- oa_fetch(entity = "works", doi = oa_dois, verbose = FALSE)
  
  # Parse OpenAlex Topics
  oa_topics_df <- works_meta %>%
    select(doi, topics) %>%
    unnest(topics) %>%
    filter(type %in% c("topic", "subfield", "field", "domain")) %>%
    arrange(doi, type, i) %>%
    pivot_wider(
      names_from = c(type, i),
      values_from = display_name,
      values_fn = list
    ) %>%
    mutate(across(
      c(starts_with("topic"), starts_with("subfield"),
        starts_with("field"), starts_with("domain")),
      ~ map_chr(., ~ if (length(.) > 0) .[1] else NA_character_)
    )) %>%
    select(doi,
           starts_with("topic"),
           starts_with("subfield"),
           starts_with("field"),
           starts_with("domain")) %>%
    distinct() %>%
    arrange(doi) %>%
    group_by(doi) %>%
    summarize(
      topic_1 = first(na.omit(topic_1)),
      topic_2 = first(na.omit(topic_2)),
      topic_3 = first(na.omit(topic_3)),
      subfield_1 = first(na.omit(subfield_1)),
      subfield_2 = first(na.omit(subfield_2)),
      subfield_3 = first(na.omit(subfield_3)),
      field_1 = first(na.omit(field_1)),
      field_2 = first(na.omit(field_2)),
      field_3 = first(na.omit(field_3)),
      domain_1 = first(na.omit(domain_1)),
      domain_2 = first(na.omit(domain_2)),
      domain_3 = first(na.omit(domain_3)),
      .groups = "drop"
    ) %>%
    mutate(doi = str_remove(doi, "^https://doi.org/"))

  # Get Scopus topics
  scopus_topics_df <- pub_df %>%
    filter(scopus_yes == 1) %>%
    select(doi) %>%
    inner_join(scopus_topic %>%
                 select(doi, 
                        asjc_topic_1, 
                        asjc_topic_2, 
                        asjc_topic_3, 
                        asjc_topic_4, 
                        keyword01, 
                        keyword02, 
                        keyword03),
               by = "doi") %>%
    pivot_longer(
      cols = starts_with("asjc_topic_"),
      names_to = "i",
      names_prefix = "asjc_topic_",
      values_to = "asjc_topic"
    ) %>%
    filter(!is.na(asjc_topic)) %>%
    distinct(doi, asjc_topic, .keep_all = TRUE) %>%
    group_by(doi) %>%
    mutate(i = row_number()) %>%
    ungroup() %>%
    pivot_wider(
      names_from = i, 
      values_from = asjc_topic, 
      names_prefix = "asjc_topic_") %>%
    distinct()
  
  # Get Dimensions concepts
  dim_topics_df <- pub_df %>%
    filter(dim_yes == 1) %>%
    select(doi) %>%
    inner_join(dim_concepts %>%
                 select(doi, concept),
               by = "doi") %>%
    group_by(doi) %>%
    mutate(i = row_number()) %>%
    ungroup() %>%
    pivot_wider(
      names_from = i,
      values_from = concept,
      names_prefix = "concept_"
    )
  
  # Join all topics
  topics_final <- pub_df %>%
    left_join(scopus_topics_df %>%
                rename(
                  s_topic_1 = asjc_topic_1,
                  s_topic_2 = asjc_topic_2,
                  s_topic_3 = asjc_topic_3,
                  s_topic_4 = asjc_topic_4,
                  
                  s_keyword01 = keyword01,
                  s_keyword02 = keyword02,
                  s_keyword03 = keyword03), 
              by = "doi") %>%
    left_join(dim_topics_df %>%
                rename(
                  dim_topic_1 = concept_1,
                  dim_topic_2 = concept_2,
                  dim_topic_3 = concept_3,
                  dim_topic_4 = concept_4,
                  dim_topic_5 = concept_5), 
              by = "doi") %>%
    left_join(oa_topics_df %>%
                rename(
                  oa_topic_1 = topic_1,
                  oa_topic_2 = topic_2,
                  oa_topic_3 = topic_3,
                  oa_subfield_1 = subfield_1,
                  oa_subfield_2 = subfield_2,
                  oa_subfield_3 = subfield_3,
                  oa_field_1 = field_1,
                  oa_field_2 = field_2,
                  oa_field_3 = field_3,
                  oa_domain_1 = domain_1,
                  oa_domain_2 = domain_2,
                  oa_domain_3 = domain_3), 
              by = "doi") %>%
    select(doi,publication_year, oa_yes, scopus_yes, dim_yes, 
           starts_with("dataset"),
           starts_with("oa_topic"),
           starts_with("oa_subfield"),
           starts_with("oa_field"),
           starts_with("oa_domain"),
           starts_with("s_topic"),
           starts_with("s_keyword"),
           starts_with("dim_topic"),
           everything()
    ) %>%
    mutate(across(
      matches("oa_topic_|oa_subfield_|oa_field_|oa_domain_|s_topic_|s_keyword|dim_topic_"),
      ~ ifelse(str_trim(str_to_lower(.x)) == !!str_trim(str_to_lower(dataset_title)), NA, .x)
    )) %>%
    distinct()
  
  # Define ordered topic columns from broadest to most granular
  topic_cols_ordered <- c(
    "oa_topic_1", "oa_topic_2", "oa_topic_3", 
    "s_topic_1", "s_topic_2", "s_topic_3", "s_topic_4",
    "s_keyword01", "s_keyword02", "s_keyword03",
    "dim_topic_1", "dim_topic_2", "dim_topic_3", "dim_topic_4", "dim_topic_5"
  )
  
  # Safely handle missing columns (e.g., if scopus or dim missing)
  topic_cols_present <- topic_cols_ordered[topic_cols_ordered %in% names(topics_final)]
  
  # Create combined_topics and topic_text
  topics_final <- topics_final %>%
    rowwise() %>%
    mutate(
      combined_topics = paste(unique(na.omit(c_across(all_of(topic_cols_present)))), collapse = "; "),
      topic_text = gsub(";\\s*", " ", combined_topics),
      topic_food_security = str_detect(topic_text, regex("\\bfood (in)?security\\b", ignore_case = TRUE))
    ) %>%
    ungroup()
  
  # Save to dynamically named object
  assign(paste0(ds_id, "_topics"), topics_final)
}

# ---- Food security flag terms ---- 
all_topic_dfs <- mget(paste0("dataset", sprintf("%02d", 1:12), "_topics"))
common_cols <- Reduce(intersect, lapply(all_topic_dfs, names))

food_security_flag_terms <- bind_rows(
  lapply(all_topic_dfs, function(df) df[, common_cols])
) %>%
  # filter(topic_food_security == TRUE) %>%
  distinct() %>%
  select(doi, publication_year, combined_topics, topic_food_security, oa_yes, scopus_yes, dim_yes)

# Write to CSV
# write_csv(food_security_flag_terms, "food_security_flag_terms.csv") # N = 1,518

# Write to Excel
# write_xlsx(all_topic_dfs, path = "all_datasets_topics.xlsx")
# ----------------- END ----------------- 

# Combine all tibbles into one with a dataset ID column
master_topics_df <- purrr::imap_dfr(
  all_topic_dfs,
  ~ mutate(.x, dataset_id = .y)
) 

topic_cols_ordered <- c(
  "oa_topic_1", "oa_topic_2", "oa_topic_3", 
  "s_topic_1", "s_topic_2", "s_topic_3", "s_topic_4",
  "s_keyword01", "s_keyword02", "s_keyword03",
  "dim_topic_1", "dim_topic_2", "dim_topic_3", "dim_topic_4", "dim_topic_5"
)

asjc_table <- master_topics_df %>%
  pivot_longer(
    cols = c(s_topic_1, s_topic_2, s_topic_3, s_topic_4),
    names_to = "asjc_level",
    values_to = "asjc_topic",
    values_drop_na = TRUE
  ) %>%
  mutate(ones = 1) %>%
  group_by(asjc_topic) %>%
  summarize(
    doi_count         = sum(ones),
    oa_topic_1     = first(oa_topic_1),
    oa_topic_2     = first(oa_topic_2),
    oa_topic_3     = first(oa_topic_3),
    oa_subfield_1  = first(oa_subfield_1),
    oa_subfield_2  = first(oa_subfield_2),
    oa_subfield_3  = first(oa_subfield_3),
    oa_field_1     = first(oa_field_1),
    oa_field_2     = first(oa_field_2),
    oa_field_3     = first(oa_field_3),
    oa_domain_1    = first(oa_domain_1),
    oa_domain_2    = first(oa_domain_2),
    oa_domain_3    = first(oa_domain_3),
    s_keyword01       = first(s_keyword01),
    s_keyword02       = first(s_keyword02),
    s_keyword03       = first(s_keyword03),
    dim_topic_1       = first(dim_topic_1),
    dim_topic_2       = first(dim_topic_2),
    dim_topic_3       = first(dim_topic_3),
    dim_topic_4       = first(dim_topic_4),
    dim_topic_5       = first(dim_topic_5),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    combined_topic_text = paste(c_across(oa_topic_1:dim_topic_5), collapse = " "),
    topic_food_security = str_detect(str_to_lower(combined_topic_text), "food (in)?security")
  ) %>%
  ungroup() %>%
  select(-combined_topic_text)

# write_xlsx(asjc_table, path = "asjc_table.xlsx")

# ---------- Topic Summary Tables: Binary Indicators ----------

# Function to extract DOIs per topic per source
extract_topic_dois <- function(df, prefix, source_flag) {
  topic_cols <- grep(paste0("^", prefix, "_topic_"), names(df), value = TRUE)
  
  if (length(topic_cols) == 0) {
    message(paste("No topic columns found for prefix:", prefix))
    return(tibble(topic = character(), doi = character()))
  }
  
  df %>%
    filter(.data[[source_flag]] == 1) %>%
    select(doi, all_of(topic_cols)) %>%
    pivot_longer(
      cols = all_of(topic_cols), 
      names_to = "col", 
      values_to = "topic") %>%
    filter(!is.na(topic)) %>%
    mutate(topic = str_to_title(trimws(topic))) %>%
    distinct(topic, doi)
}

dataset_titles <- c(
  "dataset01_pubs" = "ARMS",
  "dataset02_pubs" = "Current Population Survey Food Security Supplement",
  "dataset03_pubs" = "Farm to School Census",
  "dataset04_pubs" = "IRI InfoScan",
  "dataset05_pubs" = "Census of Agriculture",
  "dataset06_pubs" = "Rural-Urban Continuum Code",
  "dataset07_pubs" = "The Tenure, Ownership, and Transition of Agricultural Land Survey",
  "dataset08_pubs" = "Food Access Research Atlas",
  "dataset09_pubs" = "Food Acquisition and Purchase Survey",
  "dataset10_pubs" = "Household Food Security Survey Module",
  "dataset11_pubs" = "Local Food Marketing Practices Survey",
  "dataset12_pubs" = "Quarterly Food at Home Price Database"
)

# Loop through each dataset
for (i in 1:12) {
  ds_id <- sprintf("dataset%02d", i)
  df <- get(paste0(ds_id, "_topics"))
  label <- dataset_titles[paste0(ds_id, "_pubs")]
  
  # Extract (topic, doi) per source
  scopus_df    <- extract_topic_dois(df, "s", "scopus_yes")
  oa_df        <- extract_topic_dois(df, "oa", "oa_yes")
  dim_df       <- extract_topic_dois(df, "dim", "dim_yes")
  
  # Count of DOIs per topic per source
  count_scopus   <- scopus_df    %>% count(topic, name = "count_scopus")
  count_oa       <- oa_df        %>% count(topic, name = "count_oa")
  count_dim      <- dim_df       %>% count(topic, name = "count_dim")
  
  # Merge all topic counts
  all_topics <- full_join(count_scopus, count_oa, by = "topic") %>%
    full_join(count_dim, by = "topic") %>%
    replace_na(list(
      count_scopus = 0,
      count_oa = 0,
      count_dim = 0
    ))
  
  # Shared and exclusive DOI counts
  all_topics <- all_topics %>%
    rowwise() %>%
    mutate(
      total_sources = sum(c_across(c(count_scopus, count_oa, count_dim)) > 0),
      shared = if (total_sources > 1) {
        count_scopus + count_oa + count_dim
      } else {
        0
      },
      exclusive_scopus   = if (count_scopus > 0 & total_sources == 1) count_scopus else 0,
      exclusive_oa       = if (count_oa > 0 & total_sources == 1) count_oa else 0,
      exclusive_dim      = if (count_dim > 0 & total_sources == 1) count_dim else 0
    ) %>%
    ungroup() %>%
    select(topic, shared, exclusive_scopus, exclusive_oa, exclusive_dim,
           count_scopus, count_oa, count_dim) %>%
    arrange(desc(shared), desc(count_scopus + count_oa + count_dim))
  
  # Create safe filename from title
  safe_title <- gsub("[^A-Za-z0-9]+", "_", label) %>%
    tolower() %>%
    trimws()
  
  write_csv(all_topics, file.path(output_dir_tables, paste0(safe_title, "_topics_long_table.csv")))
}

# ------ Topic Summary Tables (Distinct DOI Counts): Disaggregated as in Treemaps -------

# Function to extract DOIs per topic per source
extract_topic_dois <- function(df, prefix, source_flag) {
  topic_cols <- grep(paste0("^", prefix, "_topic_"), names(df), value = TRUE)
  
  if (length(topic_cols) == 0) {
    message(paste("No topic columns found for prefix:", prefix))
    return(tibble(topic = character(), doi = character()))
  }
  
  df %>%
    filter(.data[[source_flag]] == 1) %>%
    select(doi, all_of(topic_cols)) %>%
    pivot_longer(cols = all_of(topic_cols), names_to = "col", values_to = "topic") %>%
    filter(!is.na(topic)) %>%
    mutate(topic = str_to_title(trimws(topic))) %>%
    distinct(topic, doi)
}

# Loop through each dataset
for (i in 1:12) {
  ds_id <- sprintf("dataset%02d", i)
  df <- get(paste0(ds_id, "_topics"))
  label <- dataset_titles[paste0(ds_id, "_topics")]
  
  # Extract topic-doi pairs for each source
  scopus_df   <- extract_topic_dois(df, "s", "scopus_yes")
  oa_df       <- extract_topic_dois(df, "oa", "oa_yes")
  dim_df      <- extract_topic_dois(df, "dim", "dim_yes")
  
  # Add source flags
  scopus_df$scopus_yes <- 1; scopus_df$oa_yes <- 0; scopus_df$dim_yes <- 0
  oa_df$scopus_yes <- 0; oa_df$oa_yes <- 1; oa_df$dim_yes <- 0
  dim_df$scopus_yes <- 0; dim_df$oa_yes <- 0; dim_df$dim_yes <- 1
  
  # Combine all into long form
  combined <- bind_rows(scopus_df, oa_df, dim_df) %>%
    group_by(topic, doi) %>%
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
    ))
  
  # Define all 7 group levels
  group_levels <- c(
    "Scopus only",
    "OpenAlex only",
    "Dimensions only",
    "Scopus ∩ OpenAlex",
    "Scopus ∩ Dimensions",
    "OpenAlex ∩ Dimensions",
    "Scopus ∩ OpenAlex ∩ Dimensions"
  )
  
  # Count DOIs by topic and group
  count_by_group <- combined %>%
    count(topic, group, name = "count") %>%
    mutate(group = factor(group, levels = group_levels)) %>%
    complete(topic, group = group_levels, fill = list(count = 0)) %>%  # <-- ensures all combinations exist
    pivot_wider(
      names_from = group,
      values_from = count,
      values_fill = 0
    )
  
  # Also get total counts per source (regardless of group)
  total_counts <- combined %>%
    group_by(topic) %>%
    summarize(
      count_scopus   = sum(scopus_yes),
      count_oa       = sum(oa_yes),
      count_dim      = sum(dim_yes),
      .groups = "drop"
    )
  
  # True count of unique DOIs per topic
  true_total <- combined %>%
    count(topic, name = "total_dois")
  
  # Merge everything
  all_topics <- count_by_group %>%
    left_join(true_total, by = "topic") %>%
    arrange(desc(total_dois))
  
  # Write to file
  safe_title <- gsub("[^A-Za-z0-9]+", "_", label) %>%
    tolower() %>%
    trimws()
  
  write_csv(all_topics, file.path(output_dir_tables, paste0(safe_title, "_topics_long_table.csv")))
}

# ------ Topic Summary Tables (Distinct DOI Counts): Only DOIs that appear in all three databases -------

for (i in 1:12) {
  ds_id <- sprintf("dataset%02d", i)
  df <- get(paste0(ds_id, "_topics"))
  label <- dataset_titles[paste0(ds_id, "_pubs")]
  
  # Step 1: Filter to DOIs found in all three sources
  df_common <- df %>%
    filter(oa_yes == 1, scopus_yes == 1, dim_yes == 1)
  
  print(paste0("Number of overlapping DOIS for ", label, ": ", nrow(df_common)))
  
  # Step 2: Extract topic counts for each source
  
  # OpenAlex topics
  oa_topics <- extract_topic_dois(df_common, "oa", "oa_yes") %>%
    count(topic, name = "n_doi") %>%
    arrange(desc(n_doi))
  
  # Scopus topics
  scopus_topics <- extract_topic_dois(df_common, "s", "scopus_yes") %>%
    count(topic, name = "n_doi") %>%
    arrange(desc(n_doi))
  
  # Dimensions topics
  dim_topics <- extract_topic_dois(df_common, "dim", "dim_yes") %>%
    count(topic, name = "n_doi") %>%
    arrange(desc(n_doi))
  
  # Step 3: Create Excel workbook
  wb <- createWorkbook()
  addWorksheet(wb, "OpenAlex Topics")
  addWorksheet(wb, "Scopus Topics")
  addWorksheet(wb, "Dimensions Topics")
  
  writeData(wb, sheet = "OpenAlex Topics", oa_topics)
  writeData(wb, sheet = "Scopus Topics", scopus_topics)
  writeData(wb, sheet = "Dimensions Topics", dim_topics)
  
  # Step 4: Save workbook
  safe_title <- gsub("[^A-Za-z0-9]+", "_", label) %>%
    tolower() %>%
    trimws()
  
  saveWorkbook(wb, file = file.path(output_dir_tables, paste0(safe_title, "_topics_intersect.xlsx")), overwrite = TRUE)
}

rm(df, df_common)

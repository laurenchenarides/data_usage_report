################################################################################
# Title: Bubble Charts of Top Topics by Dataset and Citation Source
# Author: Lauren Chenarides
# Last Updated: May 23, 2025
#
# Description:
# This script generates bubble charts visualizing the most frequent research topics 
# associated with each dataset, based on publications indexed in Scopus and/or 
# OpenAlex (via full text or seed corpus). Topics are categorized as either 
# exclusive to one source or shared across sources.
#
# Inputs:
# - Publication-level data for datasets 01 to 12 (e.g., `dataset01_pubs`)
# - Scopus ASJC topic metadata (e.g., `scopus_topic`)
# - OpenAlex metadata fetched via `openalexR` API; these are "topic", "subfield", "field", and "domain"
# - Dimensions topics are referred to as "concepts"
#
# Outputs:
# - Circle packing plots of top 20 topics per source (Scopus, OA Full Text, OA Seed, Dimensions)
# - PNG files saved to `graphics/bubble_charts/` folder
#
# Key Dependencies:
# - dplyr, tidyr, ggplot2, openalexR
# - packcircles (for layout), ggrepel, patchwork
# - Cairo (for high-res PNG export)
#
# Notes:
# - The base set of DOIs used for each source is not mutually exclusive.
# - Bubble chart for topics: https://www.displayr.com/what-is-a-bubble-chart/
################################################################################

# ====================================================================== #
# ------- Topic Bubble Charts by Source --------- #
# ====================================================================== #

# Title mapping
dataset_titles <- c(
  "dataset01" = "ARMS",
  "dataset02" = "Current Population Survey (CPS) Food Security Supplement (FSS)",
  "dataset03" = "Farm to School Census",
  "dataset04" = "IRI InfoScan",
  "dataset05" = "Census of Agriculture",
  "dataset06" = "Rural-Urban Continuum Code",
  "dataset07" = "The Tenure, Ownership, and Transition of Agricultural Land (TOTAL) Survey",
  "dataset08" = "Food Access Research Atlas",
  "dataset09" = "Food Acquisition and Purchase Survey (FoodAPS)",
  "dataset10" = "Household Food Security Survey Module",
  "dataset11" = "Local Food Marketing Practices Survey",
  "dataset12" = "Quarterly Food at Home Price Database"
)

# -------------- LOOP OVER DATASETS ------------- #

# Create word clouds for each dataset
for (i in 1:12) {
  ds_id <- sprintf("dataset%02d", i)
  dataset_title <- dataset_titles[[ds_id]]
  message("Creating word cloud for: ", dataset_title)
  
  df <- get(paste0(ds_id, "_topics"))
  
  topic_cols <- c("oa_topic_1", "oa_topic_2", "oa_topic_3", 
                  "s_topic_1", "s_topic_2", "s_topic_3", "s_topic_4",
                  "dim_topic_1", "dim_topic_2", "dim_topic_3", "dim_topic_4", "dim_topic_5",
                  "s_keyword01", "s_keyword02", "s_keyword03")
  
  topic_cols_present <- topic_cols[topic_cols %in% names(df)]
  
  topic_counts <- df %>%
    select(all_of(topic_cols_present)) %>%
    pivot_longer(everything(), values_to = "topic") %>%
    filter(!is.na(topic)) %>%
    filter(!grepl("\\(", topic)) %>%
    mutate(
      topic = topic %>%
        trimws() %>%
        tolower() %>%
        str_to_title()
    ) %>%
    count(topic, sort = TRUE) %>%
    slice_max(n, n = 100) %>%
    mutate(scaled_n = scales::rescale(n, to = c(5, 35)))
  
  # Generate word cloud with colorblind-friendly palette
  p <- ggplot(topic_counts, aes(label = topic, size = scaled_n, color = n)) +
    geom_text_wordcloud(area_corr = TRUE, rm_outside = TRUE, show.legend = FALSE) +
    scale_color_viridis_c(option = "turbo") +  # Options: "plasma", "viridis", "magma", "cividis", etc.
    scale_size_area(max_size = 35) +
  theme_void(base_size = 14) +  # replaces theme_minimal to remove axis padding
    theme(
      plot.margin = margin(10, 10, 20, 10),
      plot.title = element_text(hjust = 0, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0, size = 14, margin = margin(b = 10)),
      plot.caption = element_text(size = 10, margin = margin(t = 10))
    ) +
  labs(
    title = dataset_title,
    subtitle = "Most Frequent Research Topics",
    caption = "Word cloud shows the top 100 research topics associated with this dataset across Scopus, OpenAlex, and Dimensions."
  )
  
  # Save topic counts to CSV (aggregate across all sources)
  # write.csv(topic_counts, file = file.path(output_dir, paste0(ds_id, "_all.csv")), row.names = FALSE)
  
  CairoPNG(
    filename = file.path(output_dir, paste0(ds_id, "_wordcloud.png")),
    width = 10,
    height = 8,
    units = "in",
    res = 300
  )
  print(p)
  dev.off()
}

# ---------- By Source ---------- #

sources <- list(
  "Scopus" = quote(scopus_yes == 1),
  "OpenAlex" = quote(oa_yes == 1),
  "Dimensions" = quote(dim_yes == 1)
)

topic_cols <- c(
  "oa_topic_1", "oa_topic_2", "oa_topic_3", 
  "s_topic_1", "s_topic_2", "s_topic_3", "s_topic_4",
  "dim_topic_1", "dim_topic_2", "dim_topic_3", "dim_topic_4", "dim_topic_5",
  "s_keyword01", "s_keyword02", "s_keyword03"
)

for (i in 1:12) {
  ds_id <- sprintf("dataset%02d", i)
  dataset_title <- dataset_titles[[ds_id]]
  full_df <- get(paste0(ds_id, "_topics"))
  
  message("Creating word clouds for: ", dataset_title)
  
  for (source_name in names(sources)) {
    message("  Source: ", source_name)
    
    df <- full_df %>% filter(!!sources[[source_name]])
    topic_cols_present <- topic_cols[topic_cols %in% names(df)]
    if (length(topic_cols_present) == 0) next
    
    topic_counts <- df %>%
      select(all_of(topic_cols_present)) %>%
      pivot_longer(everything(), values_to = "topic") %>%
      filter(!is.na(topic)) %>%
      filter(!grepl("\\(", topic)) %>%
      mutate(
        topic = topic %>%
          trimws() %>%
          tolower() %>%
          str_to_title()
      ) %>%
      count(topic, sort = TRUE) %>%
      slice_max(n, n = 100) %>%
      mutate(scaled_n = scales::rescale(n, to = c(5, 35)))
    
    if (nrow(topic_counts) == 0) next
    
    p <- ggplot(topic_counts, aes(label = topic, size = scaled_n, color = n)) +
      geom_text_wordcloud(area_corr = TRUE, rm_outside = TRUE, show.legend = FALSE) +
      scale_color_viridis_c(option = "turbo") +
      scale_size_area(max_size = 35) +
      theme_void(base_size = 14) +
      theme(
        plot.margin = margin(10, 10, 20, 10),
        plot.title = element_text(hjust = 0, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0, size = 14, margin = margin(b = 10)),
        plot.caption = element_text(size = 10, margin = margin(t = 10))
      ) +
      labs(
        title = dataset_title,
        subtitle = paste("Most Frequent Research Topics (", source_name, ")", sep = ""),
        caption = paste0("Word cloud shows the top 100 research topics associated with this dataset based on publications indexed in ", source_name, ".")
      )
    
    # Save topic counts to CSV by source
    # write.csv(topic_counts, file = file.path(output_dir, paste0(ds_id, "_", tolower(source_name), ".csv")), row.names = FALSE)
    
    CairoPNG(
      filename = file.path(output_dir, paste0(ds_id, "_", tolower(source_name), "_wordcloud.png")),
      width = 10,
      height = 8,
      units = "in",
      res = 300
    )
    print(p)
    dev.off()
  }
}

# ---------- By Source - Only for Shared DOIs ---------- # 

# -------------- LOOP OVER DATASETS × SOURCES -------------- #

# Define topic columns by source
topic_columns_by_source <- list(
  scopus = c("s_topic_1", "s_topic_2", "s_topic_3", "s_topic_4",
             "s_keyword01", "s_keyword02", "s_keyword03"),
  openalex = c("oa_topic_1", "oa_topic_2", "oa_topic_3"),
  dimensions = c("dim_topic_1", "dim_topic_2", "dim_topic_3", "dim_topic_4", "dim_topic_5")
)

# Define source display names for subtitles/captions
source_labels <- list(
  scopus = "Scopus",
  openalex = "OpenAlex",
  dimensions = "Dimensions"
)

# Loop over datasets
for (i in 1:12) {
  ds_id <- sprintf("dataset%02d", i)
  dataset_title <- dataset_titles[[ds_id]]
  message("Creating word clouds for: ", dataset_title)
  
  df <- get(paste0(ds_id, "_topics"))
  
  # Filter to DOIs indexed in all three sources
  df <- df %>%
    filter(scopus_yes == 1, oa_yes == 1, dim_yes == 1)
  
  n_shared_dois <- n_distinct(df$doi)
  
  # Loop over each source
  for (source in names(topic_columns_by_source)) {
    
    topic_cols <- topic_columns_by_source[[source]]
    topic_cols_present <- topic_cols[topic_cols %in% names(df)]
    
    topic_counts <- df %>%
      select(all_of(topic_cols_present)) %>%
      pivot_longer(everything(), values_to = "topic") %>%
      filter(!is.na(topic)) %>%
      filter(!grepl("\\(", topic)) %>%
      mutate(
        topic = topic %>%
          trimws() %>%
          tolower() %>%
          str_to_title()
      ) %>%
      count(topic, sort = TRUE) %>%
      slice_max(n, n = 100) %>%
      mutate(scaled_n = scales::rescale(n, to = c(5, 35)))
    
    # Skip if no topics present
    if (nrow(topic_counts) == 0) next
    
    # Create plot
    p <- ggplot(topic_counts, aes(label = topic, size = scaled_n, color = n)) +
      geom_text_wordcloud(area_corr = TRUE, rm_outside = TRUE, show.legend = FALSE) +
      scale_color_viridis_c(option = "turbo") +
      scale_size_area(max_size = 35) +
      theme_void(base_size = 14) +
      theme(
        plot.margin = margin(10, 10, 20, 10),
        plot.title = element_text(hjust = 0, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0, size = 14, margin = margin(b = 10)),
        plot.caption = element_text(size = 10, margin = margin(t = 10))
      ) +
      labs(
        title = dataset_title,
        subtitle = paste("Most Frequent Research Topics from", source_labels[[source]]),
        caption = paste0(
          "Only includes DOIs that are indexed in Scopus, OpenAlex, and Dimensions. ",
          "N = ", format(n_shared_dois, big.mark = ","), " shared DOIs."
        )
      )
    
    # Save word cloud PNG
    output_file <- file.path(output_dir, paste0(ds_id, "_wordcloud_shared_", source, ".png"))
    CairoPNG(filename = output_file, width = 10, height = 8, units = "in", res = 300)
    print(p)
    dev.off()
  }
}

# ---------- Topic Summary Tables ---------- # 

# Initialize list to collect topic count data
all_topic_counts <- list()

sources <- list(
  "Scopus" = quote(scopus_yes == 1),
  "OpenAlex" = quote(oa_yes == 1),
  "Dimensions" = quote(dim_yes == 1)
)

topic_cols <- c(
  "oa_topic_1", "oa_topic_2", "oa_topic_3", 
  "s_topic_1", "s_topic_2", "s_topic_3", "s_topic_4",
  "dim_topic_1", "dim_topic_2", "dim_topic_3", "dim_topic_4", "dim_topic_5",
  "s_keyword01", "s_keyword02", "s_keyword03"
)

for (i in 1:12) {
  ds_id <- sprintf("dataset%02d", i)
  dataset_title <- dataset_titles[[ds_id]]
  full_df <- get(paste0(ds_id, "_topics"))
  
  for (source_name in names(sources)) {
    df <- full_df %>% filter(!!sources[[source_name]])
    topic_cols_present <- topic_cols[topic_cols %in% names(df)]
    if (length(topic_cols_present) == 0) next
    
    topic_counts <- df %>%
      select(all_of(topic_cols_present)) %>%
      pivot_longer(everything(), values_to = "topic") %>%
      filter(!is.na(topic)) %>%
      filter(!grepl("\\(", topic)) %>%
      mutate(
        topic = topic %>%
          trimws() %>%
          tolower() %>%
          str_to_title()
      ) %>%
      count(topic, sort = TRUE) %>%
      mutate(
        dataset = ds_id,
        dataset_title = dataset_title,
        source = source_name
      )
    
    all_topic_counts[[paste(ds_id, source_name, sep = "_")]] <- topic_counts
  }
}

# Combine all results and write to CSV
master_topic_df <- bind_rows(all_topic_counts)

write_csv(master_topic_df, file.path(output_dir_tables, "master_topic_counts.csv"))

# 
# # ------------ LOOP -------------- #
# for (i in 1:12) {
#   ds_id <- sprintf("dataset%02d", i)
#   message("Processing: ", ds_id)
#   
#   # --- NEW: Extract dataset title ---
#   dataset_title <- dataset_reference_table %>%
#     filter(dataset_id == ds_id) %>%
#     pull(dataset_title) %>%
#     tools::file_path_sans_ext() %>%
#     str_trim()
# 
#   print(dataset_title)
#   
#   # Extract DOIs for OA FT
#   pub_df <- get(paste0(ds_id, "_pubs"))   # Pulls in dataset{num}_pubs which are filtered as type = "article" published between 2017-2023
#   oa_ft_dois <- pub_df %>% filter(oa_ft_yes == 1) %>% pull(doi)
#   
#   # Fetch OpenAlex metadata
#   works_meta <- oa_fetch(entity = "works", doi = oa_ft_dois, verbose = FALSE)
#   
#   # Parse OpenAlex Topics
#   oa_ft_topics_df <- works_meta %>%
#     select(doi, topics) %>%
#     unnest(topics) %>%
#     filter(type %in% c("topic", "subfield", "field", "domain")) %>%
#     arrange(doi, type, i) %>%
#     pivot_wider(
#       names_from = c(type, i),
#       values_from = display_name,
#       values_fn = list
#     ) %>%
#     mutate(across(
#       c(starts_with("topic"), starts_with("subfield"),
#         starts_with("field"), starts_with("domain")),
#       ~ map_chr(., ~ if (length(.) > 0) .[1] else NA_character_)
#     )) %>%
#     select(doi,
#            starts_with("topic"),
#            starts_with("subfield"),
#            starts_with("field"),
#            starts_with("domain")) %>%
#     distinct() %>%
#     arrange(doi) %>%
#     group_by(doi) %>%
#     summarize(
#       topic_1 = first(na.omit(topic_1)),
#       topic_2 = first(na.omit(topic_2)),
#       topic_3 = first(na.omit(topic_3)),
#       subfield_1 = first(na.omit(subfield_1)),
#       subfield_2 = first(na.omit(subfield_2)),
#       subfield_3 = first(na.omit(subfield_3)),
#       field_1 = first(na.omit(field_1)),
#       field_2 = first(na.omit(field_2)),
#       field_3 = first(na.omit(field_3)),
#       domain_1 = first(na.omit(domain_1)),
#       domain_2 = first(na.omit(domain_2)),
#       domain_3 = first(na.omit(domain_3)),
#       .groups = "drop"
#     ) %>%
#     mutate(doi = str_remove(doi, "^https://doi.org/"))
#   
#   # Extract DOIs for OA Seed
#   oa_seed_dois <- pub_df %>% filter(oa_seed_yes == 1) %>% pull(doi)
#   
#   # Fetch OpenAlex metadata
#   works_meta <- oa_fetch(entity = "works", doi = oa_seed_dois, verbose = FALSE)
#   
#   # Parse OpenAlex Topics
#   oa_seed_topics_df <- works_meta %>%
#     select(doi, topics) %>%
#     unnest(topics) %>%
#     filter(type %in% c("topic", "subfield", "field", "domain")) %>%
#     arrange(doi, type, i) %>%
#     pivot_wider(
#       names_from = c(type, i),
#       values_from = display_name,
#       values_fn = list
#     ) %>%
#     mutate(across(
#       c(starts_with("topic"), starts_with("subfield"),
#         starts_with("field"), starts_with("domain")),
#       ~ map_chr(., ~ if (length(.) > 0) .[1] else NA_character_)
#     )) %>%
#     select(doi,
#            starts_with("topic"),
#            starts_with("subfield"),
#            starts_with("field"),
#            starts_with("domain")) %>%
#     distinct() %>%
#     arrange(doi) %>%
#     group_by(doi) %>%
#     summarize(
#       topic_1 = first(na.omit(topic_1)),
#       topic_2 = first(na.omit(topic_2)),
#       topic_3 = first(na.omit(topic_3)),
#       subfield_1 = first(na.omit(subfield_1)),
#       subfield_2 = first(na.omit(subfield_2)),
#       subfield_3 = first(na.omit(subfield_3)),
#       field_1 = first(na.omit(field_1)),
#       field_2 = first(na.omit(field_2)),
#       field_3 = first(na.omit(field_3)),
#       domain_1 = first(na.omit(domain_1)),
#       domain_2 = first(na.omit(domain_2)),
#       domain_3 = first(na.omit(domain_3)),
#       .groups = "drop"
#     ) %>%
#     mutate(doi = str_remove(doi, "^https://doi.org/"))
#   
#   # Get Scopus topics
#   scopus_topics_df <- pub_df %>%
#     filter(scopus_yes == 1) %>%
#     select(doi) %>%
#     inner_join(scopus_topic %>%
#                  select(doi, 
#                         asjc_topic_1, 
#                         asjc_topic_2, 
#                         asjc_topic_3, 
#                         asjc_topic_4, 
#                         keyword01, 
#                         keyword02, 
#                         keyword03),
#                by = "doi") %>%
#     pivot_longer(
#       cols = starts_with("asjc_topic_"),
#       names_to = "i",
#       names_prefix = "asjc_topic_",
#       values_to = "asjc_topic"
#     ) %>%
#     filter(!is.na(asjc_topic)) %>%
#     distinct(doi, asjc_topic, .keep_all = TRUE) %>%
#     group_by(doi) %>%
#     mutate(i = row_number()) %>%
#     ungroup() %>%
#     pivot_wider(
#       names_from = i, 
#       values_from = asjc_topic, 
#       names_prefix = "asjc_topic_") %>%
#     distinct()
#   
#   # Get Dimensions concepts
#   dim_topics_df <- pub_df %>%
#     filter(dim_yes == 1) %>%
#     select(doi) %>%
#     inner_join(dim_concepts %>%
#                  select(doi, concept),
#                by = "doi") %>%
#     group_by(doi) %>%
#     mutate(i = row_number()) %>%
#     ungroup() %>%
#     pivot_wider(
#       names_from = i,
#       values_from = concept,
#       names_prefix = "concept_"
#     )
#   
#   # Join all topics
#   topics_final <- pub_df %>%
#     left_join(oa_seed_topics_df %>%
#                 rename(
#                   oa_seed_topic_1 = topic_1,
#                   oa_seed_topic_2 = topic_2,
#                   oa_seed_topic_3 = topic_3,
#                   oa_seed_subfield_1 = subfield_1,
#                   oa_seed_subfield_2 = subfield_2,
#                   oa_seed_subfield_3 = subfield_3,
#                   oa_seed_field_1 = field_1,
#                   oa_seed_field_2 = field_2,
#                   oa_seed_field_3 = field_3,
#                   oa_seed_domain_1 = domain_1,
#                   oa_seed_domain_2 = domain_2,
#                   oa_seed_domain_3 = domain_3),
#               by = "doi") %>%
#     left_join(scopus_topics_df %>%
#                 rename(
#                   s_topic_1 = asjc_topic_1,
#                   s_topic_2 = asjc_topic_2,
#                   s_topic_3 = asjc_topic_3,
#                   s_topic_4 = asjc_topic_4,
#                   
#                   s_keyword01 = keyword01,
#                   s_keyword02 = keyword02,
#                   s_keyword03 = keyword03), 
#               by = "doi") %>%
#     left_join(dim_topics_df %>%
#                 rename(
#                   dim_topic_1 = concept_1,
#                   dim_topic_2 = concept_2,
#                   dim_topic_3 = concept_3,
#                   dim_topic_4 = concept_4,
#                   dim_topic_5 = concept_5), 
#               by = "doi") %>%
#     left_join(oa_ft_topics_df %>%
#                 rename(
#                   oa_ft_topic_1 = topic_1,
#                   oa_ft_topic_2 = topic_2,
#                   oa_ft_topic_3 = topic_3,
#                   oa_ft_subfield_1 = subfield_1,
#                   oa_ft_subfield_2 = subfield_2,
#                   oa_ft_subfield_3 = subfield_3,
#                   oa_ft_field_1 = field_1,
#                   oa_ft_field_2 = field_2,
#                   oa_ft_field_3 = field_3,
#                   oa_ft_domain_1 = domain_1,
#                   oa_ft_domain_2 = domain_2,
#                   oa_ft_domain_3 = domain_3), 
#               by = "doi") %>%
#     select(doi,publication_year, oa_seed_yes, oa_ft_yes, scopus_yes, dim_yes, starts_with("dataset"),
#            starts_with("oa_seed_topic"),
#            starts_with("oa_seed_subfield"),
#            starts_with("oa_seed_field"),
#            starts_with("oa_seed_domain"),
#            starts_with("oa_ft_topic"),
#            starts_with("oa_ft_subfield"),
#            starts_with("oa_ft_field"),
#            starts_with("oa_ft_domain"),
#            starts_with("s_topic"),
#            starts_with("s_keyword"),
#            starts_with("dim_concept"),
#            everything()
#            ) %>%
#     mutate(across(
#       matches("oa_seed_topic_|oa_seed_subfield_|oa_seed_field_|oa_seed_domain_|oa_ft_topic_|oa_ft_subfield_|oa_ft_field_|oa_ft_domain_|s_topic_|s_keyword|dim_topic_"),
#       ~ ifelse(str_trim(str_to_lower(.x)) == !!str_trim(str_to_lower(dataset_title)), NA, .x)
#     )) %>%
#     distinct()
#   
#   # Define ordered topic columns from broadest to most granular
#   topic_cols_ordered <- c(
#     "oa_ft_topic_1", "oa_ft_topic_2", "oa_ft_topic_3", 
#     "oa_seed_topic_1", "oa_seed_topic_2", "oa_seed_topic_3", 
#     "s_topic_1", "s_topic_2", "s_topic_3", "s_topic_4",
#     "dim_topic_1", "dim_topic_2", "dim_topic_3", "dim_topic_4", "dim_topic_5",
#     "s_keyword01", "s_keyword02", "s_keyword03"
#   )
#   
#   # Safely handle missing columns (e.g., if scopus or dim missing)
#   topic_cols_present <- topic_cols_ordered[topic_cols_ordered %in% names(topics_final)]
#   
#   # Create combined_topics and topic_text
#   topics_final <- topics_final %>%
#     rowwise() %>%
#     mutate(
#       combined_topics = paste(unique(na.omit(c_across(all_of(topic_cols_present)))), collapse = "; "),
#       topic_text = gsub(";\\s*", " ", combined_topics),
#       topic_food_security = str_detect(topic_text, regex("\\bfood (in)?security\\b", ignore_case = TRUE))
#     ) %>%
#     ungroup()
#   
#   # Save to dynamically named object
#   assign(paste0(ds_id, "_topics"), topics_final)
# }
# 
# # ---- Food security flag terms ----
# all_topic_dfs <- mget(paste0("dataset", sprintf("%02d", 1:12), "_topics"))
# common_cols <- Reduce(intersect, lapply(all_topic_dfs, names))
# food_security_flag_terms <- bind_rows(
#   lapply(all_topic_dfs, function(df) df[, common_cols])
# ) %>%
#   # filter(topic_food_security == TRUE) %>%
#   distinct() %>%
#   select(doi, publication_year, combined_topics, topic_food_security, oa_seed_yes, oa_ft_yes, scopus_yes, dim_yes)
# 
# # write_csv(food_security_flag_terms, "food_security_flag_terms.csv") # N = 1,518
# 
# # Write to Excel
# # write_xlsx(all_topic_dfs, path = "all_datasets_topics.xlsx")
# # ----------------- END -----------------
# 
# # Combine all tibbles into one with a dataset ID column
# master_topics_df <- purrr::imap_dfr(
#   all_topic_dfs,
#   ~ mutate(.x, dataset_id = .y)
# ) 
# 
# topic_cols_ordered <- c(
#   "oa_ft_topic_1", "oa_ft_topic_2", "oa_ft_topic_3", 
#   "oa_seed_topic_1", "oa_seed_topic_2", "oa_seed_topic_3", 
#   "dim_topic_1", "dim_topic_2", "dim_topic_3", "dim_topic_4", "dim_topic_5",
#   "s_keyword01", "s_keyword02", "s_keyword03"
# )
# 
# asjc_table <- master_topics_df %>%
#   pivot_longer(
#     cols = c(s_topic_1, s_topic_2, s_topic_3, s_topic_4),
#     names_to = "asjc_level",
#     values_to = "asjc_topic",
#     values_drop_na = TRUE
#   ) %>%
#   mutate(ones = 1) %>%
#   group_by(asjc_topic) %>%
#   summarize(
#     doi_count         = sum(ones),
#     oa_seed_topic_1   = first(oa_seed_topic_1),
#     oa_seed_topic_2   = first(oa_seed_topic_2),
#     oa_seed_topic_3   = first(oa_seed_topic_3),
#     oa_seed_subfield_1 = first(oa_seed_subfield_1),
#     oa_seed_subfield_2 = first(oa_seed_subfield_2),
#     oa_seed_subfield_3 = first(oa_seed_subfield_3),
#     oa_seed_field_1   = first(oa_seed_field_1),
#     oa_seed_field_2   = first(oa_seed_field_2),
#     oa_seed_field_3   = first(oa_seed_field_3),
#     oa_seed_domain_1  = first(oa_seed_domain_1),
#     oa_seed_domain_2  = first(oa_seed_domain_2),
#     oa_seed_domain_3  = first(oa_seed_domain_3),
#     oa_ft_topic_1     = first(oa_ft_topic_1),
#     oa_ft_topic_2     = first(oa_ft_topic_2),
#     oa_ft_topic_3     = first(oa_ft_topic_3),
#     oa_ft_subfield_1  = first(oa_ft_subfield_1),
#     oa_ft_subfield_2  = first(oa_ft_subfield_2),
#     oa_ft_subfield_3  = first(oa_ft_subfield_3),
#     oa_ft_field_1     = first(oa_ft_field_1),
#     oa_ft_field_2     = first(oa_ft_field_2),
#     oa_ft_field_3     = first(oa_ft_field_3),
#     oa_ft_domain_1    = first(oa_ft_domain_1),
#     oa_ft_domain_2    = first(oa_ft_domain_2),
#     oa_ft_domain_3    = first(oa_ft_domain_3),
#     s_keyword01       = first(s_keyword01),
#     s_keyword02       = first(s_keyword02),
#     s_keyword03       = first(s_keyword03),
#     dim_topic_1       = first(dim_topic_1),
#     dim_topic_2       = first(dim_topic_2),
#     dim_topic_3       = first(dim_topic_3),
#     dim_topic_4       = first(dim_topic_4),
#     dim_topic_5       = first(dim_topic_5),
#     .groups = "drop"
#   ) %>%
#   rowwise() %>%
#   mutate(
#     combined_topic_text = paste(c_across(oa_seed_topic_1:dim_topic_5), collapse = " "),
#     topic_food_security = str_detect(str_to_lower(combined_topic_text), "food (in)?security")
#   ) %>%
#   ungroup() %>%
#   select(-combined_topic_text)
# 
# # write_xlsx(asjc_table, path = "asjc_table.xlsx")

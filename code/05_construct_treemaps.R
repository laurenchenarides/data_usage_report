# ======================================================================
# Title: Source Overlap Treemap Summary and Publication Matching
# Author: Lauren Chenarides
# Last Updated: June 11, 2025
#
# Description:
# This script prepares publication-level comparisons across Scopus and 
# OpenAlex (seed corpus and full-text search) for 12 USDA-related datasets. 
# Dataset13 ("Transition of Agricultural Land Survey") is merged into 
# Dataset07 ("TOTAL Survey") due to conceptual overlap and dataset search 
# redundancy.
#
# Note:
#   - Only pub_type = articles are retained for comparison report.
#   - Only articles published between 2017-2023 are compared. 
# 
# Steps:
#   - Cleans and aligns Scopus, OpenAlex Seed, and OpenAlex Full-Text records
#   - Matches publications by DOI and ISSN across sources
#   - Constructs publication-level flags indicating dataset coverage
#   - Summarizes mutually exclusive publication groups:
#       • Scopus only
#       • OpenAlex Seed only
#       • OpenAlex FT only
#       • Pairwise and 3-way overlaps
#   - Generates treemaps visualizing source overlaps by dataset
#   - Builds a cross-dataset summary table with group counts and total distinct DOIs
#
# Inputs:
#   - master_scopus: Cleaned Scopus publication data
#   - master_openalex: OpenAlex (full-text and seed search) publication data
#   - master_dimensions: Cleaned Dimensions publication data
#   - dataset_reference_table: Dataset name mapping
#
# Outputs:
#   - Individual dataset-level publication tables (`datasetXX_pubs`)
#   - Treemap plots saved to /graphics/treemaps/
#   - Group overlap summary table:
#       `openalex_group_summary_all_datasets.csv`
#
# Required Libraries:
#   - dplyr, tidyr, stringr, readr, tools, purrr, treemapify, Cairo, ggplot2, RColorBrewer
#
# Datasets to keep for report:
# - dataset05: Census of Ag
# - dataset06: RUCC
# - dataset01: ARMS
# - dataset10: Household food security module
# - dataset09: FoodAPS
# - dataset08: FARA
# ======================================================================


# ====================================================================== #
# ------- Publication Tree Maps --------- 
# ------- LOOP ------- 
# ====================================================================== #

# Define all mutually exclusive group labels
all_groups <- tibble(group = factor(c(
  "Scopus only",
  "OpenAlex only",
  "Dimensions only",
  "Scopus ∩ OpenAlex",
  "Scopus ∩ Dimensions",
  "OpenAlex ∩ Dimensions",
  "Scopus ∩ OpenAlex ∩ Dimensions"
), levels = c(
  "Scopus only",
  "OpenAlex only",
  "Dimensions only",
  "Scopus ∩ OpenAlex",
  "Scopus ∩ Dimensions",
  "OpenAlex ∩ Dimensions",
  "Scopus ∩ OpenAlex ∩ Dimensions"
)))

# Create a list of your datasets (assuming they exist in your environment)
datasets <- mget(paste0("dataset", str_pad(1:12, 2, pad = "0"), "_pubs"))

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

# Initialize summary table
group_names <- c(
  "Scopus only",
  "OpenAlex only",
  "Dimensions only",
  "Scopus ∩ OpenAlex",
  "Scopus ∩ Dimensions",
  "OpenAlex ∩ Dimensions",
  "Scopus ∩ OpenAlex ∩ Dimensions"
)

# Define 7 distinct colors
bright_colors <- palette36.colors(7)

# Slightly desaturate and lighten the colors using HCL space
muted_colors <- colorspace::desaturate(bright_colors, amount = 0.3) %>%
  colorspace::lighten(amount = 0.2)

# Assign group names
names(muted_colors) <- group_names

summary_df <- tibble(dataset = character(), !!!setNames(rep(list(integer()), length(group_names)), group_names))

# Loop through each dataset
for (i in seq_along(datasets)) {
  
  df <- datasets[[i]]
  dataset_name <- names(datasets)[i]
  pretty_name <- dataset_titles[dataset_name]
  
  # Assign mutually exclusive group
  # Assign mutually exclusive group based on Scopus, OpenAlex (oa_yes), and Dimensions
  temp_summary <- df %>%
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
    mutate(group = factor(group, levels = group_names)) %>%
    count(group, name = "n") %>%
    right_join(all_groups, by = "group") %>%
    mutate(n = replace_na(n, 0)) %>%
    mutate(
      pct_label = paste0(round(100 * n / sum(n), 1), "%"),
      label = paste0(group, "\n", n, " (", pct_label, ")")
    )
  
  # Add to summary table
  summary_df <- summary_df %>%
    bind_rows(
      temp_summary %>%
        select(group, n) %>%
        pivot_wider(names_from = group, values_from = n) %>%
        mutate(dataset = pretty_name) %>%
        select(dataset, all_of(levels(all_groups$group)))
    )
  
  # Set factor order again just in case
  temp_summary <- temp_summary %>%
    mutate(group = factor(group, levels = levels(all_groups$group)))
  
  # Treemap plot
  p <- ggplot(temp_summary, aes(area = n, fill = group, label = label)) +
    geom_treemap() +
    geom_treemap_text(
      colour = "black",
      place = "centre",
      grow = FALSE,
      reflow = TRUE,
      min.size = 0.5,
      size = 10
    ) +
    scale_fill_manual(values = muted_colors, drop = FALSE) +
    guides(fill = guide_legend(title = "Source", ncol = 1)) +
    labs(
      title = paste("Publication Coverage by Source for", pretty_name),
      subtitle = paste("Total Distinct DOIs:", sum(temp_summary$n)),
      caption = "Each box represents a mutually exclusive group of DOIs.\nOverlap means a publication was found in more than one source.\nIf a group does not appear in the visualization, no publications were found in that category.\nAll DOIs shown are associated with publications classified as document type = 'article' published between 2017 and 2023."
    ) +
    theme_minimal() +
    theme(legend.position = "right")
  
  CairoPNG(
    filename = file.path(output_dir, paste0(gsub(" ", "_", pretty_name), "_treemap.png")),
    width = 10, height = 6, units = "in", dpi = 300
  )
  print(p)
  dev.off()
}


# Loop through each dataset, count DOIs
dataset_summary <- tibble(
  dataset_id = names(dataset_titles),
  title = unname(dataset_titles),
  N = sapply(names(dataset_titles), function(obj_name) {
    if (exists(obj_name, envir = .GlobalEnv)) {
      df <- get(obj_name, envir = .GlobalEnv)
      n_distinct(df$doi)
    } else {
      NA_integer_
    }
  })
)

# Step 2: Arrange by descending N
dataset_summary <- dataset_summary %>%
  arrange(desc(N))

# View result
print(dataset_summary)

# Join the total count into summary_df
summary_df_final <- summary_df %>%
  left_join(dataset_summary %>% 
              select(title, N), 
            by = c("dataset" = "title"))

# Export the final summary table
write_csv(summary_df_final, file.path(output_dir, "summary_all_datasets.csv"))

# ================================================================
# Title: Master Script for "Methodology for Comparing Citation Database Coverage of Dataset Usage"
# Author: Lauren Chenarides
# Last Updated: June 27, 2025
#
# Description:
# This script executes the full workflow for processing and comparing
# OpenAlex (seed & full-text), Scopus, and Dimensions metadata. It calls each modular
# script in sequence.
# 
# Program Order:
# 1. `process_openalex.R`  
# 2. `process_scopus_seed_corpus.R`  
# 3. `process_dimensions.R`  

# 4. `compare_publications.R`  
# 5. `construct_treemaps.R`  
# 6. `construct_sankey_plots.R`

# 7. `aggregate_topics.R`
# 8. `construct_bubble_charts.R`
# 9. `construct_word_clouds.R`

# 10. `clean_author_names.R`
# 11. `clean_institutional_affil.R`  
# 12. `construct_maps.R`
# ================================================================

# ---- Load Required Packages for All Source Files Using pacman ----

# Install pacman if not already installed
if (!require("pacman")) install.packages("pacman")

# Load and install (if necessary) all required packages
pacman::p_load(
  here,
  dplyr,
  tidyr,
  stringr,
  readr,
  tibble,
  purrr,
  tools,
  readxl,
  ggplot2,
  forcats,
  Cairo,
  treemapify,
  viridis,
  RColorBrewer,
  janitor,
  tidyverse,
  openalexR,
  packcircles,
  ggrepel,
  patchwork,
  Polychrome,
  networkD3,
  htmltools,
  webshot,
  writexl,
  openxlsx,
  wordcloud,
  tm,
  grid,
  ggwordcloud,
  scales,
  fuzzyjoin,
  stringdist,
  sf,
  maps
)

# ---- Set Working Directory ----
# This path is the main folder where the source files are saved.

setwd(here("data"))
getwd()

# Important: Source each script in order.

# ---- Part 1: Cleaning and Deduplicating Data [Process] ---- 

# ---- OpenAlex ----

# Note: Prior to running this script, you must run the flatten_openalex_all_datasets.ipynb in Jupyter Notebook. This notebook processes the raw JSON files from the OpenAlex Seed Search Corpus and flattens them into structured CSV tables.

# === Set this path to the location where the Seed Corpus flattened json files are stored === #
base_path <- here("code", "raw", "openalex", "seed_corpus")
print(base_path)

# === Set this path to the location where the OpenAlex Full Text files are stored === #
folder_path <- here("code", "raw", "openalex", "full_text")
print(folder_path)

source("01_process_openalex.R")

keep_list <- c("master_openalex")

rm(list = setdiff(ls(), keep_list))
gc()

# ---- Scopus Seed Search ----

# === Set this path to the location where the Scopus files are stored === #
folder_path <- here("code", "raw", "scopus") 
print(folder_path)

# === Set this path to the location where the parent-alias table is stored === #
base_path <- here("code", "reference_files")
print(base_path)

source("02_process_scopus_seed_corpus.R")

keep_list <- c("master_scopus", # Added
               "scopus_pubs",  # Added
               "dyad_model_dataset",  # Added
               "scopus_author", # Added
               "scopus_doi", # Added
               "scopus_topic", # Added
               "master_openalex")

rm(list = setdiff(ls(), keep_list))
gc()

# ---- Dimensions ----

# === Set this path to the location where the Dimensions files are stored === #
input_folder <- here("code", "raw", "dimensions", "v8")
print(input_folder)

# === Set this path to the location where the parent-alias table is stored === #
base_path <- here("code", "reference_files")
print(base_path)

source("03_process_dimensions.R")

keep_list <- c("dataset_reference_table",  # Added
               "master_dimensions",  # Added
               "dim_publications",  # Added
               "dim_authors",  # Added
               "dim_affiliations",  # Added
               "dim_concepts",  # Added
               "master_scopus", 
               "scopus_pubs",  
               "dyad_model_dataset",  
               "scopus_author", 
               "scopus_doi", 
               "scopus_topic", 
               "master_openalex")

rm(list = setdiff(ls(), keep_list))
gc()

# ==== Until this point, all publication types across all years have been processed. Only in the subsequent steps will we apply filtering to compare type = "article" published between 2017-2023. ==== #

# ---- PREP Data for Comparing Publications ---- 

source("04_compare_publications.R") # Compares pub_type = articles published between 2017-2023

# ---- Part 2: Comparing Publications (Treemaps) [Compare] ---- 

# === Set folder path (this is the folder where the treemaps will be saved) === #
output_dir <- here("graphics", "treemaps")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

source("05_construct_treemaps.R") 

# ---- Part 3: Comparing Journals (Sankey Plots) [Construct] ---- 

# === Set folder path (this is the folder where the sankey plots will be saved) === #
output_dir <- here("graphics", "sankey_plots2")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

source("06_construct_sankey_plots.R")

# ---- Part 4: Aggregate Topics Labels [Build] ---- 

# === Set folder path (this is the folder where the topic tables will be saved) === #
output_dir_tables <- here("graphics", "topic_tables")
dir.create(output_dir_tables, showWarnings = FALSE, recursive = TRUE)

source("07_aggregate_topics.R")

# ---- Part 4-1: Comparing Topics (Bubble Charts) [Construct] ---- 

# === Set folder path (this is the folder where the bubble charts will be saved) === #
output_dir <- here("graphics", "bubble_charts")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

source("08_construct_bubble_charts.R")

# ---- Part 4-2: Comparing Topics (Word Clouds) [Construct] ---- 

# === Set folder path (this is the folder where the word clouds will be saved) === #
output_dir <- here("graphics", "word_clouds")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Output directory for CSV tables
output_dir_tables <- here("graphics", "word_clouds", "topic_tables")
dir.create(output_dir_tables, showWarnings = FALSE, recursive = TRUE)

source("09_construct_word_clouds.R")

# ---- Part 5-1: Comparing Authors [Construct] ---- 

# === Set folder path (this is the folder where the author-topic tables will be saved) === #
output_dir_tables <- here("graphics", "topic_tables", "authors")
dir.create(output_dir_tables, showWarnings = FALSE, recursive = TRUE)

# === Set folder path (this is the folder where the tile maps will be saved) === #
output_dir_vis <- here("graphics", "tilemaps")
dir.create(output_dir_vis, recursive = TRUE, showWarnings = FALSE)

source("10_clean_author_names.R") 

# ---- Part 5-2: Comparing Institutions [Construct] ---- 

# Read in data from OpenSyllabus 
# Source1: https://ror.readme.io/docs/mapping#other-external-identifiers
# Source2: https://github.com/opensyllabus/institution-identifiers 
base_path <- here("code", "reference_files")
print(base_path)
institution_ids <- read_csv(file.path(base_path,"institution_identifiers.csv")) # Note: Only available for type = Education

doi_ror_locations <- read_csv(file.path(base_path,"doi_ror_pairs_with_locations.csv"))

source("11_clean_institutional_affil.R")


# === Set folder path (this is the folder where the maps will be saved) === #
output_dir <- here("graphics", "maps")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

source("12_construct_maps.R")



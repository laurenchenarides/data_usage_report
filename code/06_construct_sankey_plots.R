# ====================================================================== #
# Title: Sankey Plot Generator for Journal-Level Source Inclusion
# Author: Lauren Chenarides
# Last Updated: June 11, 2025
#
# -------------------------------------------------------------
# Description:
# This script generates Sankey diagrams showing how journals are distributed 
# across four citation data sources (Scopus, OpenAlex Seed, OpenAlex Full Text, 
# and Dimensions) for a set of 12 datasets. The flow illustrates which journals 
# are indexed in which sources, and how many appear in multiple sources.
#
# Outputs:
# - One HTML and one PNG Sankey diagram per dataset (top 40 journals only)
# - Saves files to a defined output directory
#
# Required packages: networkD3, dplyr, tidyr, readr, stringr, Cairo, webshot
# ====================================================================== #

# Ensure PhantomJS is installed (for webshot screenshots)
if (!webshot::is_phantomjs_installed()) webshot::install_phantomjs()

# ------- Journal Name by Source Sankey Plot --------- #
# ------------- LOOP ----------- #

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

# Dataset identifiers, e.g., dataset01 through dataset12
dataset_vars <- paste0("dataset", str_pad(1:12, 2, pad = "0"))

# Loop over each dataset to generate a Sankey plot
for (ds in dataset_vars) {
  
  # Skip if dataset object doesn't exist in memory
  journal_obj <- paste0(ds, "_journals")
  if (!exists(journal_obj, envir = .GlobalEnv)) next
  
  # Get journal data and corresponding pretty name
  df <- get(journal_obj, envir = .GlobalEnv)
  pretty_name <- dataset_titles[[paste0(ds, "_pubs")]]
  
  # Sanitize and shorten file name for saving
  safe_label <- gsub("[^a-zA-Z0-9_-]", "_", pretty_name)
  safe_label <- str_trunc(safe_label, 50, ellipsis = "")  # Limit to 50 characters
  
  # Reshape to long format and get top 40 journals by total inclusion count
  df_long <- df %>%
    select(journal_title, scopus_yes, oa_yes, dim_yes) %>%
    mutate(total_sources = rowSums(across(starts_with("scopus"):starts_with("dim")))) %>%
    arrange(desc(total_sources)) %>%
    slice_head(n = 40) %>%
    pivot_longer(cols = c(scopus_yes, oa_yes, dim_yes),
                 names_to = "source", values_to = "included") %>%
    filter(included == 1) %>%
    mutate(source = recode(source,
                           scopus_yes = "Scopus",
                           oa_yes = "OpenAlex",
                           dim_yes = "Dimensions"))
  
  # Build link table for Sankey input
  links <- df_long %>%
    mutate(value = 1,
           source_name = source,
           target_name = journal_title) %>%
    select(source_name, target_name, value)
  
  # Define unique nodes from sources and journal titles
  nodes <- tibble(name = unique(c(links$source_name, links$target_name)))
  
  # Match source and target to node indices
  links <- links %>%
    mutate(source = match(source_name, nodes$name) - 1,
           target = match(target_name, nodes$name) - 1)
  
  # Create Sankey diagram
  sankey <- sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = "source",
    Target = "target",
    Value = "value",
    NodeID = "name",
    sinksRight = FALSE,
    fontSize = 12,         # Optional: Increase font size
    nodeWidth = 30,        # Optional: Wider nodes
    width = 1000,          # Increase width
    height = 700,          # Increase height
    colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10)"),
    # Caption added via title attribute on hover text (not a traditional static caption)
    # You can also use an HTML wrapper to add static caption if embedding later
    nodePadding = 10
  )
  
  # Define file paths
  html_file <- file.path(output_dir, paste0(safe_label, "_sankey.html"))
  png_file <- file.path(output_dir, paste0(safe_label, "_sankey.png"))
  
  # Save HTML version (interactive, can hover to explore)
  # saveNetwork(sankey, file = html_file, selfcontained = TRUE)
  
  # Caption below the widget
  caption_div <- tags$div(
    style = "margin-top: 40px; font-size:13px; font-family:Arial, sans-serif; color:#444; max-width:800px;",
    " Each flow connects a citation database on the left (Scopus, OpenAlex Seed, OpenAlex Full Text, Dimensions) to the journals it indexes (right).",
    " Journals indexed by multiple sources will have multiple flows.",
    " This diagram shows the top 40 journals (by total DOI count) included in at least one source for this dataset."
  )
  
  # Wrap widget + caption in a full HTML page
  html_page <- tags$html(
    tags$head(tags$meta(charset = "utf-8")),
    tags$body(
      div(style = "margin: 40px;", sankey, caption_div)
    )
  )
  
  # Save HTML file
  htmltools::save_html(html_page, file = html_file)
  
  # Screenshot with webshot
  webshot::webshot(url = html_file, file = png_file, vwidth = 1300, vheight = 900)

  # ---- Save full journal inclusion table as CSV ---- #
  journal_table_out <- df %>%
    select(journal_title, scopus_yes, oa_yes, dim_yes) %>%
    arrange(journal_title)
  
  write.csv(
    journal_table_out,
    file = file.path(output_dir, paste0(safe_label, "_journal_index_table.csv")),
    row.names = FALSE
  )
}

rm(df_long, html_page)

# -----------------------------------------
# Interpretation:
# Each Sankey diagram shows the flow from each citation database (left side)
# to the journals indexed by those sources (right side). Flows represent
# inclusion, e.g., if a journal is included in both Scopus and OpenAlex FT,
# it will have two links from each source to that journal.
#
# This helps visualize coverage overlap and identify journals included in
# multiple or only one source.
# -----------------------------------------
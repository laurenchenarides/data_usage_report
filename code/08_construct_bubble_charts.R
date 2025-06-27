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

generate_topic_bubbles <- function(df, topic_prefix, source_label, top_n = 20, source_name,
                                   other_topic_sets, subtitle) {
  topic_cols <- grep(paste0("^", topic_prefix, "_topic_"), names(df), value = TRUE)
  
  # Count unique DOIs to include in caption
  n_dois <- df %>%
    filter(if_any(all_of(topic_cols), ~ !is.na(.))) %>%
    distinct(doi) %>%
    nrow()
  
  topic_counts <- df %>%
    select(all_of(topic_cols)) %>%
    pivot_longer(
      cols = everything(), 
      names_to = "col", 
      values_to = "topic"
    ) %>%
    filter(!is.na(topic)) %>%
    mutate(topic = stringr::str_to_title(topic)) %>%   # << Moved up here
    count(topic, name = "n") %>%
    arrange(desc(n)) %>%
    slice_head(n = top_n) 

  packing <- circleProgressiveLayout(topic_counts$n, sizetype = "area")
  topic_counts <- bind_cols(topic_counts, packing)
  topic_counts$label <- stringr::str_wrap(topic_counts$topic, width = 25)
  dat.gg <- circleLayoutVertices(packing, npoints = 60)
  
  ggplot() +
    geom_polygon(
      data = dat.gg, 
      aes(x, y, group = id, fill = topic_counts$origin[id]),
      fill = "#66C2A5", 
      alpha = 0.8, 
      linewidth = 0.3
      ) +
    geom_text(
      data = topic_counts, 
      aes(x, y, label = label), 
      size = 2.1, 
      lineheight = 0.9
      ) +
    coord_equal() +
    # scale_fill_manual(
    #   values = c("Exclusive" = "#66C2A5", "Shared" = "#FFD92F"),
    #   drop = FALSE
    # ) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0, face = "bold", size = 13, margin = margin(b = 5)),
      plot.subtitle = element_text(hjust = 0, size = 9),
      plot.caption = element_text(hjust = 0, size = 6, margin = margin(t = 10)),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    ) +
    labs(
      title = paste("Top", top_n, "Topics in", source_label),
      subtitle = paste("Dataset:", subtitle),
      caption = paste0(
        "Each circle represents a topic assigned to one or more of the ", n_dois, " DOIs ",
        "that reference the selected dataset in this source. \nCircle size reflects the number of publications ",
        "associated with that topic. ",
        "\nNote: the base set of DOIs is not mutually exclusive across sources."
      )
    )
}

get_topics <- function(df, topic_prefix) {
  # Match columns like "prefix_1", "prefix_2", etc.
  cols <- grep(paste0("^", topic_prefix, "_[0-9]+$"), names(df), value = TRUE)
  if (length(cols) == 0) {
    warning(paste("No topic columns found for prefix:", topic_prefix))
    return(character(0))
  }
  df %>%
    select(all_of(cols)) %>%
    pivot_longer(
      cols = everything(), 
      values_to = "topic"
      ) %>%
    filter(!is.na(topic)) %>%
    distinct(topic) %>%
    mutate(topic = stringr::str_to_title(topic)) %>%
    pull(topic)
}


for (i in 1:12) {
  ds_id <- sprintf("dataset%02d", i)
  dataset_label <- dataset_titles[ds_id]
  topic_obj <- get(paste0(ds_id, "_topics"))
  
  # Build topic sets (converted to title case for consistent matching)
  scopus_topics  <- stringr::str_to_title(get_topics(topic_obj %>% filter(scopus_yes == 1), "s"))
  oa_topics   <- stringr::str_to_title(get_topics(topic_obj %>% filter(oa_yes == 1), "oa"))
  dimensions_topics <- stringr::str_to_title(get_topics(topic_obj %>% filter(dim_yes == 1), "dim"))
  
  # Generate plots with per-dataset exclusivity
  p1 <- generate_topic_bubbles(
    topic_obj %>% filter(scopus_yes == 1),
    topic_prefix = "s",
    source_label = "Scopus",
    source_name = "Scopus",
    other_topic_sets = list(oa_topics, dimensions_topics),
    subtitle = dataset_label
  )
  
  p2 <- generate_topic_bubbles(
    topic_obj %>% filter(oa_yes == 1),
    topic_prefix = "oa",
    source_label = "OpenAlex Full Text",
    source_name = "OA",
    other_topic_sets = list(scopus_topics, dimensions_topics),
    subtitle = dataset_label
  )
  
  p3 <- generate_topic_bubbles(
    topic_obj %>% filter(dim_yes == 1),
    topic_prefix = "dim",
    source_label = "Dimensions",
    source_name = "Dimensions",
    other_topic_sets = list(scopus_topics, oa_topics),
    subtitle = dataset_label
  )
  
  # Save outputs
  CairoPNG(filename = file.path(output_dir, paste0(ds_id, "_topics_scopus.png")), width = 7, height = 6, units = "in", res = 300)
  print(p1)
  dev.off()
  
  CairoPNG(filename = file.path(output_dir, paste0(ds_id, "_topics_oa.png")), width = 7, height = 6, units = "in", res = 300)
  print(p2)
  dev.off()
  
  CairoPNG(filename = file.path(output_dir, paste0(ds_id, "_topics_dimensions.png")), width = 7, height = 6, units = "in", res = 300)
  print(p3)
  dev.off()
}




# ==============================================================================
# Script: construct_maps.R
# Author: Lauren Chenarides
# Last updated: June 27, 2025
# Purpose: Generate U.S. state-level choropleth maps of DOI counts 
#          normalized by number of institutions per state, for each 
#          citation source (Scopus, Dimensions, OpenAlex).
#
# Inputs:
#   - doi_ror_pairs_with_locations: Dataframe with DOI-level institutional
#     affiliations, RORs, and U.S. state names
#
# Outputs:
#   - PNG maps saved to: graphics/maps/
#     - scopus_normalized_dois_per_institution.png
#     - dimensions_normalized_dois_per_institution.png
#     - openalex_normalized_dois_per_institution.png
#
# Dependencies:
#   - Requires libraries: sf, maps, dplyr, ggplot2, stringr, viridis, Cairo
#   - Assumes `output_dir` is defined using `here::here()`
# ==============================================================================

# -----------------------------
# Base: US state shapefile
# -----------------------------
us_states <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>%
  mutate(state = tolower(ID))

# -----------------------------
# Caption wrapper
# -----------------------------
wrap_caption <- function(text, width = 100) {
  paste(strwrap(text, width = width), collapse = "\n")
}

# -----------------------------
# Plot function per source
# -----------------------------
plot_doi_map <- function(source_col, source_label, output_file) {
  
  # Step 1: Prepare state-level summary
  doi_state_summary <- doi_ror_pairs_with_locations %>%
    filter(.data[[source_col]] == TRUE, country_code == "US", !is.na(state), !is.na(display_name)) %>%
    mutate(state = tolower(state)) %>%
    distinct(doi, display_name, state)
  
  state_summary <- doi_state_summary %>%
    group_by(state) %>%
    summarize(
      total_dois = n_distinct(doi),
      n_institutions = n_distinct(display_name),
      doi_per_institution = total_dois / n_institutions,
      .groups = "drop"
    )
  
  # Step 2: Merge with shapefile
  us_states_merged <- us_states %>%
    left_join(state_summary, by = "state")
  
  # Step 3: Build caption
  caption_text <- state_summary %>%
    arrange(desc(total_dois)) %>%
    mutate(entry = paste0(str_to_title(state), " (", total_dois, "; ", n_institutions, ")")) %>%
    pull(entry) %>%
    paste(collapse = ", ")
  
  wrapped_caption <- wrap_caption(
    paste0("State DOI totals reflect ", source_label, "-indexed publications and are normalized by the number of institutions in each state with at least one publication indexed in this source. ",
           "How to read: State (total DOIs; total institutions). ",
           caption_text),
    width = 100
  )
  
  # Step 4: Plot
  CairoPNG(output_file, width = 1400, height = 1000, res = 150)
  
  p <- ggplot(us_states_merged) +
    geom_sf(aes(fill = doi_per_institution), color = "white", size = 0.2) +
    scale_fill_viridis_c(option = "plasma", name = "DOIs per Institution") +
    coord_sf(xlim = c(-125, -66), ylim = c(25, 50)) +
    theme_minimal(base_size = 12) +
    theme(plot.caption = element_text(size = 8, hjust = 0)) +
    labs(
      title = paste0("Normalized DOI Count by State (", source_label, ")"),
      subtitle = paste0("Map shows total DOIs indexed in ", source_label,
                        " divided by number of unique institutions per state."),
      caption = wrapped_caption,
      x = "", y = ""
    )
  
  print(p)  # <- Ensures plot is rendered in non-interactive contexts
  dev.off()
}

# -----------------------------
# Run for each source
# -----------------------------
plot_doi_map("source_scopus",     "Scopus",     file.path(output_dir, "scopus_normalized_dois_per_institution.png"))
plot_doi_map("source_dimensions", "Dimensions", file.path(output_dir, "dimensions_normalized_dois_per_institution.png"))
plot_doi_map("source_oa",         "OpenAlex",   file.path(output_dir, "openalex_normalized_dois_per_institution.png"))

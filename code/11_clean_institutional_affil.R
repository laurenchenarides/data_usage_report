# ==============================================================================
# Script Name:  clean_institutional_affil.R
# Author:    Lauren Chenarides
# Last updated: June 27, 2025
# Description:  This script standardizes and merges institutional affiliation 
#               data from Scopus, Dimensions, and OpenAlex. It also integrates 
#               ROR metadata and U.S. institutional identifiers for subsequent 
#               institutional-level analysis, including geolocation.
#
# Inputs:
#   - `scopus_author`          : Author-level metadata from Scopus
#   - `df_scopus`              : Cleaned Scopus publication metadata
#   - `dim_authors`            : Author-level metadata from Dimensions
#   - `df_dimensions`          : Cleaned Dimensions publication metadata
#   - `df_openalex`            : Cleaned OpenAlex publication metadata
#   - `works_meta`             : Full OpenAlex authorship metadata
#   - `all_works_meta`         : Combined OpenAlex metadata for institutional affiliations
#   - `institution_identifiers.csv` : External file with IPEDS/MSI + ROR crosswalk
#   - `doi_ror_pairs_with_locations.csv` : External file with ROR location metadata
#
# Outputs:
#   - `affil_all`              : Cleaned, combined institutional affiliation dataset
#   - `doi_ror_pairs_with_locations` : Final institutional data with geolocation
#
# Dependencies:
#   - Requires `oa_fetch()` function for querying OpenAlex
#   - Packages: dplyr, stringr, tidyr, readr, purrr
#
# Execution Time: Moderate
# ==============================================================================


# -------------------------
# 1. Helper function: Normalize author names
# -------------------------
normalize_name <- function(name) {
  name %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%          # Convert accented or special chars to ASCII
    str_replace_all("\\*", " ") %>%                            # Remove asterisks
    str_replace_all("[\u00A0\u200B]", " ") %>%                 # Replace non-breaking/invisible spaces with space
    str_to_lower() %>%
    str_replace_all("[^[:alnum:][:space:]'\\-\\.]", " ") %>%   # Replace strange characters with space
    str_replace_all("[[:punct:]]", " ") %>%                    # Remove punctuation
    str_squish() %>%                                           # Trim and collapse whitespace
    str_trim()                                                 # Final trim
}

# -------------------------
# 2. Process and clean each source
# -------------------------

# -------  Scopus ------ 
affil_scopus_clean <- scopus_author %>%
  left_join(df_scopus %>% select(doi, publication_year),
            by = "doi",
            relationship = "many-to-many") %>%
  filter(publication_year > 2016 & publication_year < 2023) %>%
  mutate(
    institution = institution_name
  ) %>%
  select(doi, institution) %>%
  mutate(source_scopus = TRUE) %>%
  arrange(doi) %>%
  filter(!is.na(institution)) %>%
  distinct()

nrow(affil_scopus_clean) # N = 19,044
length(unique(affil_scopus_clean$doi)) # N = 6,135

# ------ Dimensions ------ 
affil_dim_clean <- dim_authors %>%
  left_join(df_dimensions %>% select(doi, publication_year),
            by = "doi",
            relationship = "many-to-many") %>%
  filter(publication_year > 2016 & publication_year < 2023) %>%
  mutate(
    institution = raw_affiliations
  ) %>%
  select(doi, institution) %>%
  mutate(source_dimensions = TRUE) %>%
  arrange(doi) %>%
  filter(!is.na(institution)) %>%
  distinct()

nrow(affil_dim_clean) # N = 19,673
length(unique(affil_dim_clean$doi)) # N = 6,107

# ------ OpenAlex ------ 

# Extract DOIs for OA
oa_dois <- df_openalex %>% pull(doi) # N = 3,709

# Fetch OpenAlex metadata
works_meta <- oa_fetch(entity = "works", doi = oa_dois, verbose = FALSE)
colnames(works_meta)

# Parse OpenAlex Metadata
oa_author_df <- works_meta %>%
  select(doi, authorships) %>%
  unnest(authorships) 

length(unique(oa_author_df$doi)) # N = 3,699

affil_oa_clean <- oa_author_df %>%
  select(doi, affiliations) %>%
  unnest(affiliations) %>%
  mutate(doi = str_remove(doi, "^https://doi.org/")) %>%
  rename(institution = display_name) %>%
  select(doi, institution, ror) %>%
  distinct() %>%
  mutate(source_oa = TRUE) 

nrow(affil_oa_clean) # N = 9,514
length(unique(affil_oa_clean$doi)) # N = 3,487


# ------- Institutional Affiliations ------- 

affil_works <- all_works_meta %>%
  select(doi, authorships) %>%
  unnest(authorships) %>%
  select(doi, affiliations) %>%
  unnest(affiliations) %>%
  select(doi, display_name, ror, type, country_code) %>%
  distinct() %>%
  mutate(doi = str_remove(doi, "^https://doi.org/"))


# Combine and fill missing flags
affil_all <- affil_works %>%
  left_join(affil_scopus_clean, by = "doi", relationship = "many-to-many") %>%
  arrange(-source_scopus) %>%
  left_join(affil_dim_clean, by = "doi", relationship = "many-to-many") %>%
  arrange(-source_dimensions) %>%
  left_join(affil_oa_clean %>% select(-ror), by = "doi", relationship = "many-to-many") %>%
  arrange(-source_oa) %>%
  arrange(doi) %>%
  mutate(
    institution_name = coalesce(institution, institution.x, institution.y)
    ) %>%
  select(-institution, -institution.x, -institution.y) %>%
  select(doi, institution_name, ror, source_scopus, source_dimensions, source_oa, everything()) %>%
  group_by(doi, display_name) %>%
  summarize(
    # institution = first(na.omit(display_name)),
    ror = first(na.omit(ror)),
    type = first(na.omit(type)),
    country_code = first(na.omit(country_code)),
    source_scopus = any(source_scopus, na.rm = TRUE),
    source_dimensions = any(source_dimensions, na.rm = TRUE),
    source_oa = any(source_oa, na.rm = TRUE),
    .groups = "drop"
  )

length(unique(affil_all$doi)) # N = 10,710 (89.87% of all pubs have institutional information)
length(all_dois) # N = 11,917



# Filter to valid RORs and deduplicate
doi_ror_pairs <- affil_all %>%
  filter(!is.na(ror)) %>%
  arrange(doi, ror) %>%
  mutate(ror_id = str_remove(ror, "^https://ror.org/")) %>%
  left_join(institution_ids, 
            by = "ror_id",
            relationship = "many-to-one") %>%
  filter(country_code == "US") %>%
  filter(type == "education" | type == "funder")

test <- doi_ror_pairs %>%
  mutate(ones = ifelse(is.na(unitid),0,1)) %>%
  group_by(type) %>%
  summarize(
    has_unitid = sum(ones),
    .groups = 'drop'
  )
print(test)

length(unique(doi_ror_pairs$doi)) # N = 8,317 out of 11,917 publications
length(unique(doi_ror_pairs$ror)) # N = 1,042



ror_locations<- doi_ror_locations %>%
  select(-doi) %>%
  distinct()
nrow(ror_locations) # N = 3,918

doi_ror_pairs_with_locations <- affil_all %>%
  mutate(ror_id = str_remove(ror, "^https://ror.org/")) %>%
  left_join(ror_locations, by = "ror") %>%
  mutate(
    type = coalesce(type.x, type.y),
    display_name = coalesce(display_name.x, display_name.y)
  ) %>%
  select(doi, display_name, ror, ror_id, country_code, type, starts_with("source_"), org_established, org_status, org_types, city, geonames_admin1_name, latitude, longitude) %>%
  rename(state = geonames_admin1_name)

colnames(doi_ror_pairs_with_locations)


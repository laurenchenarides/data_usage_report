# ==========================================================================
# This script processes the Scopus files to create a set of descriptive tables.
# Folder: appendices/scopus_files
# File name: build_scopus_tables.R
# Author: Lauren Chenarides
# Last updated: March 17, 2025
# ==========================================================================

library(here)
library(readr)
library(tidyverse)
library(tidyr)

getwd()
setwd(here::here("appendices/scopus_files"))
getwd()

# ============================
# Top 100 Topics
# ============================
# Load the data
publication_topic <- read_csv("publication_topic.csv")
topic <- read_csv("topic.csv")

# Check the structure of the data (optional, but recommended)
glimpse(topic)

# Perform a left join to retain all publications and add journal info
publication_topic_join <- publication_topic %>%
  select(-run_id, -last_updated_date) %>%
  left_join(topic, by = c("topic_id" = "id")) %>%
  select(-last_updated_date, -external_topic_id)

# Split and count keywords
keyword_counts <- publication_topic_join %>%
  separate_rows(keywords, sep = "\\|") %>%  # splits keywords by "|"
  mutate(keywords = str_trim(keywords)) %>% # removes any extra whitespace
  count(keywords, sort = TRUE)

# Display the top 100 most common keywords
top_100_keywords <- head(keyword_counts, 100)

# View the top 100 keywords
print(top_100_keywords)

# Export top 100 keywords
write_csv(top_100_keywords,"../tables/top_100_keywords.csv")

# ============================
# Top 100 Journals
# ============================
# Load the data
publication <- read_csv("publication.csv")
journal <- read_csv("journal.csv")
publisher <- read_csv("publisher.csv")

# Perform a left join to retain all publications and add journal info
publication_journal <- publication %>%
  select(-run_id, -external_id, -tested_expressions, -last_updated_date) %>%
  rename(pub_title = title) %>%
  left_join(journal, by = c("journal_id" = "id")) %>%
  rename(publication_id = id, journal_title = title) %>%
  select(-last_updated_date, -external_id, -run_id) %>%
  left_join(publisher, by = c("publisher_id" = "id")) %>%
  select(-last_updated_date, -external_id, -run_id) %>%
  rename(publisher_name = name) %>%
  arrange(publisher_name)
  
# Check the joined data structure
glimpse(publication_journal)

# Find the top 100 journals by publication count
top_100_journals <- publication_journal %>%
  count(journal_id, journal_title, sort = TRUE) %>%  # title.y comes from journal.csv after join
  rename(publication_count = n) %>%
  slice_max(publication_count, n = 100)

# View the top 100 journals
print(top_100_journals)

# Export top 100 journals
write_csv(top_100_journals,"../tables/top_100_journals.csv")

# ============================
# Top 100 Authors
# ============================
# Load the data
author <- read_csv("author.csv")
publication_author <- read_csv("publication_author.csv")
author_affiliation <- read_csv("author_affiliation.csv")
publication_affiliation <- read_csv("publication_affiliation.csv")

# Perform a left join to retain all publications and add journal info
publication_author_join <- publication_author %>%
  select(-run_id, -last_updated_date) %>%
  left_join(author, by = c("author_id" = "id")) %>%
  rename(publication_author_id = id) %>%
  select(-last_updated_date, -external_id, -run_id, -given_name.y, -family_name.y) %>%
  arrange(publication_author_id)
  

publication_author_affiliation_join <- publication_author_join %>%
  left_join(author_affiliation, by = c("publication_author_id" = "publication_author_id")) %>%
  select(-last_updated_date, -run_id) %>%
  arrange(publication_author_id)

publication_author_affiliation_join <- publication_author_affiliation_join %>%
  left_join(publication_affiliation, by = c("publication_affiliation_id" = "id")) %>%
  select(-last_updated_date) 

# Check the joined data structure
glimpse(publication_author_affiliation_join)

# Find the top 100 journals by publication count
top_100_authors <- publication_author_affiliation_join %>%
  filter(country_code == "usa") %>%
  count(author_id, given_name.x, family_name.x, sort = TRUE) %>%
  rename(publication_count = n) %>%
  slice_max(publication_count, n = 100)

# View the top 100 authors
print(top_100_authors)

# Export top 100 journals
write_csv(top_100_authors,"../tables/top_100_authors.csv")


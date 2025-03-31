# Load required libraries
library(tidyverse)
library(readxl)
library(data.table)
library(janitor)
library(knitr)
library(fuzzyjoin)
library(stringi)
library(dplyr)
library(ggplot2)

getwd()

# ---------- Step 1: Clean IPEDS Data ----------

# Set working directory relative to the project root
setwd(file.path(getwd(), "data", "ipeds_msi", "raw_ipeds"))

# ---------- Institutional classification data ----------

hd_files <- list.files(".", pattern = "^hd20(1[7-9]|2[0-3])\\.csv$", full.names = TRUE)
process_hd_file <- function(file_path) {
  df <- read_csv(file_path, col_types = cols()) %>%
    select(UNITID,OPEID,INSTNM,ADDR, CITY,STABBR,ZIP,HBCU,ICLEVEL,CONTROL,CARNEGIE,LANDGRNT,LONGITUD,LATITUDE) # Keep only necessary columns
  # UNITID: Unique identification number of the institution
  # INSTNM: Institution (entity) name
  # ADDR: Street address or post office box
  # CITY: City location of institution
  # STABBR: State abbreviation
  # HBCU: Historically Black College or University, 1 IS YES, 2 IS NO
  # SECTOR: One of nine institutional categories resulting from dividing the universe according to control and level.   
  # CONTROL: Public, CONTROL==1; Private not-for-profit, CONTROL=2; private for-profit, CONTROL=3; not available, CONTROL=-3. Description: Control categories are public, private not-for-profit, and private for-profit. Level categories are 4-year and higher (4 year), 2-but-less-than 4-year (2 year), and less than 2-year. For example: public, 4-year institutions.
  # ICLEVEL: 4-year or higher (4 year), ICLEVEL=1; 2-but-less-than 4-year (2 year), ICLEVEL=2; or less than 2-year, ICLEVEL=3; not available, ICLEVEL=-3
  # CARNEGIE: 
  # LANDGRNT: 1, land grant; 2, not a land grant
  # LONGITUDE: 
  # LATITUDE: 
  year <- str_extract(basename(file_path), "\\d{4}") %>% # Extract year from filename (e.g., "effy2019.csv" -> 2019)
    as.integer()
  df <- df %>% 
    mutate(year = year) # Add the year column
  return(df)
}
hd_list <- lapply(hd_files, process_hd_file)
hd <- bind_rows(hd_list) %>% 
  mutate(HBCU=ifelse(HBCU==1,1,0)) %>%
  filter(ICLEVEL %in% c(1, 2)) # Keep 4-year or 2-year universities only

# Count how many institutions by year
hd %>%
  group_by(year) %>%
  summarise(num_unitid = n_distinct(UNITID))

# Check if there are duplicates
hd %>%
  count(UNITID, year) %>%
  filter(n > 1)
# Confirmed - no duplicates

unitid_opeid_counts <- hd %>%
  group_by(UNITID, year) %>%
  summarise(num_opeid = n_distinct(OPEID)) %>%
  arrange(year, desc(num_opeid), UNITID)
unique(unitid_opeid_counts$num_opeid)

# ---------- Enrollment data ----------

effy_files <- list.files(".", pattern = "^effy20(1[7-9]|2[0-3])\\.csv$", full.names = TRUE)
process_effy_file <- function(file_path) {
  df <- read_csv(file_path, col_types = cols()) %>%
    select(UNITID,EFFYLEV, EFYTOTLT) # Keep only necessary columns
    # UNITID: Unique identification number of the institution
    # EFFYLEV: Level of study, 1-All students total, 2--undergraduate, 4-graduate
    # EFYTOTLT: Grand total men and women enrolled for credit during the 12-month reporting period
  year <- str_extract(basename(file_path), "\\d{4}") %>% # Extract year from filename (e.g., "effy2019.csv" -> 2019)
    as.integer()
  df <- df %>% 
    mutate(year = year) # Add the year column
  return(df)
}
effy_list <- lapply(effy_files, process_effy_file)
enrollment <- bind_rows(effy_list) %>% 
  filter(EFFYLEV == 1)

head(enrollment)

# Count how many institutions by year
enrollment %>%
  group_by(year) %>%
  summarise(num_unitid = n_distinct(UNITID))


# ---------- Grant and endowment data ----------

f1a_files <- list.files(".", pattern = "^f[0-9]{4}_f1a\\.csv$", full.names = TRUE)
print(f1a_files)
process_f1a_file <- function(file_path) {
  df <- read_csv(file_path, col_types = cols()) %>%
    select(UNITID,F1H01, F1H02) # Keep only necessary columns
  # UNITID: Unique identification number of the institution
  # F1H01: Value of endowment assets at the beginning of the fiscal year
  # F1H02: Value of endowment assets at the end of the fiscal year
  year_string <- str_extract(basename(file_path), "(?<=f)\\d{4}") # Extract year from filename (e.g., "effy2019.csv" -> 2019)
  # Convert to full 4-digit year (1617 -> 2017, 1718 -> 2018, ..., 2223 -> 2023)
  if (!is.na(year_string)) {
    start_year <- as.integer(substr(year_string, 1, 2)) + 2000 # Extract "16" -> 2016
    year <- start_year + 1 # Convert to the second year in the range (2017)
  } else {
    year <- NA # If extraction fails, set year as NA
  }
  df <- df %>% 
    mutate(year = year) # Add the year column
  return(df)
}
f1a_list <- lapply(f1a_files, process_f1a_file)
endowment <- bind_rows(f1a_list)

head(endowment)

# Count how many institutions by year
endowment %>%
  group_by(year) %>%
  summarise(num_unitid = n_distinct(UNITID))

# ---------- Library budget data ----------

al_files <- list.files(".", pattern = "^al20(1[7-9]|2[0-3])\\.csv$", full.names = TRUE)
process_al_file <- function(file_path) {
  df <- read_csv(file_path, col_types = cols()) %>%
    select(UNITID,LEXPTOT) # Keep only necessary columns
    # UNITID: Unique identification number of the institution
    # LEXPTOT: Total expenditures (salaries/wages, benefits, materials/services, and operations/maintenance)
  year <- str_extract(basename(file_path), "\\d{4}") %>% # Extract year from filename (e.g., "effy2019.csv" -> 2019)
    as.integer()
  df <- df %>% 
    mutate(year = year) # Add the year column
  return(df)
}
al_list <- lapply(al_files, process_al_file)
library <- bind_rows(al_list)

head(library)

# Count how many institutions by year
library %>%
  group_by(year) %>%
  summarise(num_unitid = n_distinct(UNITID))

# ---------- Merge IPEDS components ----------
IPEDS <- hd %>%
  left_join(enrollment, by = c("UNITID", "year")) %>%
  left_join(endowment, by = c("UNITID", "year")) %>%
  left_join(library, by = c("UNITID", "year")) %>%
  distinct()

nrow(IPEDS)

# Count how many institutions by year
IPEDS %>%
  group_by(year) %>%
  summarise(num_unitid = n_distinct(UNITID))

# Check if there are duplicates
IPEDS %>%
  count(UNITID, INSTNM, year) %>%
  filter(n > 1)
# Confirmed - no duplicates

IPEDS <- IPEDS %>%
  rename(instname = INSTNM, assets_start = F1H01, assets_end = F1H02)

write_csv(IPEDS,"../clean_data/ipeds.csv")

# ---------- Step 2: Clean MSI Data ----------

# Set working directory relative to project root
setwd(file.path(getwd(), "..", "raw_msi"))

# ---------- MSI Data 2017–2021 ----------

# Load and clean main MSI dataset
msi_1721_raw <- read_excel("MSI Data Project - July 2023 - V1.1.xlsx", sheet = "Data") %>%
  clean_names() 
dput(colnames(msi_1721_raw))

# Define the MSI types (lowercase for matching)
msi_types <- c("tccu", "hsi", "aanapisi", "hbcu", "hbgi", "pbi", "nasnti", "annh")

# Build a regex pattern to match any of the substrings
pattern <- paste(msi_types, collapse = "|")

# Get the column names that match
matching_columns <- grep(pattern, names(msi_1721_raw), ignore.case = TRUE, value = TRUE)
# Print the result
print(matching_columns)

elig_columns <- grep("m_elig_", matching_columns, value = TRUE)
# Print the result
print(elig_columns)

funded_columns<- grep("_funded", matching_columns, value = TRUE)
# Print the result
print(funded_columns)

# ---------- FUNDED MSI Data 2021–2023 ----------

msi_1721_funded <- msi_1721_raw %>%
  filter(matchtype %in% "1: UnitID, OPEID, and State") %>%
  select(UNITID = 'unitid', opeid, year, instname, funded_atleastone, eligtxt, all_of(funded_columns), level, control, stabbr) %>% # Keep only necessary columns
  group_by(UNITID, year) %>%
  ungroup() %>%
  filter(UNITID!="UNITID") %>%
  filter(stabbr!="PR") %>%
  separate(instname, into = c("instname", "campus"), sep = " - ") %>% # Drop campus observations
  filter(is.na(campus)) %>%
  select(-campus) %>%
  filter(opeid!= "00106308") # Drop Interior Alaska Campus in 2017
  
unitid_opeid_counts <- msi_1721_funded %>%
  group_by(UNITID, year) %>%
  summarise(num_opeid = n_distinct(opeid)) %>%
  arrange(year, desc(num_opeid), UNITID) %>%
  filter(num_opeid > 1)
# Results in a list of distinct UNITID's by year. Removed all duplicates by only selecting the "main" campus.

head(msi_1721_funded)
colnames(msi_1721_funded)

# Construct MSI type flags
msi_1721_funded <- msi_1721_funded %>%
  mutate(
    msi_type_hbcu = if_else(hbcu_funded == "Currently funded" |
                              hbcu_ma_funded == "Currently funded" |
                              hbgi_funded == "Currently funded", 1, 0),
    msi_type_aanapisi = if_else(aanapisi_funded == "Currently funded" |
                                  aanapisi_f_funded == "Currently funded", 1, 0),
    msi_type_annhsi = if_else(annhsi_funded == "Currently funded" |
                                annhsi_f_funded == "Currently funded", 1, 0),
    msi_type_hsi = if_else(hsi_funded == "Currently funded" |
                             hsi_ppoha_funded == "Currently funded" |
                             hsi_stem_funded == "Currently funded", 1, 0),
    msi_type_nasnti = if_else(nasnti_funded == "Currently funded" |
                                nasnti_f_funded == "Currently funded", 1, 0),
    msi_type_tccu = if_else(tccu_funded == "Currently funded", 1, 0),
    msi_type_pbi = if_else(pbi_a_funded == "Currently funded" |
                             pbi_f_funded == "Currently funded" |
                             pbi_md_funded == "Currently funded", 1, 0)
  ) %>%
  select(UNITID, year, opeid, instname, stabbr, level, control,
         starts_with("msi_type_")) %>%
  distinct() %>%
  mutate(
    nonprofit = if_else(str_detect(control, "not-for-profit"), 1, 0),
    control = str_replace(control, "Private not-for-profit", "Private"),
    forprofit = if_else(str_detect(control, "for-profit"), 1, 0),
    control = str_replace(control, "Private for-profit", "Private")
  ) %>%
  mutate(across(c(msi_type_hbcu:forprofit), ~replace_na(.x, 0))) 

head(msi_1721_funded)
colnames(msi_1721_funded)

# Check if there are duplicates
msi_1721_funded %>%
  count(UNITID, opeid, year) %>%
  filter(n > 1)
# Confirmed - no duplicates

unique(msi_1721_funded$control)
unique(msi_1721_funded$level)

# ---------- ELIGIBLE MSI Data 2021–2023 ----------

msi_1721_elig <- msi_1721_raw %>%
  filter(matchtype %in% "1: UnitID, OPEID, and State") %>%
  select(UNITID = 'unitid', opeid, year, instname, funded_atleastone, eligtxt, all_of(elig_columns), m_tccu, level, control, stabbr) %>% # Keep only necessary columns
  group_by(UNITID, year) %>%
  ungroup() %>%
  filter(UNITID!="UNITID") %>%
  filter(stabbr!="PR") %>%
  separate(instname, into = c("instname", "campus"), sep = " - ") %>% # Drop campus observations
  filter(is.na(campus)) %>%
  select(-campus) %>%
  filter(opeid!= "00106308") # Drop Interior Alaska Campus in 2017

unitid_opeid_counts <- msi_1721_elig %>%
  group_by(UNITID, year) %>%
  summarise(num_opeid = n_distinct(opeid)) %>%
  arrange(year, desc(num_opeid), UNITID) %>%
  filter(num_opeid > 1)
# Results in a list of distinct UNITID's by year. Removed all duplicates by only selecting the "main" campus.

head(msi_1721_elig)
colnames(msi_1721_elig)

# Construct MSI type flags
msi_1721_elig <- msi_1721_elig %>%
  mutate(
    msi_type_hbcu = if_else(grepl("Ineligible", m_elig_hbcu, ignore.case = TRUE), 0, 1),
    msi_type_aanapisi = if_else(grepl("Ineligible", m_elig_aanapisi, ignore.case = TRUE) |
                                  grepl("Ineligible", m_elig_aanapisi_f, ignore.case = TRUE), 0, 1),
    msi_type_annhsi = if_else(grepl("Ineligible", m_elig_annhsi, ignore.case = TRUE) |
                                grepl("Ineligible", m_elig_annhsi_f, ignore.case = TRUE), 0, 1),
    msi_type_hsi = if_else(grepl("Ineligible", m_elig_hsi, ignore.case = TRUE) |
                             grepl("Ineligible", m_elig_hsi_ppoha, ignore.case = TRUE) |
                             grepl("Ineligible", m_elig_hsi_stem, ignore.case = TRUE), 0, 1),
    msi_type_nasnti = if_else(grepl("Ineligible", m_elig_nasnti, ignore.case = TRUE) |
                                grepl("Ineligible", m_elig_nasnti_f, ignore.case = TRUE), 0, 1),
    msi_type_tccu = if_else(grepl("Ineligible", m_tccu, ignore.case = TRUE), 0, 1),
    msi_type_pbi = if_else(grepl("Ineligible", m_elig_pbi, ignore.case = TRUE) |
                             grepl("Ineligible", m_elig_pbi_f, ignore.case = TRUE), 0, 1)
  ) %>%
  select(UNITID, year, opeid, instname, stabbr, level, control,
         starts_with("msi_type_")) %>%
  distinct() %>%
  mutate(
    nonprofit = if_else(str_detect(control, "not-for-profit"), 1, 0),
    control = str_replace(control, "Private not-for-profit", "Private"),
    forprofit = if_else(str_detect(control, "for-profit"), 1, 0),
    control = str_replace(control, "Private for-profit", "Private")
  ) %>%
  mutate(across(c(msi_type_hbcu:forprofit), ~replace_na(.x, 0))) 

head(msi_1721_elig)
colnames(msi_1721_elig)

# Check if there are duplicates
msi_1721_elig %>%
  count(UNITID, opeid, year) %>%
  filter(n > 1)
# Confirmed - no duplicates

unique(msi_1721_elig$control)
unique(msi_1721_elig$level)

# ---------- MSI Data 2021–2023 ----------

# Function to read, clean, and add a year column
read_msi_data <- function(file, year) {
  df <- read_excel(file) %>%
    clean_names() %>%  # Standardize column names
    mutate(year = year,  # Add year column
           type_control = as.character(type_control))  # Ensure type_control is character
  
  # Standardize column names
  colnames(df) <- gsub("\r\n", "", colnames(df))  # Remove newline characters
  
  return(df)
}

# Read all datasets and ensure type_control is character
msi2021 <- read_msi_data("MSI_List_2021.xlsx", 2021)
msi2021 <- msi2021 %>%
  mutate(unit_id = str_replace_all(unit_id, "[^0-9]", "")) %>%  # Remove non-numeric characters
  filter(unit_id!= "") %>%
  filter(state!="PR") %>%
  separate(institution_name, into = c("instname", "campus"), sep = " - ") %>% # Drop campus observations
  filter(is.na(campus)) %>%
  select(-campus) %>%
  filter(opeid!= "00106308") %>% # Drop Interior Alaska Campus in 2017
  separate(type_control, into = c("type", "control"), sep = " ") %>%
  filter(control!= "") %>%
  mutate(level = control, control = type) %>%
  select(-type) %>%
  distinct()

msi2022 <- read_msi_data("MSI_List_2022.xlsx", 2022)
msi2022 <- msi2022 %>%
  mutate(unit_id = str_replace_all(unit_id, "[^0-9]", "")) %>%  # Remove non-numeric characters
  filter(unit_id!= "") %>%
  filter(state!="PR") %>%
  separate(institution_name, into = c("instname", "campus"), sep = " - ") %>% # Drop campus observations
  filter(is.na(campus)) %>%
  select(-campus) %>%
  separate(type_control, into = c("type", "control"), sep = " ") %>%
  filter(control!= "") %>%
  mutate(level = control, control = type) %>%
  select(-type) %>%
  distinct()  %>%
  mutate(
    nonprofit = if_else(str_detect(control, "not-for-profit"), 1, 0),
    control = str_replace(control, "Private not-for-profit", "Private"),
    forprofit = if_else(str_detect(control, "for-profit"), 1, 0),
    control = str_replace(control, "Private for-profit", "Private")
  )

unique(msi2022$control)
unique(msi2022$level)

msi2023 <- read_msi_data("MSI_List_2023.xlsx", 2023)
msi2023 <- msi2023 %>%
  mutate(unit_id = str_replace_all(unit_id, "[^0-9]", "")) %>%  # Remove non-numeric characters
  filter(unit_id != "") %>%
  filter(state!="PR") %>%
  separate(institution_name, into = c("instname", "campus"), sep = " - ") %>% # Drop campus observations
  filter(is.na(campus)) %>%
  select(-campus) %>%
  separate(type_control, into = c("type", "control"), sep = ", ", extra = "merge", fill = "right") %>%
  filter(control != "") %>%
  mutate(level = control, control = type) %>%
  select(-type) %>%
  distinct()  %>%
  mutate(
    nonprofit = if_else(str_detect(control, "not-for-profit"), 1, 0),
    control = str_replace(control, "Private not-for-profit", "Private"),
    forprofit = if_else(str_detect(control, "for-profit"), 1, 0),
    control = str_replace(control, "Private for-profit", "Private")
  ) %>%
  mutate(level = case_when(
    level == "4-year or above" ~ "4yr",
    level == "2-year" ~ "2yr",
    TRUE ~ level  # Keep all other values unchanged
  )) 

unique(msi2023$control)
unique(msi2023$level)

# ---------- Combine MSI components ----------

# Combine datasets correctly using `bind_rows()`
msi_2223 <- bind_rows(msi2022, msi2023) %>%
  mutate(year = as.character(year)) %>%
  mutate(msi_type_hbcu = ifelse(grepl("HBCU", msi_type, ignore.case = TRUE), 1, 0),
         msi_type_aanapisi = ifelse(grepl("AANAPISI", msi_type, ignore.case = TRUE), 1, 0),
         msi_type_annhsi = ifelse(grepl("ANNH", msi_type, ignore.case = TRUE), 1, 0),
         msi_type_hsi = ifelse(grepl("HSI", msi_type, ignore.case = TRUE), 1, 0),
         msi_type_nasnti = ifelse(grepl("NASNTI", msi_type, ignore.case = TRUE), 1, 0),
         msi_type_tccu = ifelse(grepl("TCU", msi_type, ignore.case = TRUE), 1, 0),
         msi_type_pbi = ifelse(grepl("PBI", msi_type, ignore.case = TRUE), 1, 0)) %>%
  arrange(instname, year) %>%
  select(-msi_type) %>%
  distinct() %>%
  rename(UNITID = unit_id) %>%
  arrange(UNITID, year)

colnames(msi_2223)

unique(msi_2223$control)
unique(msi_2223$level)
unique(msi_2223$year)

nrow(msi_2223)

unitid_opeid_counts <- msi_2223 %>%
  group_by(UNITID, year) %>%
  mutate(ones=1) %>%
  summarise(num_opeid = sum(ones)) %>%
  arrange(year, desc(num_opeid), UNITID) %>%
  filter(num_opeid > 1)

# Collapse duplicates by UNITID and YEAR, keeping MAX of each MSI flag
msi_2223 <- msi_2223 %>%
  group_by(UNITID, year) %>%
  summarise(
    across(starts_with("msi_type_"), max, na.rm = TRUE),
    instname = first(instname),
    city = first(city),
    state = first(state),
    control = first(control),
    level = first(level),
    nonprofit = max(nonprofit),
    .groups = "drop"
  ) %>%
  mutate(across(c(msi_type_hbcu:msi_type_pbi), ~replace_na(.x, 0))) %>%
  arrange(UNITID, year)

nrow(msi_2223) # dropped 10 duplicates

# Check if there are duplicates
msi_2223 %>%
  count(UNITID, year) %>%
  filter(n > 1)
# Confirmed - no more duplicates

nrow(msi_2223) 

# ---------- Final MSI Table ----------

msi_all <- bind_rows(msi_1721_elig, msi_2223) %>% # Update msi_1721_elig OR msi_1721_funded
  arrange(UNITID, year)

unique(msi_all$year)
unique(msi_all$control)
unique(msi_all$level)

unitid_opeid_counts <- msi_all %>%
  group_by(UNITID, year) %>%
  summarise(num_opeid = n_distinct(opeid)) %>%
  arrange(year, desc(num_opeid), UNITID) %>%
  filter(num_opeid > 1)

colnames(msi_all)

# Add total MSI flag
msi_final <- msi_all %>%
  # sum: number of MSI designations
  # fund: at least one designation
  mutate(
    sum = rowSums(across(starts_with("msi_type_"))),
    fund = if_else(sum > 0, 1, 0)
  ) %>%
  # Fill in missing values with the previous year
  arrange(UNITID, year) %>%
  group_by(UNITID) %>%
  fill(level, control, .direction = "down") %>%
  ungroup() %>%
  select(-city, -state, -stabbr) %>%
  distinct()

unique(msi_final$control)
unique(msi_final$level)

# Check if there are duplicates
msi_final %>%
  count(UNITID, instname, year) %>%
  filter(n > 1)

View(msi_final)

# Save
write_csv(msi_final, "../clean_data/msi.csv")

# ------ Step 3: Merge IPEDS & MSI Data -------

non_us_states <- c("AS", "GU", "MP", "PR", "FM", "PW", "VI", "MH")

ipeds_panel <- IPEDS %>%
  select(UNITID, OPEID, instname, CITY, STABBR, ZIP, LONGITUD, LATITUDE) %>%
  separate(ZIP, into = c("ZIP", "ZIP4"), sep = "-", fill = "right") %>%
  select(-ZIP4) %>%
  distinct() %>%
  crossing(year = 2017:2023) %>%
  mutate(year = as.character(year)) %>%
  group_by(UNITID, instname, year) %>%
  slice(1) %>%
  ungroup() %>%
  filter(!STABBR %in% non_us_states)

# Check if there are duplicates
ipeds_panel %>%
  count(UNITID, instname, year) %>%
  filter(n > 1)

ipeds_char <- IPEDS %>%
  select(UNITID, year, ICLEVEL, CONTROL, CARNEGIE, LANDGRNT, EFFYLEV, EFYTOTLT, assets_start, assets_end, LEXPTOT, HBCU) %>%
  rename(level_of_study = EFFYLEV, enrollment = EFYTOTLT, library_exp = LEXPTOT) %>%
  mutate(year = as.character(year)) 

msi_final$year<-as.character(msi_final$year)

ipeds_msi <- ipeds_panel %>%
  left_join(ipeds_char, by = c("UNITID", "year")) %>%
  arrange(UNITID, year) %>%
  mutate(UNITID = as.character(UNITID)) %>%
  left_join(msi_final %>% select(-instname), by = c("UNITID", "year")) %>%
  distinct() %>%
  arrange(UNITID, year) %>% 
  mutate(level = case_when(
    ICLEVEL == 1 ~ "4yr",
    ICLEVEL == 2 ~ "2yr",
    ICLEVEL == 3 ~ "Less than 2yr",
    TRUE ~ level  # Keep all other values unchanged
  )) %>%
  mutate(control = case_when(
    CONTROL == 1 ~ "Public",
    CONTROL == 2 ~ "Private",
    CONTROL == 3 ~ "Private",
    TRUE ~ control  # Keep all other values unchanged
  )) %>%
  mutate(forprofit = case_when(
    CONTROL == 3 ~ 1,
    TRUE ~ forprofit  # Keep all other values unchanged
  )) %>%
  mutate(nonprofit = case_when(
    CONTROL == 2 ~ 1,
    TRUE ~ nonprofit  # Keep all other values unchanged
  )) %>%
  mutate(across(c(nonprofit:forprofit), ~replace_na(.x, 0))) %>%
  distinct() %>%
  select(UNITID, OPEID, -opeid, year, instname, CITY:LATITUDE, ICLEVEL, level, CONTROL, control, forprofit, nonprofit, CARNEGIE, LANDGRNT, level_of_study, enrollment, assets_start, assets_end, library_exp, HBCU, msi_type_hbcu:msi_type_pbi, sum, fund) %>%
  mutate(msi_type_hbcu = case_when(
    is.na(msi_type_hbcu) & HBCU == 1 ~ 1,
    is.na(msi_type_hbcu) & HBCU == 0 ~ 0,
    TRUE ~ msi_type_hbcu
  )) 

# Function to get the mode (most frequent value)
mode_or_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  ux <- na.omit(x)
  as.numeric(names(sort(table(ux), decreasing = TRUE))[1])
}

msi_vars <- c(
  "msi_type_hbcu", "msi_type_aanapisi", "msi_type_annhsi",
  "msi_type_hsi", "msi_type_nasnti", "msi_type_tccu", "msi_type_pbi"
)

# Loop through and fill NAs using prior-year mode
ipeds_msi <- ipeds_msi %>%
  arrange(UNITID, year) %>%
  group_by(UNITID)

for (var in msi_vars) {
  ipeds_msi <- ipeds_msi %>%
    mutate(!!sym(var) := if_else(
      is.na(.data[[var]]),
      mode_or_na(lag(.data[[var]])),
      .data[[var]]
    ))
}

ipeds_msi <- ipeds_msi %>% 
  ungroup() %>%
  # sum: number of MSI designations
  # fund: at least one designation
  mutate(
    sum = rowSums(across(starts_with("msi_type_"))),
    fund = if_else(sum > 0, 1, 0)
  ) %>%
  group_by(UNITID) %>%
  mutate(fund_2021 = if_else(year == "2021", fund, NA_real_),
         fund_2022 = if_else(year == "2022", fund, NA_real_)) %>%
  fill(fund_2021, fund_2022, .direction = "downup") %>%  # Fill across years
  ungroup() %>%
  mutate(jump_2022 = if_else(fund_2021 == 0 & fund_2022 == 1, 1, 0))

colnames(ipeds_msi)
unique(ipeds_msi$level)
unique(ipeds_msi$control)
unique(ipeds_msi$CARNEGIE)
unique(ipeds_msi$ICLEVEL)
unique(ipeds_msi$level)
unique(ipeds_msi$ICLEVEL)

# Check if there are duplicates
ipeds_msi %>%
  count(UNITID, instname, year) %>%
  filter(n > 1)
# Confirmed - no duplicates

# Count how many institutions by year
ipeds_msi %>%
  group_by(year) %>%
  summarise(num_unitid = n_distinct(UNITID))

# ------ Step 4: Summary Tables -------

ipeds_msi_summary <- ipeds_msi %>%
  filter(!is.na(enrollment)) %>%
  group_by(year) %>%
  summarise(
    eligible_institutions = sum(fund == 1, na.rm = TRUE),
    jump_2022_count = if_else(first(year) == "2022", sum(jump_2022 == 1, na.rm = TRUE), NA_integer_),
    total_institutions = n(),
    percent_eligible = round((eligible_institutions / total_institutions) * 100, 2),
    .groups = "drop"
  )

# View result
print(ipeds_msi_summary)

# Define a rescale factor to bring percent into same range as eligible_institutions
scale_factor <- max(ipeds_msi_summary$eligible_institutions, na.rm = TRUE) / 
  max(ipeds_msi_summary$percent_eligible, na.rm = TRUE)

# Create updated caption with total institution counts
caption_text <- paste0(
  "Note: Data sources differ across years.\n",
  "2017–2021: MSI Data Project (Nguyen et al., 2023); ",
  "2022–2023: Rutgers CMSI\n",
  "Filters: Only 2- or 4-year institutions in the 50 US states. Enrollment > 0.\n",
  "MSI Types: ANNHSI, AANAPISI, HSI, HBCU, NASNTI, PBI, TCCU.\n",
  "Total Institutions per Year:\n",
  paste0(ipeds_msi_summary$year, " = ", ipeds_msi_summary$total_institutions, collapse = ", ")
)

# Plot
msi_eligibility_plot <- ggplot(ipeds_msi_summary, aes(x = year)) +
  geom_col(aes(y = eligible_institutions), fill = "skyblue") +
  geom_text(aes(y = eligible_institutions - 100, label = eligible_institutions), color = "black", size = 3.5) +
  geom_line(aes(y = percent_eligible * scale_factor), group = 1, color = "orange", size = 1.2) +
  geom_point(aes(y = percent_eligible * scale_factor), color = "orange", size = 2) +
  geom_text(data = ipeds_msi_summary %>% filter(year == "2022" & !is.na(jump_2022_count)),
            aes(y = eligible_institutions + 50, label = paste0("Jump from 2021 to 2022: ", jump_2022_count)),
            color = "gray30", vjust = -0.5) +
  scale_y_continuous(
    name = "Eligible Institutions",
    sec.axis = sec_axis(~ . / scale_factor, name = "% Eligible")
  ) +
  labs(
    title = "MSI Eligibility Over Time",
    x = "Year",
    caption = caption_text
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "skyblue"),
    axis.title.y.right = element_text(color = "orange"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save the plot as a PNG file
ggsave("../msi_eligibility_plot.png", plot = msi_eligibility_plot, width = 10, height = 8, dpi = 300)


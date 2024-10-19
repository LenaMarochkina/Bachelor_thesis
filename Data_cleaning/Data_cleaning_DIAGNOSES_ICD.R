library(dplyr)
library(tidyverse)
library(lubridate)

# Read diagnoses from csv
diagnoses <- read.csv("data/raw/DIAGNOSES_ICD.csv", stringsAsFactors = TRUE)

# Read diagnoses codes from csv
diagnoses_codes <- read.csv("data/raw/D_ICD_DIAGNOSES.csv", stringsAsFactors = TRUE)

# Find rows where ICD9_CODE is NA in diagnoses_codes
na_codes <- diagnoses_codes %>%
  filter(is.na(ICD9_CODE)) %>%
  distinct(ICD9_CODE)

# Normalize the columns by converting everything to uppercase
diagnoses <- diagnoses %>%
  mutate(across(where(is.factor), function(x) toupper(x) %>% as.factor()))

# Replace empty spaces with NA
diagnoses_NA <- diagnoses %>%
  mutate(across(where(is.factor), function(x) as.character(x))) %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%
  mutate(across(where(is.character), function(x) as.factor(x)))

# Remove exact duplicate rows
diagnoses_clean <- diagnoses_NA %>%
  distinct()

# Check for invalid ICD9_CODE (i.e., codes not present in d_icd_diagnoses)
invalid_codes <- diagnoses_clean %>%
  anti_join(diagnoses_codes, by = "ICD9_CODE")

# Get the distinct invalid ICD9 codes along with the count of patients for each code
distinct_invalid_codes_with_count <- invalid_codes %>%
  group_by(ICD9_CODE) %>%
  summarise(patient_count = n_distinct(SUBJECT_ID))
 
# Filter ICD9 codes with more than 400 patients (1 % of total number of patients)
significant_codes <- distinct_invalid_codes_with_count %>%
  filter(patient_count > 400)

# Add missing significant ICD9 codes to the diagnoses_codes table
# Add them with NA for SHORT_TITLE and LONG_TITLE initially
diagnoses_codes <- diagnoses_codes %>%
  bind_rows(
    significant_codes %>%
      mutate(SHORT_TITLE = NA_character_, LONG_TITLE = NA_character_)  # Initialize missing titles with NA
  )

# Update the SHORT_TITLE column for significant ICD9 codes (newly added)
diagnoses_codes <- diagnoses_codes %>%
  mutate(SHORT_TITLE = case_when(
    ICD9_CODE == "0414" ~ "Escherichia coli [e. coli] infection in conditions classified elsewhere and of unspecified site",
    ICD9_CODE == "2765" ~ "Volume depletion disorder",
    ICD9_CODE == "2766" ~ "Fluid overload disorder",
    ICD9_CODE == "2841" ~ "Pancytopenia",
    ICD9_CODE == "2874" ~ "Secondary thrombocytopenia",
    ICD9_CODE == "4538" ~ "Acute venous embolism and thrombosis of other specified veins",
    ICD9_CODE == "5185" ~ "Pulmonary insufficiency following trauma and surgery",
    ICD9_CODE == "5997" ~ "Hematuria",
    ICD9_CODE == "7793" ~ "Feeding problems in newborn",
    ICD9_CODE == "7806" ~ "Fever and other physiologic disturbances of temperature regulation",
    ICD9_CODE == "7895" ~ "Ascites",
    ICD9_CODE == "9974" ~ "Digestive system complications not elsewhere classified",
    TRUE ~ SHORT_TITLE  # Keep existing short titles for non-significant codes
  ))
  
# Perform a left join to add SHORT_TITLE and LONG_TITLE from d_icd_diagnoses to diagnoses_icd
diagnoses_clean <- diagnoses_clean %>%
  left_join(diagnoses_codes %>% select(ICD9_CODE, SHORT_TITLE, LONG_TITLE), by = "ICD9_CODE")

# Replace other empty or NA values in SHORT_TITLE and LONG_TITLE with "Unknown"
diagnoses_clean <- diagnoses_clean %>%
  mutate(
    SHORT_TITLE = ifelse(is.na(SHORT_TITLE) | SHORT_TITLE == "", "Unknown", SHORT_TITLE),
    LONG_TITLE = ifelse(is.na(LONG_TITLE) | LONG_TITLE == "", "Unknown", LONG_TITLE)
  )

# Calculate the number of diagnoses per patient (SUBJECT_ID)
patient_diagnosis_count <- diagnoses_clean %>%
  group_by(SUBJECT_ID) %>%
  summarise(DIAGNOSES_NUM = n_distinct(SEQ_NUM, na.rm = TRUE))

#  Add the DIAGNOSES_NUM column to the diagnoses_clean table by joining the count
diagnoses_clean <- diagnoses_clean %>%
  left_join(patient_diagnosis_count, by = "SUBJECT_ID")

diagnoses_clean <- diagnoses_clean %>%
  relocate(DIAGNOSES_NUM, .after = SEQ_NUM)

# Write cleaned diagnoses data to csv
write.csv(diagnoses_clean, "data/raw/cleaned/DIAGNOSES_ICD_clean.csv")
  

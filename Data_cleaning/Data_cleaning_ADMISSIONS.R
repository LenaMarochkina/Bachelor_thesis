library(dplyr)
library(tidyverse)

# Read admissions from csv
admissions <- read.csv("data/raw/ADMISSIONS.csv", stringsAsFactors = TRUE)

# Select necessary columns
admissions <- admissions %>%
  select(SUBJECT_ID, HADM_ID, ADMITTIME, DISCHTIME, EDREGTIME, EDOUTTIME, DIAGNOSIS, DISCHARGE_LOCATION, DEATHTIME, ADMISSION_TYPE, INSURANCE, RELIGION, MARITAL_STATUS, ETHNICITY, HOSPITAL_EXPIRE_FLAG)

# Normalize the columns by converting everything to uppercase
admissions <- admissions %>%
  mutate(across(where(is.factor), function(x) toupper(x) %>% as.factor()))

# Replace empty spaces with NA
admissions_NA <- admissions %>%
  mutate(across(where(is.factor), function(x) as.character(x))) %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%
  mutate(across(where(is.character), function(x) as.factor(x)))

# Remove exact duplicate rows
admissions_clean <- admissions_NA %>%
  distinct()

# Impute missing categorical variables
admissions_clean <- admissions_clean %>%
  mutate(RELIGION = as.factor(replace_na(as.character(RELIGION), "UNKNOWN")),
         INSURANCE = as.factor(replace_na(as.character(INSURANCE), "UNKNOWN")),
         MARITAL_STATUS = as.factor(replace_na(as.character(MARITAL_STATUS), "UNKNOWN")),
         ETHNICITY = as.factor(replace_na(as.character(ETHNICITY), "UNKNOWN")))

# Change datetime format
admissions_clean <-  admissions_clean %>%
  mutate(ADMITTIME = ymd_hms(ADMITTIME),
         DISCHTIME = ymd_hms(DISCHTIME),
         DEATHTIME = ymd_hms(DEATHTIME),
         EDREGTIME = ymd_hms(EDREGTIME),
         EDOUTTIME = ymd_hms(EDOUTTIME))

# Check if all times have valid hours (0-23), minutes (0-59), and seconds (0-59)
time_validation <- admissions_clean %>%
  mutate(valid_time = 
           is.na(ADMITTIME) | hour(ADMITTIME) %in% 0:23 & minute(ADMITTIME) %in% 0:59 & second(ADMITTIME) %in% 0:59 &
           is.na(DISCHTIME) | hour(DISCHTIME) %in% 0:23 & minute(DISCHTIME) %in% 0:59 & second(DISCHTIME) %in% 0:59 &
           is.na(DEATHTIME) | hour(DEATHTIME) %in% 0:23 & minute(DEATHTIME) %in% 0:59 & second(DEATHTIME) %in% 0:59 &
           is.na(EDREGTIME) | hour(EDREGTIME) %in% 0:23 & minute(EDREGTIME) %in% 0:59 & second(EDREGTIME) %in% 0:59 &
           is.na(EDOUTTIME) | hour(EDOUTTIME) %in% 0:23 & minute(EDOUTTIME) %in% 0:59 & second(EDOUTTIME) %in% 0:59)

# Filter rows with invalid times (if any)
admissions_clean <- time_validation %>%
  filter(valid_time == TRUE) %>%
  mutate(valid_time = NULL)

# Check Admission ID
admissions_clean <- admissions_clean %>%
  filter(HADM_ID %in% 100000:199999)

# Check ADMISSION_TYPE
admissions_clean <- admissions_clean %>%
  filter(ADMISSION_TYPE %in% c("ELECTIVE", "URGENT", "NEWBORN", "EMERGENCY"))

# Check uniwue values
unique_insurance <- admissions_clean %>%
  distinct(INSURANCE)

unique_religion <- admissions_clean %>%
  distinct(RELIGION)

unique_marital_status <- admissions_clean %>%
  distinct(MARITAL_STATUS)

unique_ethnicity <- admissions_clean %>%
  distinct(ETHNICITY)

unique_ethnicity %>% View()
unique_insurance %>% View()
unique_marital_status %>% View()
unique_religion %>% View()

# Merge Unknow marital status
admissions_clean <- admissions_clean %>%
  mutate(
    MARITAL_STATUS = case_when(
      MARITAL_STATUS %in% c("UNKNOWN", "UNKNOWN (DEFAULT)") ~ "UNKNOWN",  # Merge these categories into "UNKNOWN"
      TRUE ~ as.character(MARITAL_STATUS)  # Keep original values for all other cases
    )
  )
# Create a new column for Ethnicity categories (White, Black, Asian)
admissions_clean <- admissions_clean %>%
  mutate(
    ETHNICITY = case_when(
      str_detect(ETHNICITY, "WHITE|MIDDLE EASTERN|AMERICAN INDIAN|PORTUGUESE|CARIBBEAN|HISPANIC|LATINO") ~ "WHITE",
      str_detect(ETHNICITY, "BLACK|AFRICAN|CAPE VERDEAN|HAITIAN") ~ "BLACK",
      str_detect(ETHNICITY, "ASIAN") ~ "ASIAN",
      str_detect(ETHNICITY, "UNKNOWN|UNABLE TO OBTAIN|PATIENT DECLINED") ~ "UNKNOWN",  # Check for keywords
      TRUE ~ "OTHER" # For all other ethnicities
    ),
    ETHNICITY = as.factor(ETHNICITY)
  )

# Create a new column for Religion categories (CHRISTIANITY, JUDAISM, ISLAM, HINDUISM, BUDDHISM, OTHER, UNKNOWN)
admissions_clean <- admissions_clean %>%
  mutate(
    RELIGION = case_when(
      str_detect(RELIGION, "CATHOLIC|PROTESTANT|BAPTIST|METHODIST|7TH DAY ADVENTIST|UNITARIAN-UNIVERSALIST|EPISCOPALIAN|JEHOVAH|LUTHERAN|CHRISTIAN SCIENTIST|GREEK ORTHODOX|ROMANIAN EAST. ORTH") ~ "CHRISTIANITY",
      str_detect(RELIGION, "JEWISH|HEBREW") ~ "JUDAISM",
      str_detect(RELIGION, "MUSLIM") ~ "OTHER",
      str_detect(RELIGION, "HINDU") ~ "OTHER",
      str_detect(RELIGION, "BUDDHIST") ~ "OTHER",
      RELIGION %in% c("OTHER") ~ "OTHER",
      RELIGION %in% c("UNKNOWN", "UNOBTAINABLE", "NOT SPECIFIED", NA) ~ "UNKNOWN",
      TRUE ~ "OTHER"
    ),
    RELIGION = as.factor(toupper(RELIGION)) # Convert RELIGION_GROUP to a factor and ensure uppercase
  )

# Create a new column to check if all conditions are met for expired patients
check_expired <- admissions_clean %>%
  mutate(
    validity_check = case_when(
      # Check for flag = true with DEATHTIME as NA or DISCHARGE_LOCATION not equal to "DEAD/EXPIRED"
      HOSPITAL_EXPIRE_FLAG == 1 & (is.na(DEATHTIME) | DISCHARGE_LOCATION != "DEAD/EXPIRED") ~ "Invalid",
      
      # Check for flag = false with DEATHTIME not NA or DISCHARGE_LOCATION equal to "DEAD/EXPIRED"
      HOSPITAL_EXPIRE_FLAG == 0 & (!is.na(DEATHTIME) | DISCHARGE_LOCATION == "DEAD/EXPIRED") ~ "Invalid",
      
      # If no issues, mark as valid
      TRUE ~ "Valid"
    )
  ) %>%
  filter(validity_check == FALSE) %>%
  select(SUBJECT_ID, validity_check)

# Check for multiple DEATHTIME entries per SUBJECT_ID
subject_death_check <- admissions_clean %>%
  select(SUBJECT_ID, DEATHTIME, DIAGNOSIS) %>%
  filter(!is.na(DEATHTIME)) %>%   # Exclude rows where DEATHTIME is NA
  group_by(SUBJECT_ID) %>%
  filter(n_distinct(DEATHTIME) > 1) %>%  # Keep only SUBJECT_IDs with more than one distinct DEATHTIME
  arrange(SUBJECT_ID, DEATHTIME, DIAGNOSIS)

# Check association with 2nd death time and diagnosis DONOR
subject_is_donor <- subject_death_check %>%
  group_by(SUBJECT_ID) %>%
  summarise(has_organ_donor_diagnosis = any(DIAGNOSIS %in% c("ORGAN DONOR ACCOUNT", "ORGAN DONOR", "DONOR ACCOUNT")))

if (all(subject_is_donor$has_organ_donor_diagnosis)) {
  print("All patients with 2 death times were donors.")
} else {
  print("Not all patients with 2 death times were donors.")
}

# Create a list of SUBJECT_IDs that have more than one DEATHTIME
subject_ids_with_multiple_deaths <- unique(subject_death_check$SUBJECT_ID)

# Filter the original data to remove only the second (and later) DEATHTIME for these SUBJECT_IDs
admissions_clean <- admissions_clean %>%
  group_by(SUBJECT_ID) %>%
  filter(
    !(
      SUBJECT_ID %in% subject_ids_with_multiple_deaths & 
        DEATHTIME != min(DEATHTIME, na.rm = TRUE)
    )
  ) %>%
  ungroup()

# Create the SURVIVAL variable in days for each unique SUBJECT_ID and HADM_ID
admissions_clean <- admissions_clean %>%
  arrange(SUBJECT_ID, HADM_ID) %>%
  mutate(
    SURVIVAL = ifelse(
      !is.na(DEATHTIME),
      as.numeric(difftime(DEATHTIME, ADMITTIME, units = "days")),
      as.numeric(difftime(DISCHTIME, ADMITTIME, units = "days"))
    ),
    # Recalculate SURVIVAL if it's negative
    SURVIVAL = ifelse(SURVIVAL < 0, abs(SURVIVAL), SURVIVAL)
  )

# Ensure no duplicates after merging
admissions_clean <- admissions_clean %>%
  distinct(SUBJECT_ID, HADM_ID, .keep_all = TRUE)

# Choose necessary columns after checking all data
admissions_clean <- admissions_clean %>%
  select(SUBJECT_ID, HADM_ID, ADMITTIME, DISCHTIME, DEATHTIME, ADMISSION_TYPE, INSURANCE, RELIGION, MARITAL_STATUS, ETHNICITY, SURVIVAL)

# Write cleaned admissions to csv
write.csv(admissions_clean, "data/raw/cleaned/ADMISSIONS_clean.csv", row.names = FALSE)

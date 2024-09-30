# Read patients from csv
patients <- read.csv("data/raw/patients.csv", stringsAsFactors = TRUE)

# Read admissions from csv
admissions <- read.csv("data/raw/cleaned/ADMISSIONS_clean.csv", stringsAsFactors = TRUE)

# Normalize the columns by converting everything to uppercase
patients <- patients %>%
  mutate(across(where(is.factor), function(x) toupper(x) %>% as.factor()))

# Replace empty spaces with NA
patients_NA <- patients %>%
  mutate(across(where(is.factor), function(x) as.character(x))) %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%
  mutate(across(where(is.character), function(x) as.factor(x)))

# Remove exact duplicate rows
patients_clean <- patients_NA %>%
  distinct()

# Convert TIMESTAMP columns to datetime format
patients_clean <- patients_clean %>%
  mutate(
    DOB = as.POSIXct(DOB, format = "%Y-%m-%d %H:%M:%S"),
    DOD = as.POSIXct(DOD, format = "%Y-%m-%d %H:%M:%S"),
    DOD_HOSP = as.POSIXct(DOD_HOSP, format = "%Y-%m-%d %H:%M:%S"),
    DOD_SSN = as.POSIXct(DOD_SSN, format = "%Y-%m-%d %H:%M:%S")
  )

admissions <- admissions %>%
  mutate(ADMITTIME = as.POSIXct(ADMITTIME, format = "%Y-%m-%d %H:%M:%S")) 

# Check for inconsistencies: EXPIRE_FLAG = 1 must have valid DOD_SSN, EXPIRE_FLAG = 0 must not
patients_clean <- patients_clean %>%
  mutate(
    death_record_exists = !is.na(DOD)
  ) %>%
  filter(
    (EXPIRE_FLAG == 1 & death_record_exists) |  # EXPIRE_FLAG is 1, and there is a death record
      (EXPIRE_FLAG == 0 & !death_record_exists)  # EXPIRE_FLAG is 0, and there is no death record
  ) %>%
  select(-death_record_exists)  # Remove temporary column

# Filter admission data to choose distinct rows with certain columns 
admissions <- admissions %>%
  select(SUBJECT_ID, ADMITTIME) %>%
  distinct()

# Perform a left join to bring in ADMITTIME from the admissions table and ensure the result is distinct
patients_clean <- patients_clean %>%
  left_join(admissions, 
            by = c("SUBJECT_ID")) %>%
  distinct(SUBJECT_ID, .keep_all = TRUE)  # Remove any duplicates based on SUBJECT_ID and HADM_ID

# Calculate age at ICU admission in years and round to nearest whole number
patients_clean <- patients_clean %>%
  mutate(AGE_AT_ADMISSION = round(as.numeric(difftime(ADMITTIME, DOB, units = "days")) / 365.25))

# Calculate age at death for deceased patients in years and round to nearest whole number
patients_clean <- patients_clean %>%
  mutate(AGE_AT_DEATH = round(as.numeric(difftime(DOD, DOB, units = "days")) / 365.25))

# Define a function to calculate the adjusted age
adjust_age <- function(age) {
  if (is.na(age)) {
    return(NA_real_)  # Return NA if the value is missing
  } else if (age >= 300) {
    return((age * 90) / 300)  # Apply the formula for all ages greater than 300
  } else {
    return(age)  # Keep the original age if less than 300
  }
}

# Define a function to check if the age is within a normal range (e.g., 0 to 120 years)
check_age_range <- function(age, min_age = 0, max_age = 120) {
  if (is.na(age)) {
    return(TRUE)  # Return TRUE for missing age values
  } else if (age >= min_age & age <= max_age) {
    return(TRUE)  # Return TRUE if age is within the normal range
  } else {
    return(FALSE)  # Return FALSE if age is outside the normal range
  }
}

patients_clean <- patients_clean %>%
  mutate(
    AGE_AT_ADMISSION = sapply(AGE_AT_ADMISSION, adjust_age),  # Apply the adjust_age function
    AGE_AT_DEATH = sapply(AGE_AT_ADMISSION, adjust_age)
  )

# Round the age to the nearest whole number
patients_clean <- patients_clean %>%
  mutate(AGE_AT_ADMISSION = round(AGE_AT_ADMISSION),
         AGE_AT_DEATH = round(AGE_AT_DEATH))

# Example usage: Filter patients with age in the normal range without adding a new column
patients_with_abnormal_ages <- patients_clean %>%
  filter(!sapply(AGE_AT_ADMISSION, check_age_range), !sapply(AGE_AT_DEATH, check_age_range))

patients_clean <- patients_clean %>%
  select(-DOD_SSN, -DOD_HOSP, -ADMITTIME, -EXPIRE_FLAG)

# Write cleaned patients data to csv
write.csv(patients_clean, "data/raw/cleaned/PATIENTS_clean.csv")



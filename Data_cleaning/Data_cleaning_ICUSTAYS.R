# Read icu_stays from csv
icu_stays <- read.csv("data/raw/ICUSTAYS.csv", stringsAsFactors = TRUE)

# Read admissions from csv
admissions <- read.csv("data/raw/cleaned/ADMISSIONS_clean.csv", stringsAsFactors = TRUE)

# Normalize the columns by converting everything to uppercase
icu_stays <- icu_stays %>%
  mutate(across(where(is.factor), function(x) toupper(x) %>% as.factor()))

# Replace empty spaces with NA
icu_stays_NA <- icu_stays %>%
  mutate(across(where(is.factor), function(x) as.character(x))) %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%
  mutate(across(where(is.character), function(x) as.factor(x)))

# Remove exact duplicate rows
icu_stays_clean <- icu_stays_NA %>%
  distinct()

# Change datetime format
# Convert time columns to proper datetime format using lubridate's ymd_hms
icu_stays_clean <- icu_stays_clean %>%
  mutate((INTIME = ymd_hms(INTIME)),
         (OUTTIME = ymd_hms(OUTTIME))) 

# Check if all times have valid hours (0-23), minutes (0-59), and seconds (0-59)
time_validation <- icu_stays_clean %>%
  mutate(valid_time = 
           (is.na(INTIME) | (hour(INTIME) %in% 0:23 & minute(INTIME) %in% 0:59 & second(INTIME) %in% 0:59)),
           (is.na(OUTTIME) | (hour(OUTTIME) %in% 0:23 & minute(OUTTIME) %in% 0:59 & second(OUTTIME) %in% 0:59)))

# View rows with invalid time
invalid_time_rows <- time_validation %>%
  filter(valid_time == FALSE)

# Select only necessary columns
icu_stays_clean <- icu_stays_clean %>%
  select(SUBJECT_ID, HADM_ID, ICUSTAY_ID, INTIME, OUTTIME, LOS)

# Write cleaned icu stays to csv
write.csv(icu_stays_clean, "data/raw/cleaned/ICUSTAYS_clean.csv")

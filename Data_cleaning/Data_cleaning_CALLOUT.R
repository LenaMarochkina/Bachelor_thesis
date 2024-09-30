# Read callout from csv
callout <- read.csv("data/raw/CALLOUT.csv", stringsAsFactors = TRUE)

# Choose necessary columns
callout <- callout %>%
  select(ROW_ID, SUBJECT_ID, CALLOUT_OUTCOME, OUTCOMETIME, HADM_ID, REQUEST_TELE, REQUEST_RESP, REQUEST_CDIFF, REQUEST_MRSA, REQUEST_VRE)

# Normalize the columns by converting everything to uppercase
callout <- callout %>%
  mutate(across(where(is.factor), function(x) toupper(x) %>% as.factor()))

# Replace empty spaces with NA
callout_NA <- callout %>%
  mutate(across(where(is.factor), function(x) as.character(x))) %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%
  mutate(across(where(is.character), function(x) as.factor(x)))

# Remove exact duplicate rows
callout_clean <- callout_NA %>%
  distinct()

# Change datetime format
# Convert time columns to proper datetime format using lubridate's ymd_hms
callout_clean <- callout_clean %>%
  mutate(OUTCOMETIME = ymd_hms(OUTCOMETIME)) 

# Check if all times have valid hours (0-23), minutes (0-59), and seconds (0-59)
time_validation <- callout_clean %>%
  mutate(valid_time = 
           (is.na(OUTCOMETIME) | (hour(OUTCOMETIME) %in% 0:23 & minute(OUTCOMETIME) %in% 0:59 & second(OUTCOMETIME) %in% 0:59)))

# View rows with invalid time
invalid_time_rows <- time_validation %>%
  filter(valid_time == FALSE)

# Check CALLOUT_OUTCOME vs OUTCOMETIME consistency
# Check if the status is OK
inconsistent_status <- callout_clean %>%
  filter(CALLOUT_OUTCOME %in% c('Discharged', 'Cancelled'))

# Identify rows where CALLOUT_OUTCOME is 'Discharged' but OUTCOMETIME is NA
inconsistent_discharge <- callout_clean %>%
  filter(CALLOUT_OUTCOME == 'Discharged' & is.na(OUTCOMETIME))

# Identify rows where CALLOUT_OUTCOME is 'Cancelled' but OUTCOMETIME is not NA
inconsistent_cancel <- callout_clean %>%
  filter(CALLOUT_OUTCOME == 'Cancelled' & !is.na(OUTCOMETIME))

# Combine both sets of inconsistent rows
inconsistent_rows <- bind_rows(inconsistent_discharge, inconsistent_cancel, inconsistent_status)

# Check if any inconsistent rows exist
if (nrow(inconsistent_rows) > 0) {
  print("Inconsistent rows found:")
  print(inconsistent_rows)
} else {
  print("All rows have consistent CALLOUT_OUTCOME and OUTCOMETIME values.")
}

# Write cleaned admissions to csv
write.csv(callout_clean, "data/raw/cleaned/CALLOUT_clean.csv")

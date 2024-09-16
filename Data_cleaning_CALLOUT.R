# Read callout from csv
callout <- read.csv("data/raw/CALLOUT.csv", stringsAsFactors = TRUE)

callout %>% View()

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
  mutate(CREATETIME = ymd_hms(CREATETIME),
         UPDATETIME = ymd_hms(UPDATETIME),
         ACKNOWLEDGETIME = ymd_hms(ACKNOWLEDGETIME),
         OUTCOMETIME = ymd_hms(OUTCOMETIME),
         FIRSTRESERVATIONTIME = ymd_hms(FIRSTRESERVATIONTIME),
         CURRENTRESERVATIONTIME = ymd_hms(CURRENTRESERVATIONTIME))

# Check if all times have valid hours (0-23), minutes (0-59), and seconds (0-59)
time_validation <- callout_clean %>%
  mutate(valid_time = 
           (is.na(CREATETIME) | (hour(CREATETIME) %in% 0:23 & minute(CREATETIME) %in% 0:59 & second(CREATETIME) %in% 0:59)) &
           (is.na(UPDATETIME) | (hour(UPDATETIME) %in% 0:23 & minute(UPDATETIME) %in% 0:59 & second(UPDATETIME) %in% 0:59)) &
           (is.na(ACKNOWLEDGETIME) | (hour(ACKNOWLEDGETIME) %in% 0:23 & minute(ACKNOWLEDGETIME) %in% 0:59 & second(ACKNOWLEDGETIME) %in% 0:59)) &
           (is.na(OUTCOMETIME) | (hour(OUTCOMETIME) %in% 0:23 & minute(OUTCOMETIME) %in% 0:59 & second(OUTCOMETIME) %in% 0:59)) &
           (is.na(FIRSTRESERVATIONTIME) | (hour(FIRSTRESERVATIONTIME) %in% 0:23 & minute(FIRSTRESERVATIONTIME) %in% 0:59 & second(FIRSTRESERVATIONTIME) %in% 0:59)) &
           (is.na(CURRENTRESERVATIONTIME) | (hour(CURRENTRESERVATIONTIME) %in% 0:23 & minute(CURRENTRESERVATIONTIME) %in% 0:59 & second(CURRENTRESERVATIONTIME) %in% 0:59))
  )

# View rows with invalid time
invalid_time_rows <- time_validation %>%
  filter(valid_time == FALSE)

# Check for invalid values in binary columns (REQUEST_TELE, REQUEST_RESP, REQUEST_CDIFF, REQUEST_MRSA, REQUEST_VRE)
invalid_values <- callout_clean %>%
  filter(
    !REQUEST_TELE %in% c(0, 1) |
      !REQUEST_RESP %in% c(0, 1) |
      !REQUEST_CDIFF %in% c(0, 1) |
      !REQUEST_MRSA %in% c(0, 1) |
      !REQUEST_VRE %in% c(0, 1)
  )

# Check if there are any invalid values
if (nrow(invalid_values) > 0) {
  print("Invalid values found in binary columns:")
  print(invalid_values)
} else {
  print("All binary columns are valid (only 0/1 values present).")
}

# Check CALLOUT_OUTCOME vs OUTCOMETIME consistency
# Identify rows where CALLOUT_OUTCOME is 'Discharged' but OUTCOMETIME is NA
inconsistent_discharge <- callout_clean %>%
  filter(CALLOUT_OUTCOME == 'Discharged' & is.na(OUTCOMETIME))

# Identify rows where CALLOUT_OUTCOME is 'Cancelled' but OUTCOMETIME is not NA
inconsistent_cancel <- callout_clean %>%
  filter(CALLOUT_OUTCOME == 'Cancelled' & !is.na(OUTCOMETIME))

# Combine both sets of inconsistent rows
inconsistent_rows <- bind_rows(inconsistent_discharge, inconsistent_cancel)

# Check if any inconsistent rows exist
if (nrow(inconsistent_rows) > 0) {
  print("Inconsistent rows found:")
  print(inconsistent_rows)
} else {
  print("All rows have consistent CALLOUT_OUTCOME and OUTCOMETIME values.")
}

# Identify rows where ACKNOWLEDGE_STATUS is 'Acknowledged' but ACKNOWLEDGETIME is NA
inconsistent_acknowledge <- callout_clean %>%
  filter(ACKNOWLEDGE_STATUS == 'Acknowledged' & is.na(ACKNOWLEDGETIME))

# Check if any inconsistent rows exist
if (nrow(inconsistent_acknowledge) > 0) {
  print("Inconsistent rows where ACKNOWLEDGE_STATUS is 'Acknowledged' but ACKNOWLEDGETIME is NA:")
  print(inconsistent_acknowledge)
} else {
  print("All rows have consistent ACKNOWLEDGE_STATUS and ACKNOWLEDGETIME values.")
}

# Example join with PATIENTS and ADMISSIONS
callout_clean <- callout_clean %>%
  left_join(patients_callout, by = "SUBJECT_ID") %>%
  left_join(admissions_callout, by = "HADM_ID")

# Write cleaned admissions to xslx
write_xlsx(callout_clean, "data/raw/CALLOUT_clean.xlsx")

# Read data from csv
data <- read.csv("data/raw/ADMISSIONS.csv", stringsAsFactors = TRUE)
data %>% View()

# Replace empty spaces with NA
data_NA <- data %>%
  mutate(across(where(is.factor), function(x) as.character(x))) %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%
  mutate(across(where(is.character), function(x) as.factor(x)))

# Remove exact duplicate rows
data_clean <- data_NA %>%
  distinct()

# Change datetime format
data_clean <-  data_clean %>%
  mutate(ADMITTIME = ymd_hms(ADMITTIME),
         DISCHTIME = ymd_hms(DISCHTIME),
         DEATHTIME = ymd_hms(DEATHTIME),
         EDREGTIME = ymd_hms(EDREGTIME),
         EDOUTTIME = ymd_hms(EDREGTIME))

# Check for multiple DEATHTIME entries per SUBJECT_ID
subject_death_check <- data_clean %>%
  select(SUBJECT_ID, DEATHTIME, DIAGNOSIS) %>%
  filter(!is.na(DEATHTIME)) %>%   # Exclude rows where DEATHTIME is NA
  group_by(SUBJECT_ID) %>%
  filter(n_distinct(DEATHTIME) > 1) %>%  # Keep only SUBJECT_IDs with more than one distinct DEATHTIME
  arrange(SUBJECT_ID, DEATHTIME, DIAGNOSIS)

# View results
subject_death_check %>% View()

# Check association with 2nd death time and diagnosis DONOR
subject_is_donor <- subject_death_check %>%
  group_by(SUBJECT_ID) %>%
  summarise(has_organ_donor_diagnosis = any(DIAGNOSIS %in% c("ORGAN DONOR ACCOUNT", "ORGAN DONOR", "DONOR ACCOUNT")))

if (all(subject_is_donor$has_organ_donor_diagnosis)) {
  print("All patients with 2 death times were donors.")
} else {
  print("Not all patients with 2 death times were donors.")
}

# Create the TIMETODEATH variable
data_with_timetodeath <- data_clean %>%
  group_by(SUBJECT_ID) %>%
  filter(!is.na(DEATHTIME)) %>%
  arrange(DEATHTIME) %>%
  slice(1) %>%  
  mutate(TIMETODEATH = difftime(DEATHTIME, ADMITTIME, units = "secs")) %>%
  mutate(acceptable_negative_timetodeath = ifelse(TIMETODEATH < 0 & (EDREGTIME == EDOUTTIME | (is.na(EDREGTIME) & is.na(EDOUTTIME))), TRUE, NA)) %>%  # Create flag only for negative TIMETODEATH and when EDREGTIME == EDOUTTIME
  mutate(TIMETODEATH = sprintf("%02d:%02d:%02d", 
                               as.integer(TIMETODEATH) %/% 3600,           # Hours
                               (as.integer(TIMETODEATH) %% 3600) %/% 60,   # Minutes
                               as.integer(TIMETODEATH) %% 60))             # Seconds

data_clean <- data_with_timetodeath %>%
  arrange(SUBJECT_ID, DEATHTIME) %>%
  bind_rows(data_clean) %>%
  arrange(SUBJECT_ID, DEATHTIME) %>%
  filter(is.na(acceptable_negative_timetodeath)) %>%
  mutate(acceptable_negative_timetodeath = NULL)

data_clean %>% View()

# Check for missing values
missing_values <- data_clean %>%
  summarise_all(~ sum(is.na(.)))
print(missing_values)

# Write cleaned data to xslx
write_xlsx(data_clean, "data/raw/ADMISSIONS_clean.xlsx")

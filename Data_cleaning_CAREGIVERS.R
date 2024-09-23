# Read admissions from csv
caregivers <- read.csv("data/raw/CAREGIVERS.csv", stringsAsFactors = TRUE)
caregivers %>% View()

# Normalize the columns by converting everything to uppercase
caregivers <- caregivers %>%
  mutate(across(where(is.factor), function(x) toupper(x) %>% as.factor()))

# Replace empty spaces with NA
caregivers_NA <- caregivers %>%
  mutate(across(where(is.factor), function(x) as.character(x))) %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%
  mutate(across(where(is.character), function(x) as.factor(x)))

# Remove exact duplicate rows
caregivers_clean <- caregivers_NA %>%
  distinct()

# Validate that CGID is unique
# CGID should be a unique identifier for each caregiver, so we can check for duplicates
cgid_duplicates <- caregivers_clean %>%
  group_by(CGID) %>%
  filter(n() > 1)

if(nrow(cgid_duplicates) > 0){
  print("Warning: Duplicate CGID found. Check for possible inconsistencies.")
} else {
  print("All CGID values are unique.")
}

# Write cleaned admissions to xslx
write.csv(caregivers_clean, "data/raw/ADMISSIONS_clean.csv")
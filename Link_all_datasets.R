# Read ADMISSIONS from xslx
admissions_clean <- read_excel("data/raw/ADMISSIONS_clean.xlsx")

# Read CALLOUT from xslx
callout_clean <- read_excel("data/raw/CALLOUT_clean.xlsx")

# Check if joining the tables is ok by SUBJECT_ID or HADM_ID
# First, try joining by SUBJECT_ID and HADM_ID
linked_data <- callout_clean %>%
  left_join(admissions_clean, by = c("SUBJECT_ID", "HADM_ID"))

# Check for inconsistencies (rows in CALLOUT that don't have matching ADMISSIONS)
inconsistent_rows <- linked_data %>%
  filter(is.na(ADMITTIME))  # Assuming ADMITTIME is a column in ADMISSIONS_clean

# Check if any inconsistencies are found
if (nrow(inconsistent_rows) > 0) {
  print("Inconsistent rows found:")
  print(inconsistent_rows)
} else {
  print("All rows have consistent matches between ADMISSIONS and CALLOUT.")
}
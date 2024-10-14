library(dplyr)
library(tidyverse)
library(lubridate)

# Read procedure events from csv
microbiology <- read.csv("data/raw/MICROBIOLOGYEVENTS.csv", stringsAsFactors = TRUE)

# Select necessary columns
microbiology <- microbiology %>%
  select(SUBJECT_ID, HADM_ID, CHARTTIME, SPEC_TYPE_DESC, ORG_NAME, AB_NAME, INTERPRETATION)

# Normalize the columns by converting everything to uppercase
microbiology <- microbiology %>%
  mutate(across(where(is.factor), function(x) toupper(x) %>% as.factor()))

# Replace empty spaces with NA
microbiology_NA <- microbiology %>%
  mutate(across(where(is.factor), function(x) as.character(x))) %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%
  mutate(across(where(is.character), function(x) as.factor(x)))


# Remove exact duplicate rows and 
microbiology_clean <- microbiology_NA %>%
  distinct() %>%
  filter(!is.na(ORG_NAME) & ORG_NAME != "2ND ISOLATE")

# Group by patient (SUBJECT_ID) and organism (ORG_NAME), then count how many antibiotics have INTERPRETATION == "R" (resistant)
resistance_summary <- microbiology_clean %>%
  filter(INTERPRETATION == "R") %>%  # Filter to only include resistant cases
  group_by(SUBJECT_ID, ORG_NAME) %>%  # Group by patient and organism
  summarise(RES_AB_COUNT = n_distinct(AB_NAME)) %>%  # Count unique antibiotics where the organism is resistant
  arrange(SUBJECT_ID, ORG_NAME)  # Optional: Sort the results

microbiology_clean <- microbiology_clean %>%
  left_join(resistance_summary, by = c("SUBJECT_ID", "ORG_NAME")) %>%
  mutate(RES_AB_COUNT = ifelse(is.na(RES_AB_COUNT), 0, RES_AB_COUNT))  # Replace NA with 0 where no resistance

# Write cleaned microbiology to csv
write.csv(microbiology_clean, "data/raw/cleaned/MICROBIOLOGY_clean.csv")

library(dplyr)
library(tidyverse)
library(lubridate)

# Read procedure events from csv
procedureevents <- read.csv("data/raw/PROCEDUREEVENTS_MV.csv", stringsAsFactors = TRUE)

# Read D_items from csv
items <- read.csv("data/raw/D_ITEMS.csv", stringsAsFactors = TRUE)

# Select necessary columns
procedureevents <- procedureevents %>%
  select(SUBJECT_ID, HADM_ID, ICUSTAY_ID, ITEMID, STARTTIME, ENDTIME, VALUE, VALUEUOM, 
         ORDERCATEGORYNAME, STATUSDESCRIPTION)

# Normalize the columns by converting everything to uppercase
procedureevents <- procedureevents %>%
  mutate(across(where(is.factor), function(x) toupper(x) %>% as.factor()))

# Normalize the columns by converting everything to uppercase
items <- items %>%
  mutate(across(where(is.factor), function(x) toupper(x) %>% as.factor()))

# Replace empty spaces with NA
procedureevents_NA <- procedureevents %>%
  mutate(across(where(is.factor), function(x) as.character(x))) %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%
  mutate(across(where(is.character), ~ na_if(., "NONE"))) %>% 
  mutate(across(where(is.character), function(x) as.factor(x)))

# Replace empty spaces with NA
items_NA <- items %>%
  mutate(across(where(is.factor), function(x) as.character(x))) %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%
  mutate(across(where(is.character), function(x) as.factor(x)))

# Remove exact duplicate rows
procedureevents_clean <- procedureevents_NA %>%
  distinct()

# Remove exact duplicate rows
items_clean <- items_NA %>%
  distinct()

# Change datetime format
procedureevents_clean <-  procedureevents_clean %>%
  mutate(STARTTIME = ymd_hms(STARTTIME),
         ENDTIME = ymd_hms(ENDTIME))

# Check if all times have valid hours (0-23), minutes (0-59), and seconds (0-59)
time_validation <- procedureevents_clean %>%
  mutate(valid_time = 
           is.na(STARTTIME) | hour(STARTTIME) %in% 0:23 & minute(STARTTIME) %in% 0:59 & second(STARTTIME) %in% 0:59 &
           is.na(ENDTIME) | hour(ENDTIME) %in% 0:23 & minute(ENDTIME) %in% 0:59 & second(ENDTIME) %in% 0:59)

# Filter rows with invalid times (if any)
procedureevents_clean <- time_validation %>%
  filter(valid_time == TRUE) %>%
  mutate(valid_time = NULL)

# Perform a left join to add the LABEL column from items_clean to procedureevents_clean
procedureevents_clean <- procedureevents_clean %>%
  left_join(items_clean %>% select(ITEMID, LABEL), by = "ITEMID")

# Relocate the LABEL column to be immediately after the ITEMID column
procedureevents_clean <- procedureevents_clean %>%
  relocate(LABEL, .after = ITEMID)

# Arrange the procedures by SUBJECT_ID, STARTTIME, DURATION, and ENDTIME to ensure a logical order
procedureevents_clean <- procedureevents_clean %>%
  arrange(SUBJECT_ID, STARTTIME, VALUE, ENDTIME)

# Create a sequential order of procedures for each patient using row_number()
procedureevents_clean <- procedureevents_clean %>%
  group_by(SUBJECT_ID) %>%
  mutate(SEQ_NUM = row_number()) %>%
  ungroup()

# Remove the STARTTIME and ENDTIME columns
procedureevents_clean <- procedureevents_clean %>%
  select(-STARTTIME, -ENDTIME)

# Write cleaned admissions to csv
write.csv(procedureevents_clean, "data/raw/cleaned/PROCEDURES_clean.csv")
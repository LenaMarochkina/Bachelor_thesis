library(dplyr)
library(tidyverse)

# Define only the important predictors
important_labels <- c(
  "Heart Rate",
  
  # Arterial Blood Pressure
  "Arterial Blood Pressure systolic",
  "Arterial Blood Pressure diastolic",
  "Arterial Blood Pressure mean",
  "Arterial BP [Systolic]",
  "Arterial BP [Diastolic]",
  "Arterial BP Mean",
  "ABP [Systolic]",
  "ABP [Diastolic]",
  "ART BP Systolic",
  "ART BP Diastolic",
  "ART BP mean",
  
  # Non-Invasive BP
  "Non Invasive Blood Pressure systolic",
  "Non Invasive Blood Pressure diastolic",
  "Non Invasive Blood Pressure mean",
  "NBP [Systolic]",
  "NBP [Diastolic]",
  "NBP Mean",
  
  # Manual BP
  "Manual BP [Systolic]",
  "Manual BP [Diastolic]",
  
  # BP by location
  "BP Right Leg [Systolic]",
  "BP Right Leg [Mean]",
  "BP Right Leg [Diastolic]",
  "BP Right Arm [Systolic]",
  "BP Right Arm [Mean]",
  "BP Right Arm [Diastolic]",
  "BP Left Leg [Systolic]",
  "BP Left Leg [Mean]",
  "BP Left Leg [Diastolic]",
  "BP Left Arm [Systolic]",
  "BP Left Arm [Mean]",
  "BP Left Arm [Diastolic]",
  "BP Cuff [Systolic]",
  "BP Cuff [Mean]",
  "BP Cuff [Diastolic]",
  
  # Temperature
  "Temperature Celsius",
  "Temperature Fahrenheit",
  "Skin Temperature",
  "Temp Rectal",
  "Temp Skin [C]",
  "Temperature C",
  "Temperature F",
  
  # Respiratory Rate
  "Respiratory Rate",
  "Resp Rate",
  "Resp Rate (Total)",
  "Resp. Rate",
  "Respiratory Rate (Total)",
  "Breath Rate",
  
  # Pupils
  "Pupil Response [Left]",
  "Pupil Response [Right]"
)


# Category is Routine Vital Signs
d_items <- read.csv("data/raw/D_ITEMS.csv", stringsAsFactors = FALSE)

# Extract ITEMIDs for the selected labels
important_itemids <- d_items %>%
  filter(LABEL %in% important_labels) %>%
  pull(ITEMID)

# Vector to collect filtered results
df_important_all <- list()

# Loop through 7 files
for (i in 1:7) {
  # Construct file name
  file_path <- sprintf("data/raw/chartevents_part_00%d.csv", i)
  
  # Read file
  df <- read.csv(file_path, stringsAsFactors = TRUE)
  
  # Filter and process
  df_filtered <- df %>%
    filter(ITEMID %in% important_itemids) %>%
    filter(ERROR == 0 & WARNING == 0) %>%
    select(-ICUSTAY_ID, -STORETIME, -CGID, -VALUE, 
           -WARNING, -ERROR, -RESULTSTATUS, -STOPPED) %>%
    mutate(
      SUBJECT_ID_COMPOSE = paste(SUBJECT_ID, HADM_ID, sep = "_"),
      CHARTTIME = ymd_hms(as.character(CHARTTIME))
    ) %>%
    group_by(SUBJECT_ID_COMPOSE, ITEMID) %>%
    arrange(desc(CHARTTIME)) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(-ROW_ID, -HADM_ID) %>%
    distinct()
  
  # Append to result list
  df_important_all[[i]] <- df_filtered
  
  # Remove raw df to save memory
  rm(df)
  gc()  # Call garbage collector
}

# Combine all parts into one dataframe
df_important_final <- bind_rows(df_important_all)

# Optional: remove intermediate list if needed
rm(df_important_all)
gc()

# Add labels name from d_items by ITEMID
df_final <- df_important_final %>%
  left_join(d_items %>% select(ITEMID, LABEL), by = "ITEMID") %>%
  filter(VALUENUM != "NA")

# Mapping from original LABEL to standardized names
label_map <- c(
  "Heart Rate" = "Heart_Rate",
  
  # Arterial BP
  "Arterial Blood Pressure systolic" = "ABP_SYS",
  "Arterial BP [Systolic]" = "ABP_SYS",
  "ABP [Systolic]" = "ABP_SYS",
  "ART BP Systolic" = "ABP_SYS",
  
  "Arterial Blood Pressure diastolic" = "ABP_DIA",
  "Arterial BP [Diastolic]" = "ABP_DIA",
  "ABP [Diastolic]" = "ABP_DIA",
  "ART BP Diastolic" = "ABP_DIA",
  
  "Arterial Blood Pressure mean" = "ABP_MEAN",
  "Arterial BP Mean" = "ABP_MEAN",
  "ART BP mean" = "ABP_MEAN",
  
  # Non-Invasive BP
  "Non Invasive Blood Pressure systolic" = "NBP_SYS",
  "NBP [Systolic]" = "NBP_SYS",
  "Manual BP [Systolic]" = "NBP_SYS",
  "BP Right Leg [Systolic]" = "NBP_SYS",
  "BP Right Arm [Systolic]" = "NBP_SYS",
  "BP Left Leg [Systolic]" = "NBP_SYS",
  "BP Left Arm [Systolic]" = "NBP_SYS",
  "BP Cuff [Systolic]" = "NBP_SYS",
  
  "Non Invasive Blood Pressure diastolic" = "NBP_DIA",
  "NBP [Diastolic]" = "NBP_DIA",
  "Manual BP [Diastolic]" = "NBP_DIA",
  "BP Right Leg [Diastolic]" = "NBP_DIA",
  "BP Right Arm [Diastolic]" = "NBP_DIA",
  "BP Left Leg [Diastolic]" = "NBP_DIA",
  "BP Left Arm [Diastolic]" = "NBP_DIA",
  "BP Cuff [Diastolic]" = "NBP_DIA",
  
  "Non Invasive Blood Pressure mean" = "NBP_MEAN",
  "NBP Mean" = "NBP_MEAN",
  "BP Right Leg [Mean]" = "NBP_MEAN",
  "BP Right Arm [Mean]" = "NBP_MEAN",
  "BP Left Leg [Mean]" = "NBP_MEAN",
  "BP Left Arm [Mean]" = "NBP_MEAN",
  "BP Cuff [Mean]" = "NBP_MEAN",
  
  # Temperature
  "Temperature Celsius" = "TEMP",
  "Temperature Fahrenheit" = "TEMP",
  "Skin Temperature" = "TEMP",
  "Temp Rectal" = "TEMP",
  "Temp Skin [C]" = "TEMP",
  "Temperature C" = "TEMP",
  "Temperature F" = "TEMP",
  
  # Respiratory Rate
  "Respiratory Rate" = "RESP_RATE",
  "Resp Rate" = "RESP_RATE",
  "Resp Rate (Total)" = "RESP_RATE",
  "Resp. Rate" = "RESP_RATE",
  "Respiratory Rate (Total)" = "RESP_RATE",
  "Breath Rate" = "RESP_RATE",
  
  # Pupils
  "Pupil Response [Left]" = "Pupil_Response_L",
  "Pupil Response [Right]" = "Pupil_Response_R"
)

# Apply renaming + Fahrenheit → Celsius conversion
df_final <- df_final %>%
  mutate(
    VALUENUM = case_when(
      LABEL %in% c( "Temperature Fahrenheit", "Temperature F") ~ round((VALUENUM - 32) * 5 / 9, 2),
      TRUE ~ VALUENUM
    ),
    LABEL = label_map[LABEL]
  ) %>% 
  mutate(
    CHARTTIME = parse_date_time(as.character(CHARTTIME), 
                                orders = c("ymd HMS", "ymd HM", "ymd"), 
                                tz = "UTC", 
                                quiet = TRUE)
  ) %>%
  group_by(SUBJECT_ID_COMPOSE, LABEL) %>%
  arrange(desc(CHARTTIME)) %>%
  slice_head(n = 1) %>%
  ungroup()

# Check number of unique VALUEUOMs per ITEMID
itemid_uom_check <- df_final %>%
  group_by(ITEMID) %>%
  summarise(n_uom = n_distinct(VALUEUOM), .groups = "drop") %>%
  filter(n_uom > 1)

# View ITEMIDs with more than one VALUEUOM
print(itemid_uom_check)

# Check VALUEUOM consistency per LABEL (instead of ITEMID)
label_uom_check <- df_final %>%
  group_by(LABEL) %>%
  summarise(n_uom = n_distinct(VALUEUOM), .groups = "drop") %>%
  filter(n_uom > 1)

# View labels with more than one VALUEUOM
print(label_uom_check)

# Check ranges of VALUE

df_final <- df_final %>%
  mutate(VALUENUM = case_when(
    LABEL == "Heart_Rate" & (VALUENUM < 20 | VALUENUM > 250) ~ NA_real_,
    
    LABEL == "ABP_SYS" & (VALUENUM < 30 | VALUENUM > 300) ~ NA_real_,
    LABEL == "ABP_DIA" & (VALUENUM < 10 | VALUENUM > 200) ~ NA_real_,
    LABEL == "ABP_MEAN" & (VALUENUM < 20 | VALUENUM > 250) ~ NA_real_,
    
    LABEL == "NBP_SYS" & (VALUENUM < 40 | VALUENUM > 300) ~ NA_real_,
    LABEL == "NBP_DIA" & (VALUENUM < 10 | VALUENUM > 200) ~ NA_real_,
    LABEL == "NBP_MEAN" & (VALUENUM < 20 | VALUENUM > 250) ~ NA_real_,
    
    LABEL == "TEMP" & (VALUENUM < 25 | VALUENUM > 43) ~ NA_real_,
    
    LABEL == "RESP_RATE" & (VALUENUM < 4 | VALUENUM > 70) ~ NA_real_,
    
    TRUE ~ VALUENUM
  ))

df_final <- df_final %>%
  filter(!is.na(VALUENUM)) %>%
  select(-ITEMID, -VALUEUOM, -CHARTTIME) %>%
  distinct() 

# Write cleaned admissions to csv
write.csv(df_final, "data/raw/cleaned/VITAL_SIGNS_clean.csv", row.names = FALSE)


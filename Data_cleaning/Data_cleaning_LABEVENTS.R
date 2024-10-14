library(dplyr)
library(tidyverse)
library(lubridate)

# Read labs from csv
labs <- read.csv("data/raw/LABEVENTS.csv", stringsAsFactors = TRUE)

# Read D_items from csv
lab_items <- read.csv("data/raw/D_LABITEMS.csv", stringsAsFactors = TRUE)

# Select necessary columns
labs <- labs %>%
  select(SUBJECT_ID, HADM_ID, ITEMID, CHARTTIME, VALUE, VALUENUM, VALUEUOM, FLAG)

# Normalize the columns by converting everything to uppercase
labs <- labs %>%
  mutate(across(where(is.factor), function(x) toupper(x) %>% as.factor()))

# Normalize the columns by converting everything to uppercase
lab_items <- lab_items %>%
  mutate(across(where(is.factor), function(x) toupper(x) %>% as.factor()))

# Replace empty spaces with NA
labs_NA <- labs %>%
  mutate(across(where(is.factor), function(x) as.character(x))) %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%
  mutate(across(where(is.character), function(x) as.factor(x)))

# Replace empty spaces with NA
lab_items_NA <- lab_items %>%
  mutate(across(where(is.factor), function(x) as.character(x))) %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%
  mutate(across(where(is.character), function(x) as.factor(x)))

# Change datetime format
labs_clean <-  labs %>%
  mutate(CHARTTIME = ymd_hms(CHARTTIME))

# Check if all times have valid hours (0-23), minutes (0-59), and seconds (0-59)
time_validation <- labs_clean %>%
  mutate(valid_time =
           is.na(CHARTTIME) | hour(CHARTTIME) %in% 0:23 & minute(CHARTTIME) %in% 0:59 & second(CHARTTIME) %in% 0:59)

# Filter rows with invalid times (if any)
labs_clean <- time_validation %>%
  filter(valid_time == TRUE) %>%
  mutate(valid_time = NULL)

# Perform a left join to add the LABEL column from items_clean to labs_clean
labs_clean <- labs_clean %>%
  left_join(lab_items_NA %>% select(ITEMID, LABEL, FLUID, CATEGORY), by = "ITEMID")

# Remove exact duplicate rows
labs_clean <- labs_clean %>%
  distinct()

# For each row, if a numeric value exists in the VALUE column and the VALUENUM column is empty (NA),
# copy the numeric value from VALUE to VALUENUM. Then, remove numeric values from VALUE if they are present
# in VALUENUM or if already copied, replacing them with NA.
labs_clean <- labs_clean %>%
  mutate(
    # If VALUENUM is NA and VALUE contains anything other than letters, copy VALUE to VALUENUM
    VALUENUM = ifelse(
      is.na(VALUENUM) & !grepl("^[A-Za-z]+$", VALUE), 
      ifelse(
        grepl("^\\d+-\\d+$", VALUE), 
        # For ranges like 3-5, calculate the midpoint
        (as.numeric(sub("-.*", "", VALUE)) + as.numeric(sub(".*-", "", VALUE))) / 2,  
        # For pure numbers with comparison symbols, remove symbols and extract number
        as.numeric(gsub("[^0-9.]", "", VALUE))  
      ), 
      VALUENUM
    ),
    
    # If VALUE contains a number or range, delete it (set to NA), leave words intact
    VALUE = ifelse(!grepl("^[A-Za-z]+$", VALUE), NA, as.character(VALUE))
  ) %>%
  mutate(
    # Convert VALUE to factor for future usage
    VALUE = as.factor(VALUE)
  )


# Cleaning and checking for distinct units
labs_clean <- labs_clean %>%
  mutate(
    # Ensure that VALUEUOM is treated as a character and clean up empty values
    VALUEUOM = as.character(VALUEUOM),
    VALUEUOM = ifelse(VALUEUOM == "" | VALUEUOM == " ", NA, VALUEUOM),  # Replace empty strings with NA
    VALUEUOM = trimws(VALUEUOM),  # Remove leading/trailing spaces
    VALUEUOM = toupper(VALUEUOM)  # Convert to uppercase to avoid case-related duplicates
  )

# Define a custom Mode function to find the most frequent value
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

labs_clean <- labs_clean %>%
  group_by(LABEL) %>%
  mutate(
    # Ensure that VALUEUOM is treated as a character column
    VALUEUOM = as.character(VALUEUOM),
    # Find the most frequent non-NA unit for each LABEL
    MostCommonUnit = ifelse(length(na.omit(VALUEUOM)) > 0, Mode(VALUEUOM[!is.na(VALUEUOM)]), NA),
    # Replace NA in VALUEUOM with the most frequent unit for that LABEL, but keep NA if no common unit
    VALUEUOM = ifelse(is.na(VALUEUOM), MostCommonUnit, VALUEUOM)
  ) %>%
  ungroup() %>%
  mutate(
    # Convert VALUEUOM to factor
    VALUEUOM = as.factor(VALUEUOM)
  ) %>%
  select(-MostCommonUnit)  # Remove the temporary MostCommonUnit column

# Check UOM for one label is the same
UOM_check <- labs_clean %>%
  group_by(LABEL) %>%
  summarize(
    UniqueUnits = n_distinct(VALUEUOM),  # Count distinct units for each label
    UnitsList = paste(unique(VALUEUOM), collapse = ", ")  # List all distinct units
  ) %>%
  filter(UniqueUnits > 1) 

labs_clean <- labs_clean %>%
  mutate(
    # Clean and standardize the unit names
    VALUEUOM = toupper(trimws(VALUEUOM)),  # Convert to uppercase and remove extra spaces
    
    # Standardize MG/24HOURS to MG/24HR (no value change needed)
    VALUEUOM = ifelse(VALUEUOM %in% c("MG/24HR", "MG/24HOURS"), "MG/24HR", VALUEUOM),
    
    # Convert MG/DL to EU/DL for BILIRUBIN
    VALUENUM = ifelse(LABEL == "BILIRUBIN" & VALUEUOM == "MG/DL", VALUENUM * 0.06, VALUENUM),
    VALUEUOM = ifelse(LABEL == "BILIRUBIN" & VALUEUOM == "MG/DL", "EU/DL", VALUEUOM),
    
    # Convert MG/DL to MG/L for C-REACTIVE PROTEIN
    VALUENUM = ifelse(LABEL == "C-REACTIVE PROTEIN" & VALUEUOM == "MG/DL", VALUENUM * 10, VALUENUM),
    VALUEUOM = ifelse(LABEL == "C-REACTIVE PROTEIN" & VALUEUOM == "MG/DL", "MG/L", VALUEUOM),
    
    # Convert UG/DL to NG/ML for DHEA-SULFATE
    VALUENUM = ifelse(LABEL == "DHEA-SULFATE" & VALUEUOM == "UG/DL", VALUENUM * 10, VALUENUM),
    VALUEUOM = ifelse(LABEL == "DHEA-SULFATE" & VALUEUOM == "UG/DL", "NG/ML", VALUEUOM),
    
    # Convert MIU/L to MIU/ML for FOLLICLE STIMULATING HORMONE and LUTEINIZING HORMONE
    VALUENUM = ifelse(LABEL %in% c("FOLLICLE STIMULATING HORMONE", "LUTEINIZING HORMONE") & VALUEUOM == "MIU/L", VALUENUM / 1000, VALUENUM),
    VALUEUOM = ifelse(LABEL %in% c("FOLLICLE STIMULATING HORMONE", "LUTEINIZING HORMONE") & VALUEUOM == "MIU/L", "MIU/ML", VALUEUOM),
    
    # Convert MOSM/L to MOSM/KG for OSMOLALITY, MEASURED (usually 1:1 conversion)
    VALUEUOM = ifelse(LABEL == "OSMOLALITY, MEASURED" & VALUEUOM == "MOSM/L", "MOSM/KG", VALUEUOM),
    
    # Standardize SECONDS to SEC everywhere (no value change needed)
    VALUEUOM = ifelse(VALUEUOM == "SECONDS", "SEC", VALUEUOM),
    
    # Convert PROSTATE SPECIFIC ANTIGEN UG/L to NG/ML
    VALUENUM = ifelse(LABEL == "PROSTATE SPECIFIC ANTIGEN" & VALUEUOM == "UG/L", VALUENUM * 1000, VALUENUM),
    VALUEUOM = ifelse(LABEL == "PROSTATE SPECIFIC ANTIGEN" & VALUEUOM == "UG/L", "NG/ML", VALUEUOM),
    
    # Convert RHEUMATOID FACTOR I.U. to IU/ML
    VALUENUM = ifelse(LABEL == "RHEUMATOID FACTOR" & VALUEUOM == "I.U.", VALUENUM, VALUENUM),  # Conversion factor if needed
    VALUEUOM = ifelse(LABEL == "RHEUMATOID FACTOR" & VALUEUOM == "I.U.", "IU/ML", VALUEUOM),
    
    # Convert TESTOSTERONE, FREE NG/DL to PG/ML
    VALUENUM = ifelse(LABEL == "TESTOSTERONE, FREE" & VALUEUOM == "NG/DL", VALUENUM * 10, VALUENUM),
    VALUEUOM = ifelse(LABEL == "TESTOSTERONE, FREE" & VALUEUOM == "NG/DL", "PG/ML", VALUEUOM),
    
    # Convert THYROID STIMULATING HORMONE UIU/ML to UU/ML
    VALUEUOM = ifelse(LABEL == "THYROID STIMULATING HORMONE" & VALUEUOM == "UIU/ML", "UU/ML", VALUEUOM),
    
    # Convert UROBILINOGEN MG/DL to EU/DL
    VALUEUOM = ifelse(LABEL == "UROBILINOGEN" & VALUEUOM == "MG/DL", "EU/DL", VALUEUOM),
    
   # Change label if condition is met
   LABEL = ifelse(LABEL == "CD34" & VALUEUOM == "#/UL", "CD34 CELL COUNT", as.character(LABEL)),  # Change label if condition is met

    LABEL = as.factor(LABEL),
   VALUEUOM = as.factor(VALUEUOM)
)

# Add seq_num for each analysis
labs_clean <- labs_clean %>%
  arrange(SUBJECT_ID, CATEGORY, CHARTTIME) %>%  # Ensure data is sorted by PatientID, Category, and Time
  group_by(SUBJECT_ID, CATEGORY, CHARTTIME) %>%  # Group by PatientID, Category, and Time
  mutate(
    SEQ_NUM = row_number()  # Create a sequence within each group starting from 1
  ) %>%
  ungroup()  # Ungroup after the mutation

# Clean VALUE COL
labs_clean <- labs_clean %>%
  filter(!(is.na(VALUE) & VALUENUM == 0)) %>%
  #  Delete rows where VALUE is "A" or "ERROR"
  filter(!(VALUE %in% c("A", "ERROR", "ERROE", "ERRROR", "ALT"))) %>%
  
  # Delete rows where VALUE == "DONE" and VALUENUM is NA
  filter(!(VALUE %in% c("DONE", "NOTDONE") & is.na(VALUENUM))) %>%

  mutate(
    LABEL = ifelse(LABEL == "URINE APPEARANCE" & VALUE %in% c("PINK", "RED"), "URINE COLOR", as.character(LABEL)),
    
    VALUE = ifelse(LABEL == "URINE COLOR" & VALUE %in% c("AM", "AMB"), "AMBER", as.character(VALUE)),
    VALUE = ifelse(LABEL == "URINE COLOR" & VALUE %in% c("Y", "YEL", "YELL", "YELLO", "TELLOW"), "YELLOW", as.character(VALUE)),
    VALUE = ifelse(LABEL == "URINE COLOR" & VALUE %in% c("S", "STR"), "STRAW", as.character(VALUE)),
    VALUE = ifelse(LABEL == "URINE COLOR" & VALUE %in% c("P"), "PINK", as.character(VALUE)),
    VALUE = ifelse(LABEL == "URINE COLOR" & VALUE %in% c("LT"), "LIGHT", as.character(VALUE)),
    VALUE = ifelse(LABEL == "URINE COLOR" & VALUE %in% c("BROWN", "DKBROWN", "LTBROWN"), "BROWN", as.character(VALUE)),
    VALUE = ifelse(LABEL == "URINE COLOR" & VALUE %in% c("CLEAR"), "CLEAR", as.character(VALUE)),
    VALUE = ifelse(LABEL == "URINE COLOR" & VALUE %in% c("DKAMB", "DKAMBE", "DKAMBER", "DKAML", "DRKAMBER"), "DARK AMBER", as.character(VALUE)),
    VALUE = ifelse(LABEL == "URINE COLOR" & VALUE %in% c("B"), "BLUE", as.character(VALUE)),
    
    VALUE = ifelse(LABEL == "URINE APPEARANCE" & VALUE %in% c("CL", "CLEAR"), "CLEAR", as.character(VALUE)),
    VALUE = ifelse(LABEL == "URINE APPEARANCE" & VALUE %in% c("CLDY", "CLO", "CLOU", "CLOUD", "CLOUDY"), "CLOUDY", as.character(VALUE)),
    VALUE = ifelse(LABEL == "URINE APPEARANCE" & VALUE %in% c("H", "HA", "HAZY"), "HAZY", as.character(VALUE)),
    VALUE = ifelse(LABEL == "URINE APPEARANCE" & VALUE %in% c("SL", "SLCLDY", "SLCLOUDY", "SLHAZY"), "SLIGHTLY CLOUDY", as.character(VALUE)),
    
    VALUE = ifelse(VALUE %in% c("NEGATIVE", "NEHATIVE"), "NEG", as.character(VALUE)),
    VALUE = ifelse(VALUE %in% c("P", "POSITIVE", "Y"), "POS", as.character(VALUE)),
    VALUE = ifelse(VALUE %in% c("N"), "NORMAL", as.character(VALUE)),
    VALUE = ifelse(VALUE == "NOT", "NONE", as.character(VALUE)),
    VALUE = ifelse(VALUE %in% c("L"), "LOW", as.character(VALUE)),
    VALUE = ifelse(VALUE %in% c("FEW", "F", "R", "RARE", "SM", "SMALL"), "FEW", as.character(VALUE)),
    VALUE = ifelse(VALUE %in% c("OCC", "O", "SOME"), "OCCASIONAL", as.character(VALUE)),
    VALUE = ifelse(VALUE %in% c("MOD", "MO", "M"), "MODERATE", as.character(VALUE)),
    VALUE = ifelse(VALUE %in% c("MANY", "LRG", "LG", "LGE", "LARGE", "VOID", "LARGE", "LOADED"), "MANY", as.character(VALUE)),
    VALUE = ifelse(VALUE == "TR", "TRACE", as.character(VALUE)),
    VALUE = ifelse(VALUE == "MED", "MEDIUM", as.character(VALUE)),
    VALUE = as.factor(VALUE),
    LABEL = as.factor(LABEL)
  )

LABS_VALUE <- labs_clean %>%
  filter(!is.na(VALUE)) %>%
  select (VALUE, VALUENUM, VALUEUOM, LABEL, CATEGORY) %>%
  distinct()

# Write cleaned admissions to csv
write.csv(labs_clean, "data/raw/cleaned/LABEVENTS_clean.csv")


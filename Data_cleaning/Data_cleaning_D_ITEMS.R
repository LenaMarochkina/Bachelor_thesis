# Read items from csv
items <- read.csv("data/raw/D_ITEMS.csv", stringsAsFactors = TRUE)
items %>% View()

# Normalize the columns by converting everything to uppercase
items <- items %>%
  mutate(across(where(is.factor), function(x) toupper(x) %>% as.factor()))

# Replace empty spaces with NA
items_NA <- items %>%
  mutate(across(where(is.factor), function(x) as.character(x))) %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%
  mutate(across(where(is.character), function(x) as.factor(x)))

# Remove exact duplicate rows
items_clean <- items_NA %>%
  distinct()

items <- items_clean %>%
  select(LABEL) %>%
  arrange(desc(LABEL))

# Choose procedures process
# Choose all patients ID who died from ADMISSION_clean.csv
died_patients <- read.csv("data/raw/ADMISSIONS_CLEAN.csv", stringsAsFactors = TRUE)
died_patients <- died_patients %>%
  filter(TIMETODEATH != 0) %>%  
  select(SUBJECT_ID) %>%                
  distinct()   

# Write cleaned items to csv
write.csv(items, "data/raw/items.csv")
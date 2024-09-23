# Read events from csv
# Define a function to process each chunk of data
f <- function(df, pos) {
  # Filter the chunk based on ITEMID (e.g., keeping only ITEMIDs 123, 456, 789)
  filtered_df <- df %>% group_by(ITEMID)
  
  # Print the position of the current chunk and the number of rows after filtering
  print(paste("Processing chunk at position:", pos, "with", nrow(filtered_df), "rows after filtering"))
  
  # Return the filtered chunk
  return(filtered_df)
}

# Read the CSV in chunks and apply the callback function
events <- read_csv_chunked("data/raw/CHARTEVENTS.csv", 
                           callback = DataFrameCallback$new(f),
                           chunk_size = 1000000)

# Normalize the columns by converting everything to uppercase
events <- events %>%
  mutate(across(where(is.factor), function(x) toupper(x) %>% as.factor()))

# Replace empty spaces with NA
events_NA <- events %>%
  mutate(across(where(is.factor), function(x) as.character(x))) %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%
  mutate(across(where(is.character), function(x) as.factor(x)))

# Remove exact duplicate rows
events_clean <- events_NA %>%
  distinct()
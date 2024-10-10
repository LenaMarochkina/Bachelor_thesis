# Read prescriptions from csv
prescriptions <- read.csv("data/raw/PRESCRIPTIONS.csv", stringsAsFactors = TRUE)

# Select necessary columns
prescriptions <- prescriptions %>%
  select(SUBJECT_ID, HADM_ID, ICUSTAY_ID, STARTDATE, ENDDATE,
         DRUG_TYPE, DRUG, DRUG_NAME_GENERIC, NDC, DOSE_VAL_RX, DOSE_UNIT_RX, ROUTE)

# Normalize the columns by converting everything to uppercase
prescriptions <- prescriptions %>%
  mutate(across(where(is.factor), function(x) toupper(x) %>% as.factor()))

# Replace empty spaces with NA
prescriptions_NA <- prescriptions %>%
  mutate(across(where(is.factor), function(x) as.character(x))) %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%
  mutate(across(where(is.character), function(x) as.factor(x)))

# Change datetime format
prescriptions_clean <-  prescriptions_NA %>%
  mutate(STARTDATE = ymd_hms(STARTDATE),
         ENDDATE = ymd_hms(ENDDATE))

# Check if all times have valid hours (0-23), minutes (0-59), and seconds (0-59)
time_validation <- prescriptions_clean %>%
  mutate(valid_time =
           is.na(STARTDATE) | hour(STARTDATE) %in% 0:23 & minute(STARTDATE) %in% 0:59 & second(STARTDATE) %in% 0:59 &
           is.na(ENDDATE) | hour(ENDDATE) %in% 0:23 & minute(ENDDATE) %in% 0:59 & second(ENDDATE) %in% 0:59)

# Filter rows with invalid times (if any)
prescriptions_clean <- time_validation %>%
  filter(valid_time == TRUE) %>%
  mutate(valid_time = NULL)

# Calculate the duration of each prescription in days
prescriptions_clean <- prescriptions_clean %>%
  mutate(PRESCRIPTION_DURATION = as.numeric(difftime(ENDDATE, STARTDATE, units = "days")))

# Ensure prescriptions are sorted by SUBJECT_ID, STARTDATE, and ENDDATE
prescriptions_clean <- prescriptions_clean %>%
  arrange(SUBJECT_ID, STARTDATE, ENDDATE)

# Create a sequential order of prescriptions for each patient using row_number()
prescriptions_clean <- prescriptions_clean %>%
  group_by(SUBJECT_ID) %>%
  mutate(SEQ_NUM = row_number()) %>%
  ungroup()

# Remove exact duplicate rows
prescriptions_clean <- prescriptions_NA %>%
  distinct()

# Function to clean the text by removing numbers, text in parentheses, and the '%' symbol
clean_text <- function(text) {
  text <- gsub("\\bNEO\\*[^*]*\\*\\b", "", text, ignore.case = TRUE)  # Remove patterns like NEO*PO*, NEO*IV*, etc.
  text <- gsub("\\s*\\([^)]*\\)", "", text)  # Remove text inside parentheses, including the space before
  text <- gsub("\\s*[0-9]+%?", "", text)  # Remove numbers and any following '%' symbol, including the space before
  text <- gsub("\\bflush\\b", "", text, ignore.case = TRUE)  # Remove the word "flush", case-insensitive
  text <- gsub("\\bhcl\\b", "", text, ignore.case = TRUE)  # Remove the word "hcl", case-insensitive
  text <- gsub("%", "", text)  # Remove any standalone '%' symbol
  text <- gsub("\\s+", " ", text)  # Replace multiple spaces with a single space
  text <- gsub("\\.", "", text)  # Remove all dots, including those inside words
  text <- trimws(text)  # Trim leading and trailing whitespace
  return(text)
}

# Apply the cleaning function to DRUG and GENERIC_DRUG_NAME columns
prescriptions_clean <- prescriptions_clean %>%
  mutate(
    DRUG = clean_text(DRUG),
    DRUG_NAME_GENERIC = clean_text(DRUG)
  )

# Create a named vector for drug patterns and their replacements (generic names or group names)
drug_lookup <- c(
  "DW" = "DEXTROSE",
  "ISO-OSMOTIC DEXTROSE" = "DEXTROSE",
  "NS" = "SODIUM CHLORIDE",
  "ISO-OSMOTIC SODIUM CHLORIDE" = "SODIUM CHLORIDE",
  "SW" = "STERILE WATER",
  "VANCOMYCIN" = "VANCOMYCIN",
  "VANCOMYCIN HCL" = "VANCOMYCIN",
  "HEPARIN SODIUM" = "HEPARIN",
  "PANTOPRAZOLE" = "PANTOPRAZOLE",
  "METRONIDAZOLE" = "METRONIDAZOLE",
  "AMIODARONE" = "AMIODARONE",
  "INSULIN - SLIDING SCALE" = "INSULIN (HUMAN)",
  "INSULIN HUMAN NPH" = "INSULIN (HUMAN)",
  "ASPIRIN" = "ACETYLSALICYLIC ACID",
  "ASPIRIN EC" = "ACETYLSALICYLIC ACID",
  "LR" = "LACTATED RINGERS",
  "IPRATROPIUM BROMIDE NEB" = "IPRATROPIUM BROMIDE",
  "HEPARIN FLUSH" = "HEPARIN",
  "ALBUTEROL INHALER" = "ALBUTEROL",
  "PIPERACILLIN-TAZOBACTAM NA" = "PIPERACILLIN-TAZOBACTAM",
  "HYDROMORPHONE PCA" = "HYDROMORPHONE",
  "CIPROFLOXACIN IV" = "CIPROFLOXACIN",
  "IPRATROPIUM BROMIDE HFA" = "IPRATROPIUM BROMIDE",
  "LIDOCAINE JELLY" = "LIDOCAINE",
  "MICONAZOLE POWDER 2%" = "MICONAZOLE",
  "TRAMADOL (ULTRAM)" = "TRAMADOL",
  "FLUTICASONE PROPIONATE 110MCG" = "FLUTICASONE PROPIONATE",
  "SYRINGE (IV ROOM)" = "SYRINGE",
  "HEPARIN (PRESERVATIVE FREE)" = "HEPARIN",
  "CYCLOSPORINE MODIFIED (NEORAL)" = "NEORAL",
  "NITROGLYCERIN SL" = "NITROGLYCERIN",
  "FENTANYL PATCH" = "FENTANYL",
  "ALUMINUM-MAGNESIUM HYDROX.-SIMETHICONE" = "SIMETHICONE",
  "DEXAMETHASONE SOD PHOSPHATE" = "DEXAMETHASONE SODIUM PHOSPHATE",
  "MORPHINE PCA" = "MORPHINE",
  "ISOTONIC SODIUM CHLORIDE" = "SODIUM CHLORIDE",
  "OXYCODONE-ACETAMINOPHEN ELIXIR" = "OXYCODONE-ACETAMINOPHEN",
  "METOPROLOL XL" = "METOPROLOL",
  "ARTIFICIAL TEARS PRESERV. FREE" = "ARTIFICIAL TEARS",
  "HYDROMORPHONE P.F." = "HYDROMORPHONE",
  "NIFEDIPINE CR" = "NIFEDIPINE",
  "IPRATROPIUM BROMIDE MDI" = "IPRATROPIUM BROMIDE",
  "MORPHINE SR" = "MORPHINE"
)

# Function to replace values in DRUG_NAME_GENERIC and DRUG columns based on the lookup table
replace_drug_names <- function(data, lookup) {
  data <- data %>%
    mutate(
      DRUG_NAME_GENERIC = case_when(
        sapply(names(lookup), function(pattern) grepl(pattern, DRUG_NAME_GENERIC, ignore.case = TRUE)) %>% rowSums() > 0 ~
          lookup[apply(sapply(names(lookup), function(pattern) grepl(pattern, DRUG_NAME_GENERIC, ignore.case = TRUE)), 1, which.max)],
        TRUE ~ as.character(DRUG_NAME_GENERIC)  # Keep the original value if no match is found
      ),
      DRUG = case_when(
        sapply(names(lookup), function(pattern) grepl(pattern, DRUG, ignore.case = TRUE)) %>% rowSums() > 0 ~
          lookup[apply(sapply(names(lookup), function(pattern) grepl(pattern, DRUG, ignore.case = TRUE)), 1, which.max)],
        TRUE ~ as.character(DRUG)  # Keep the original value if no match is found
      )
    )
  return(data)
}

# Apply the function to the prescriptions_clean dataset
prescriptions_clean <- replace_drug_names(prescriptions_clean, drug_lookup)

# Ensure that prescriptions_clean has the NDC column
ndc_codes <- prescriptions_clean$NDC

# Remove any duplicates to avoid querying the same code multiple times
ndc_codes <- unique(ndc_codes)

# Remove any NA values in case there are missing NDC codes
ndc_codes <- na.omit(ndc_codes)

# Function to query the RxNorm API and extract the first RXCUI using the NDC code
get_rxcui_from_ndc <- function(ndc) {
  url <- paste0("https://rxnav.nlm.nih.gov/REST/relatedndc.json?relation=drug&ndc=", ndc)
  response <- GET(url)

  if (status_code(response) == 200) {
    data <- content(response, as = "parsed", type = "application/json")

    # Check if ndcInfoList exists in the response
    if (!is.null(data$ndcInfoList$ndcInfo)) {
      # Extract the first rxcui value associated with the given ndc
      first_rxcui <- data$ndcInfoList$ndcInfo[[1]]$rxcui
      return(first_rxcui)  # Return the first RXCUI found
    } else {
      return(NA)  # Return NA if no relevant information is found
    }
  } else {
    message("Error in API call: ", status_code(response))
    return(NA)
  }
}

# Ensure that prescriptions_clean has the NDC column
ndc_codes <- unique(na.omit(prescriptions_clean$NDC))  # Extract unique and non-NA NDC codes

# Map NDC codes to RxCUI and save results in a separate data frame
ndc_rxcui_map <- data.frame(NDC = character(), RXCUI = character(), stringsAsFactors = FALSE)

for (ndc in ndc_codes) {
  rxcui <- get_rxcui_from_ndc(ndc)
  ndc_rxcui_map <- rbind(ndc_rxcui_map, data.frame(NDC = ndc, RXCUI = rxcui, stringsAsFactors = FALSE))
}

# Create dataset with NA RXCUI, add to dataset DRUG_GENERIC_NAME + DOSE_VAL_RX + DOSE_UNIT_RX  as new col
# Filter rows with missing RXCUI values
na_rxcui_dataset_with_doses <- ndc_rxcui_map %>%
  filter(is.na(RXCUI))

na_rxcui_dataset_with_doses <- na_rxcui_dataset_with_doses %>%
  left_join(prescriptions_clean %>% select(NDC, DRUG, DRUG_NAME_GENERIC, DOSE_VAL_RX, DOSE_UNIT_RX), by = "NDC") %>%
  mutate(
    COMBINED_INFO = ifelse(
      !is.na(DRUG_NAME_GENERIC) & !is.na(DOSE_VAL_RX) & DOSE_VAL_RX != 0,
      paste(DRUG_NAME_GENERIC, DOSE_VAL_RX, DOSE_UNIT_RX),
      ifelse(
        is.na(DRUG_NAME_GENERIC) & !is.na(DRUG),  # Check if DRUG_NAME_GENERIC is NA and DRUG is not NA
        ifelse(!is.na(DOSE_VAL_RX) & DOSE_VAL_RX != 0, paste(DRUG, DOSE_VAL_RX, DOSE_UNIT_RX), DRUG),  # Combine DRUG with dose if applicable
        as.character(DRUG_NAME_GENERIC)  # Default to DRUG_NAME_GENERIC if available
      )
    )
  ) %>% select(-DRUG, -DRUG_NAME_GENERIC, -DOSE_VAL_RX, -DOSE_UNIT_RX) %>%
  distinct()

# Function to query the RxNorm API and extract the first RXCUI using the COMBINED_INFO column
get_rxcui_from_name <- function(combined_info) {
  url <- paste0("https://rxnav.nlm.nih.gov/REST/rxcui.json?name=", URLencode(combined_info), "&search=2&allsrc=1")
  response <- GET(url)

  # Check if the API call was successful
  if (status_code(response) == 200) {
    data <- content(response, as = "parsed", type = "application/json")

    # Print the raw response to check if it contains the expected data
    print(data)

    # Check if idGroup and rxnormId exist in the response
    if (!is.null(data$idGroup$rxnormId)) {
      first_rxcui <- data$idGroup$rxnormId[[1]]
      return(first_rxcui)  # Return the first RXCUI found
    } else {
      message("RXCUI not found for: ", combined_info)
      return(NA)  # Return NA if no relevant information is found
    }
  } else {
    message("Error in API call for: ", combined_info, " - Status code: ", status_code(response))
    return(NA)
  }
}

# Function to fetch RXCUI in parallel using furrr with improved error handling
fetch_rxcui_in_parallel <- function(combined_info_list, batch_size = 500) {
  library(furrr)
  plan(multisession)  # Set the plan for parallel processing

  # Split the list into batches of size `batch_size`
  batches <- split(combined_info_list, ceiling(seq_along(combined_info_list) / batch_size))

  rxcui_results <- vector("character", length(combined_info_list))  # Pre-allocate results vector
  total_batches <- length(batches)  # Total number of batches

  # Iterate over each batch and fetch results in parallel
  for (i in seq_along(batches)) {
    message(paste("Processing batch", i, "of", total_batches, "- Remaining:", total_batches - i, "batches"))

    batch <- batches[[i]]
    batch_results <- future_map(batch, possibly(get_rxcui_from_name, NA_character_), .progress = TRUE)

    # Calculate the start and end positions for the batch in the results vector
    start_index <- (i - 1) * batch_size + 1
    end_index <- min(i * batch_size, length(combined_info_list))  # Ensure the end index doesn't exceed the list size

    # Store the results in the corresponding positions in the results vector
    rxcui_results[start_index:end_index] <- unlist(batch_results)
  }

  return(rxcui_results)
}

# Apply the function to the na_rxcui_dataset to fetch RXCUI using the COMBINED_INFO column with a progress bar
na_rxcui_dataset_with_doses <- na_rxcui_dataset_with_doses %>%
  mutate(rxcui = fetch_rxcui_in_parallel(COMBINED_INFO)) %>%
  mutate(RXCUI = rxcui) %>%
  select(-rxcui)

# Perform a left join to add RXCUI from ndc_rxcui_map to prescriptions_clean using the NCD column
prescriptions_clean <- prescriptions_clean %>%
  left_join(ndc_rxcui_map, by = "NDC")

na_rxcui_dataset_names_only <- prescriptions_clean %>%
  filter(is.na(RXCUI)) %>%  # Filter rows where RXCUI is missing
  mutate(
    COMBINED_INFO = ifelse(
      !is.na(DRUG_NAME_GENERIC),
      as.character(DRUG_NAME_GENERIC),  # Use DRUG_NAME_GENERIC if available
      as.character(DRUG)  # Otherwise, use DRUG
    )
  ) %>%
  select(NDC, COMBINED_INFO) %>%  # Select only NDC and COMBINED_INFO columns
  distinct()  # Remove duplicate rows

# Apply the function to the na_rxcui_dataset to fetch RXCUI using the COMBINED_INFO column in parallel
na_rxcui_dataset_names_only <- na_rxcui_dataset_names_only %>%
  mutate(rxcui = fetch_rxcui_in_parallel(COMBINED_INFO))

prescriptions_clean <- prescriptions_clean %>%
  mutate(
    COMBINED_INFO = ifelse(
      !is.na(DRUG_NAME_GENERIC),
      as.character(DRUG_NAME_GENERIC),  # Use DRUG_NAME_GENERIC if available
      as.character(DRUG)  # Otherwise, use DRUG
    )
  )

prescriptions_clean <- prescriptions_clean %>%
  left_join(na_rxcui_dataset_names_only, by = c("NDC", "COMBINED_INFO")) %>%  # Perform the join based on the NDC column
  mutate(
    RXCUI = coalesce(RXCUI, rxcui)  # Keep the existing RXCUI if present, otherwise use the value from rxcui
  ) %>%
  select(-rxcui)

na <- prescriptions_clean %>%
  filter(is.na(RXCUI)) %>%
  count(COMBINED_INFO, name = "Frequency") %>%  # Count occurrences of each unique COMBINED_INFO value
  arrange(desc(Frequency))  # Arrange the results in descending order of frequency

# Function to get ATC data using the RxClass API
get_atc_info <- function(rxcui) {
  url <- paste0("https://rxnav.nlm.nih.gov/REST/rxclass/class/byRxcui.xml?rxcui=", rxcui, "&relaSource=ATC")
  response <- GET(url)

  if (status_code(response) == 200) {
    data <- content(response, as = "parsed")

    # Parse XML response to extract ATC information
    atc_id <- xml_text(xml_find_first(data, "//rxclassMinConceptItem/classId"))
    atc_name <- xml_text(xml_find_first(data, "//rxclassMinConceptItem/className"))
    atc_type <- xml_text(xml_find_first(data, "//rxclassMinConceptItem/classType"))

    return(list(RXCUI = rxcui, ATC_ID = atc_id, ATC_NAME = atc_name, ATC_TYPE = atc_type))
  } else {
    return(list(RXCUI = rxcui, ATC_ID = NA, ATC_NAME = NA, ATC_TYPE = NA))
  }
}

# Function to fetch ATC information in parallel with a progress bar
fetch_atc_info_parallel <- function(rxcui_list, batch_size = 1000) {
  plan(multisession)  # Use multiple sessions for parallel processing

  # Split the list into batches of size `batch_size`
  batches <- split(rxcui_list, ceiling(seq_along(rxcui_list) / batch_size))

  pb <- progress_bar$new(
    format = "Fetching ATC Info [:bar] :percent (:current/:total) Elapsed: :elapsed ETA: :eta",
    total = length(batches), clear = FALSE, width = 60
  )

  atc_results <- vector("list", length(rxcui_list))  # Pre-allocate list for results

  for (i in seq_along(batches)) {
    batch <- batches[[i]]

    # Send API requests in parallel for the current batch
    batch_results <- future_map(batch, get_atc_info, .progress = FALSE)

    start_index <- (i - 1) * batch_size + 1
    end_index <- min(i * batch_size, length(rxcui_list))

    atc_results[start_index:end_index] <- batch_results

    pb$tick()  # Update the progress bar
  }

  # Convert list of results to a data frame
  atc_df <- do.call(rbind, lapply(atc_results, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  return(atc_df)
}

# Generate a list of unique RXCUI values from prescriptions_clean
unique_rxcui_list <- prescriptions_clean %>%
  filter(!is.na(RXCUI)) %>%  # Remove rows with missing RXCUI values
  distinct(RXCUI) %>%  # Keep only unique RXCUI values
  pull(RXCUI)  # Extract the unique values as a list

# Fetch ATC data using the unique RXCUI list and ensure all RXCUI values are included
atc_data <- fetch_atc_info_parallel(unique_rxcui_list)

# Combine the ATC data with the prescriptions_clean DataFrame
prescriptions_clean <- prescriptions_clean %>%
  left_join(atc_data, by = "RXCUI")



top_50_drugs <- prescriptions_clean %>%
  group_by(ATC_ID, DRUG_NAME_GENERIC) %>%
  summarise(Count = n()) %>%  # Count occurrences of each combination
  arrange(desc(Count)) %>%  # Sort by count in descending order
  slice_head(n = 50)



# Function to manually update the ATC codes for specific drugs
update_atc_codes <- function(data) {
  data <- data %>%
    mutate(
      ATC_ID = case_when(
        GENERIC_DRUG_NAME == "sodium chloride" ~ "B05XA",
        GENERIC_DRUG_NAME == "lactated ringers" ~ "B05BB",
        TRUE ~ ATC_ID  # Keep the existing ATC_ID if no match is found
      ),
      ATC_NAME = case_when(
        GENERIC_DRUG_NAME == "sodium chloride" ~ "Electrolyte solutions",
        GENERIC_DRUG_NAME == "lactated ringers" ~ "Solutions affecting the electrolyte balance",
        TRUE ~ ATC_NAME  # Keep the existing ATC_NAME if no match is found
      )
    )

  return(data)
}

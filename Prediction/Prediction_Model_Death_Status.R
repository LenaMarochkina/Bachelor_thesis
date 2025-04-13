# Load the necessary package if not return error 
# Check if the xgboost package is installed and load it
if (!requireNamespace("xgboost", quietly = TRUE)) {
  stop("The xgboost package is required but not installed. Please install it.")
}
library(xgboost)

# Function to load the saved XGBoost model
load_model <- function() {
  xgb.load("~/Bachelor_thesis/Prediction/Models/final_xgb_model_shap.json")
}

train_means <- c(
  AGE_AT_ADMISSION = 63.51743,
  ICD9_51881 = 0,
  PRESCRIPTIONS_NUM = 75.14568,
  BG_LAST_LACTATE = 2.218006,
  H_LAST_RDW = 15.1308,
  H_LAST_LYMPHOCYTES = 14.52874,
  CH_LAST_LD = 384.9189,
  ICD9_0389 = 0,
  DIAGNOSES_NUM = 14.02369,
  BG_LAST_CALCULATED_TOTAL_CO2 = 25.93045,
  CH_LAST_GLUCOSE = 139.8626,
  INVASIVE_VENTILATION = 1091.938,
  BG_LAST_TIDAL_VOLUME = 558.5699,
  H_LAST_PLATELET_COUNT = 239.4711,
  H_LAST_WHITE_BLOOD_CELLS = 11.41238,
  CH_LAST_UREA_NITROGEN = 26.46355,
  ICD9_431 = 0,
  ICD9_41401 = 0,
  BG_LAST_PO2 = 173.5177,
  CH_LAST_BILIRUBIN_TOTAL = 1.422085,
  CH_LAST_MAGNESIUM = 1.993165,
  BG_LAST_PCO2 = 42.49261,
  BG_LAST_PEEP = 6.579534,
  CH_LAST_ANION_GAP = 14.33658,
  H_LAST_PTT = 35.92556,
  BG_LAST_OXYGEN = 62.91051,
  CH_LAST_SODIUM = 138.5351,
  BG_LAST_ALVEOLAR_ARTERIAL_GRADIENT = 444.5892,
  H_LAST_MCHC = 33.67353,
  CH_LAST_TROPONIN_T = 0.4011111,
  BG_LAST_TEMPERATURE = 37.00815,
  BG_LAST_REQUIRED_O2 = 75.05059,
  H_LAST_INR_PT = 1.476776,
  H_LAST_MCV = 89.77843,
  EXTUBATION = 0.1549485,
  BG_LAST_PH = 7.379726,
  H_LAST_FIBRINOGEN_FUNCTIONAL = 330.9716,
  CH_LAST_CREATININE = 1.464814,
  H_LAST_PT = 15.57762
)


# Define the prediction function that handles NA/infinite imputation and applies a logged transformation with offset.
x <- function(
    AGE_AT_ADMISSION, ICD9_51881, PRESCRIPTIONS_NUM, BG_LAST_LACTATE, H_LAST_RDW, H_LAST_LYMPHOCYTES,
    CH_LAST_LD, ICD9_0389, DIAGNOSES_NUM, BG_LAST_CALCULATED_TOTAL_CO2, CH_LAST_GLUCOSE,
    INVASIVE_VENTILATION, BG_LAST_TIDAL_VOLUME, H_LAST_PLATELET_COUNT, H_LAST_WHITE_BLOOD_CELLS, CH_LAST_UREA_NITROGEN,
    ICD9_431, ICD9_41401, BG_LAST_PO2, CH_LAST_BILIRUBIN_TOTAL, CH_LAST_MAGNESIUM, BG_LAST_PCO2, BG_LAST_PEEP,
    CH_LAST_ANION_GAP, H_LAST_PTT, BG_LAST_OXYGEN, CH_LAST_SODIUM, BG_LAST_ALVEOLAR_ARTERIAL_GRADIENT, H_LAST_MCHC,
    CH_LAST_TROPONIN_T, BG_LAST_TEMPERATURE, BG_LAST_REQUIRED_O2, H_LAST_INR_PT, H_LAST_MCV, EXTUBATION, BG_LAST_PH,
    H_LAST_FIBRINOGEN_FUNCTIONAL, CH_LAST_CREATININE, H_LAST_PT
) {
  # Create a one-row data frame with the provided parameters.
  input_data <- data.frame(
    AGE_AT_ADMISSION = as.numeric(AGE_AT_ADMISSION),
    ICD9_51881 = as.numeric(ICD9_51881),
    PRESCRIPTIONS_NUM = as.numeric(PRESCRIPTIONS_NUM),
    BG_LAST_LACTATE = as.numeric(BG_LAST_LACTATE),
    H_LAST_RDW = as.numeric(H_LAST_RDW),
    H_LAST_LYMPHOCYTES = as.numeric(H_LAST_LYMPHOCYTES),
    CH_LAST_LD = as.numeric(CH_LAST_LD),
    ICD9_0389 = as.numeric(ICD9_0389),
    DIAGNOSES_NUM = as.numeric(DIAGNOSES_NUM),
    BG_LAST_CALCULATED_TOTAL_CO2 = as.numeric(BG_LAST_CALCULATED_TOTAL_CO2),
    CH_LAST_GLUCOSE = as.numeric(CH_LAST_GLUCOSE),
    INVASIVE_VENTILATION = as.numeric(INVASIVE_VENTILATION),
    BG_LAST_TIDAL_VOLUME = as.numeric(BG_LAST_TIDAL_VOLUME),
    H_LAST_PLATELET_COUNT = as.numeric(H_LAST_PLATELET_COUNT),
    H_LAST_WHITE_BLOOD_CELLS = as.numeric(H_LAST_WHITE_BLOOD_CELLS),
    CH_LAST_UREA_NITROGEN = as.numeric(CH_LAST_UREA_NITROGEN),
    ICD9_431 = as.numeric(ICD9_431),
    ICD9_41401 = as.numeric(ICD9_41401),
    BG_LAST_PO2 = as.numeric(BG_LAST_PO2),
    CH_LAST_BILIRUBIN_TOTAL = as.numeric(CH_LAST_BILIRUBIN_TOTAL),
    CH_LAST_MAGNESIUM = as.numeric(CH_LAST_MAGNESIUM),
    BG_LAST_PCO2 = as.numeric(BG_LAST_PCO2),
    BG_LAST_PEEP = as.numeric(BG_LAST_PEEP),
    CH_LAST_ANION_GAP = as.numeric(CH_LAST_ANION_GAP),
    H_LAST_PTT = as.numeric(H_LAST_PTT),
    BG_LAST_OXYGEN = as.numeric(BG_LAST_OXYGEN),
    CH_LAST_SODIUM = as.numeric(CH_LAST_SODIUM),
    BG_LAST_ALVEOLAR_ARTERIAL_GRADIENT = as.numeric(BG_LAST_ALVEOLAR_ARTERIAL_GRADIENT),
    H_LAST_MCHC = as.numeric(H_LAST_MCHC),
    CH_LAST_TROPONIN_T = as.numeric(CH_LAST_TROPONIN_T),
    BG_LAST_TEMPERATURE = as.numeric(BG_LAST_TEMPERATURE),
    BG_LAST_REQUIRED_O2 = as.numeric(BG_LAST_REQUIRED_O2),
    H_LAST_INR_PT = as.numeric(H_LAST_INR_PT),
    H_LAST_MCV = as.numeric(H_LAST_MCV),
    EXTUBATION = as.numeric(EXTUBATION),
    BG_LAST_PH = as.numeric(BG_LAST_PH),
    H_LAST_FIBRINOGEN_FUNCTIONAL = as.numeric(H_LAST_FIBRINOGEN_FUNCTIONAL),
    CH_LAST_CREATININE = as.numeric(CH_LAST_CREATININE),
    H_LAST_PT = as.numeric(H_LAST_PT)
  )
  
  # Impute missing or infinite values using the training means
  for (col in names(input_data)) {
    if (is.na(input_data[[col]]) || is.infinite(input_data[[col]])) {
      input_data[[col]] <- train_means[[col]]
    }
  }
  
  # --- Apply logarithmic transformation with an offset to avoid log(0) ---
  offset <- 1e-8  # small constant to ensure positivity
  input_data_logged <- as.data.frame(lapply(input_data, function(x) log(x + offset)))
  
  # Convert the logged data to an xgb.DMatrix, indicating that NA is missing.
  dmatrix <- xgb.DMatrix(data = as.matrix(input_data_logged), missing = NA)
  
  # Load the model
  model <- load_model()
  
  # Generate predictions (model output is on the logged target scale)
  preds <- predict(model, dmatrix)
  
  # Round the prediction to 2 decimal places
  preds <- round(preds, 2)
  
  # Determine risk category based on the unlogged prediction
  risk_category <- ifelse(preds < 0.1, "Very Low Risk",
                          ifelse(preds < 0.2, "Low Risk",
                                 ifelse(preds < 0.5, "Moderate Risk",
                                        ifelse(preds < 0.8, "High Risk", "Very High Risk"))))
  
  result <- paste("The probability of mortality is", preds,
                  ". The patient is under", risk_category, "!")
  return(result)
}

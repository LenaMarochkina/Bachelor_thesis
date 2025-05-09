# Load the necessary package if not return error 
# Check if the xgboost package is installed and load it
if (!requireNamespace("xgboost", quietly = TRUE)) {
  stop("The xgboost package is required but not installed. Please install it.")
}
library(xgboost)

# Function to load the saved XGBoost model
load_model <- function() {
  xgb.load("~/Bachelor_thesis/Prediction/Models/xgboost_shap_model_Survival_Length.json")
}

# Function to load the training means
train_means <- c(
  DRUGS_NUM = 28.64229192,
  BG_LAST_OXYGEN = 63.25991315,
  DIAGNOSES_NUM = 13.81556499,
  BG_LAST_LACTATE = 2.21953779,
  INVASIVE_VENTILATION = 913.02248722,
  CH_LAST_BILIRUBIN_TOTAL = 1.37077734,
  CH_LAST_ANION_GAP = 14.35616868,
  H_LAST_BASOPHILS = 0.44449672,
  DIALYSIS_CRRT = 71.15574103,
  H_LAST_EOSINOPHILS = 1.70945800,
  BG_LAST_PH = 7.37927758,
  H_LAST_PT = 15.56416126,
  BG_LAST_O2_FLOW = 12.85163491,
  ICD9_0389 = 0,
  CH_LAST_CREATININE = 1.46281911,
  H_LAST_WHITE_BLOOD_CELLS = 11.41697951,
  CH_LAST_AST = 118.23124366,
  CH_LAST_BICARBONATE = 24.79968306,
  ARTERIAL_LINE = 1032.88175617,
  CH_LAST_GLUCOSE = 140.15249083,
  ICD9_5070 = 0,
  BG_LAST_PO2 = 175.19368506,
  CH_LAST_POTASSIUM = 4.20638465,
  BG_LAST_FREE_CALCIUM = 1.13338606,
  CH_LAST_LD = 384.76045380,
  BG_LAST_OXYGEN_SATURATION = 89.90769472,
  MULTI_LUMEN = 814.41778295,
  BG_LAST_CALCULATED_TOTAL_CO2 = 25.93878080,
  H_LAST_PTT = 35.81208830,
  CH_LAST_PHOSPHATE = 3.57225768
)

x <- function(
    DRUGS_NUM, BG_LAST_OXYGEN, DIAGNOSES_NUM, BG_LAST_LACTATE, INVASIVE_VENTILATION, 
    CH_LAST_BILIRUBIN_TOTAL, CH_LAST_ANION_GAP, H_LAST_BASOPHILS, DIALYSIS_CRRT, 
    H_LAST_EOSINOPHILS, BG_LAST_PH, H_LAST_PT, BG_LAST_O2_FLOW, ICD9_0389, 
    CH_LAST_CREATININE, H_LAST_WHITE_BLOOD_CELLS, CH_LAST_AST, CH_LAST_BICARBONATE, 
    ARTERIAL_LINE, CH_LAST_GLUCOSE, ICD9_5070, BG_LAST_PO2, CH_LAST_POTASSIUM, 
    BG_LAST_FREE_CALCIUM, CH_LAST_LD, BG_LAST_OXYGEN_SATURATION, MULTI_LUMEN, 
    BG_LAST_CALCULATED_TOTAL_CO2, H_LAST_PTT, CH_LAST_PHOSPHATE
) {
  # Create one-row data frame with all predictors
  input_data <- data.frame(
    DRUGS_NUM = as.numeric(DRUGS_NUM),
    BG_LAST_OXYGEN = as.numeric(BG_LAST_OXYGEN),
    DIAGNOSES_NUM = as.numeric(DIAGNOSES_NUM),
    BG_LAST_LACTATE = as.numeric(BG_LAST_LACTATE),
    INVASIVE_VENTILATION = as.numeric(INVASIVE_VENTILATION),
    CH_LAST_BILIRUBIN_TOTAL = as.numeric(CH_LAST_BILIRUBIN_TOTAL),
    CH_LAST_ANION_GAP = as.numeric(CH_LAST_ANION_GAP),
    H_LAST_BASOPHILS = as.numeric(H_LAST_BASOPHILS),
    DIALYSIS_CRRT = as.numeric(DIALYSIS_CRRT),
    H_LAST_EOSINOPHILS = as.numeric(H_LAST_EOSINOPHILS),
    BG_LAST_PH = as.numeric(BG_LAST_PH),
    H_LAST_PT = as.numeric(H_LAST_PT),
    BG_LAST_O2_FLOW = as.numeric(BG_LAST_O2_FLOW),
    ICD9_0389 = as.numeric(ICD9_0389),
    CH_LAST_CREATININE = as.numeric(CH_LAST_CREATININE),
    H_LAST_WHITE_BLOOD_CELLS = as.numeric(H_LAST_WHITE_BLOOD_CELLS),
    CH_LAST_AST = as.numeric(CH_LAST_AST),
    CH_LAST_BICARBONATE = as.numeric(CH_LAST_BICARBONATE),
    ARTERIAL_LINE = as.numeric(ARTERIAL_LINE),
    CH_LAST_GLUCOSE = as.numeric(CH_LAST_GLUCOSE),
    ICD9_5070 = as.numeric(ICD9_5070),
    BG_LAST_PO2 = as.numeric(BG_LAST_PO2),
    CH_LAST_POTASSIUM = as.numeric(CH_LAST_POTASSIUM),
    BG_LAST_FREE_CALCIUM = as.numeric(BG_LAST_FREE_CALCIUM),
    CH_LAST_LD = as.numeric(CH_LAST_LD),
    BG_LAST_OXYGEN_SATURATION = as.numeric(BG_LAST_OXYGEN_SATURATION),
    MULTI_LUMEN = as.numeric(MULTI_LUMEN),
    BG_LAST_CALCULATED_TOTAL_CO2 = as.numeric(BG_LAST_CALCULATED_TOTAL_CO2),
    H_LAST_PTT = as.numeric(H_LAST_PTT),
    CH_LAST_PHOSPHATE = as.numeric(CH_LAST_PHOSPHATE)
  )

  # Replace NA or Inf with training means
  for (col in names(input_data)) {
    val <- input_data[[col]]
    if (is.na(val) || is.infinite(val)) {
      input_data[[col]] <- train_means[[col]]
    }
  }
  
  no_log_vars <- c("ICD9_0389", "ICD9_5070")
  
  # Step 4: Create a copy of input_data to apply the transformation.
  input_data_transformed <- input_data
  
  # For each column, apply the log transformation only if the column is NOT in no_log_vars.
  for (col in names(input_data)) {
    if (!(col %in% no_log_vars)) {
      # Use log(x + 1) to avoid log(0); adjust the offset if needed.
      input_data_transformed[[col]] <- log(input_data[[col]] + 1)
    } 
    # Else, leave the column value unchanged.
  }
  
  # Convert to xgb.DMatrix
  dmatrix <- xgb.DMatrix(data = as.matrix(input_data_transformed), missing = NA)
  
  # Load trained model
  model <- load_model()
  
  # Predict and round
  preds <- round(predict(model, dmatrix), 0)
  
  result <- paste("Expected Survival Length for the patient is ", preds, ".")
  return(result)
}

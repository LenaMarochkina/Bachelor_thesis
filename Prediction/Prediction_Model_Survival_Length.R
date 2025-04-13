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
  
)

x <- function(
    DRUGS_NUM, BG_LAST_OXYGEN, DIAGNOSES_NUM, BG_LAST_LACTATE, INVASIVE_VENTILATION,
    CH_LAST_BILIRUBIN_TOTAL, CH_LAST_ANION_GAP, H_LAST_BASOPHILS, DIALYSIS_CRRT,
    H_LAST_EOSINOPHILS, BG_LAST_PH, H_LAST_PT, BG_LAST_O2_FLOW, ICD9_0389,
    CH_LAST_CREATININE, H_LAST_WHITE_BLOOD_CELLS, CH_LAST_AST, CH_LAST_BICARBONATE,
    ARTERIAL_LINE, CH_LAST_GLUCOSE, ICD9_5070, BG_LAST_PO2, CH_LAST_POTASSIUM,
    BG_LAST_FREE_CALCIUM, CH_LAST_LD, BG_LAST_OXYGEN_SATURATION, MULTI_LUMEN,
    BG_LAST_CALCULATED_TOTAL_CO2, H_LAST_PTT, CH_LAST_PHOSPHATE, H_LAST_LYMPHOCYTES,
    H_LAST_NEUTROPHILS, CH_LAST_MAGNESIUM, H_LAST_RED_BLOOD_CELLS, CH_LAST_ALT,
    BG_LAST_PCO2, CH_LAST_UREA_NITROGEN, H_LAST_MCH, AGE_AT_ADMISSION, CH_LAST_TROPONIN_T
) {
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
    CH_LAST_PHOSPHATE = as.numeric(CH_LAST_PHOSPHATE),
    H_LAST_LYMPHOCYTES = as.numeric(H_LAST_LYMPHOCYTES),
    H_LAST_NEUTROPHILS = as.numeric(H_LAST_NEUTROPHILS),
    CH_LAST_MAGNESIUM = as.numeric(CH_LAST_MAGNESIUM),
    H_LAST_RED_BLOOD_CELLS = as.numeric(H_LAST_RED_BLOOD_CELLS),
    CH_LAST_ALT = as.numeric(CH_LAST_ALT),
    BG_LAST_PCO2 = as.numeric(BG_LAST_PCO2),
    CH_LAST_UREA_NITROGEN = as.numeric(CH_LAST_UREA_NITROGEN),
    H_LAST_MCH = as.numeric(H_LAST_MCH),
    AGE_AT_ADMISSION = as.numeric(AGE_AT_ADMISSION),
    CH_LAST_TROPONIN_T = as.numeric(CH_LAST_TROPONIN_T)
  )
  
  # Replace NA or Inf with training means
  for (col in names(input_data)) {
    val <- input_data[[col]]
    if (is.na(val) || is.infinite(val)) {
      input_data[[col]] <- train_means[[col]]
    }
  }
  
  # Log transformation with offset
  input_data_logged <- as.data.frame(lapply(input_data, function(x) log(x + 1)))
  
  # Convert to xgb.DMatrix
  dmatrix <- xgb.DMatrix(data = as.matrix(input_data_logged), missing = NA)
  
  # Load trained model
  model <- load_model()
  
  # Predict and round
  preds <- round(predict(model, dmatrix), 0)
  
  result <- paste("Expected Survival Length for the patient is ", preds, ".")
  return(result)
}

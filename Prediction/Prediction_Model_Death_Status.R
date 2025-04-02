# Load the necessary package
library(xgboost)

# Function to load the saved XGBoost model
load_model <- function() {
  xgb.load("~/Bachelor_thesis/Prediction/Models/final_xgb_model_shap.json")
}

# Define the prediction function with new parameter names.
predict_survival <- function(
    AGE_AT_ADMISSION, ICD9_51881, PRESCRIPTIONS_NUM, BG_LAST_LACTATE, H_LAST_RDW, H_LAST_LYMPHOCYTES,
    CH_LAST_LD, ICD9_0389, DIAGNOSES_NUM, BG_LAST_CALCULATED_TOTAL_CO2, CH_LAST_GLUCOSE,
    INVASIVE_VENTILATION, BG_LAST_TIDAL_VOLUME, H_LAST_PLATELET_COUNT, H_LAST_WHITE_BLOOD_CELLS, CH_LAST_UREA_NITROGEN,
    ICD9_431, ICD9_41401, BG_LAST_PO2, CH_LAST_BILIRUBIN_TOTAL, CH_LAST_MAGNESIUM, BG_LAST_PCO2, BG_LAST_PEEP,
    CH_LAST_ANION_GAP, H_LAST_PTT, BG_LAST_OXYGEN, CH_LAST_SODIUM, BG_LAST_ALVEOLAR_ARTERIAL_GRADIENT, H_LAST_MCHC,
    CH_LAST_TROPONIN_T, BG_LAST_TEMPERATURE, BG_LAST_REQUIRED_O2, H_LAST_INR_PT, H_LAST_MCV, EXTUBATION, BG_LAST_PH,
    H_LAST_FIBRINOGEN_FUNCTIONAL, CH_LAST_CREATININE, H_LAST_PT
) {
  # Create a one-row data frame with the provided parameters.
  # For features with special characters (like a period), use backticks.
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
  
  # Convert the data frame to xgb.DMatrix format
  dmatrix <- xgb.DMatrix(data = as.matrix(input_data))
  
  # Load the model
  model <- load_model()
  
  # Generate predictions (this returns a probability from 0 to 1)
  preds <- predict(model, dmatrix)
  
  return(preds)
}

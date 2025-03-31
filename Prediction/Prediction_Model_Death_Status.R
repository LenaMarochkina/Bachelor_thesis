# Load the necessary package
library(xgboost)

# Function to load the saved XGBoost model
load_model <- function() {
  readRDS("../data/prepared_to_prediction/Models/final_xgb_model_shap.rds")
}

predict_survival <- function(ATC_N02AA, AGE_AT_ADMISSION, ICD9_51881, PRESCRIPTIONS_NUM,
                             ATC_C01CA, BG_LAST_PH, ATC_C09AA, ATC_B01AB, ATC_B01AA,
                             H_LAST_INR_PT, CH_LAST_LD, ICD9_0389, BG_LAST_LACTATE,
                             H_LAST_RDW, CH_LAST_TROPONIN_T, H_LAST_LYMPHOCYTES, DIAGNOSES_NUM,
                             BG_LAST_TIDAL_VOLUME, ATC_N01AH, CH_LAST_ANION_GAP, INVASIVE_VENTILATION,
                             CH_LAST_MAGNESIUM, H_LAST_PLATELET_COUNT, ICD9_431, BG_LAST_TEMPERATURE,
                             CH_LAST_GLUCOSE, ICD9_41401, BG_LAST_ALVEOLAR_ARTERIAL_GRADIENT,
                             ADMISSION_TYPE_EMERGENCY, H_LAST_MCHC) {
  
  # Create a one-row data frame using the provided parameters and matching the feature names
  input_data <- data.frame(
    ATC_N02AA                           = as.numeric(ATC_N02AA),
    AGE_AT_ADMISSION                    = as.numeric(AGE_AT_ADMISSION),
    ICD9_51881                          = as.numeric(ICD9_51881),
    PRESCRIPTIONS_NUM                   = as.numeric(PRESCRIPTIONS_NUM),
    ATC_C01CA                           = as.numeric(ATC_C01CA),
    BG_LAST_PH                          = as.numeric(BG_LAST_PH),
    ATC_C09AA                           = as.numeric(ATC_C09AA),
    ATC_B01AB                           = as.numeric(ATC_B01AB),
    ATC_B01AA                           = as.numeric(ATC_B01AA),
    H_LAST_INR_PT                       = as.numeric(H_LAST_INR_PT),
    CH_LAST_LD                          = as.numeric(CH_LAST_LD),
    ICD9_0389                           = as.numeric(ICD9_0389),
    BG_LAST_LACTATE                     = as.numeric(BG_LAST_LACTATE),
    H_LAST_RDW                          = as.numeric(H_LAST_RDW),
    CH_LAST_TROPONIN_T                  = as.numeric(CH_LAST_TROPONIN_T),
    H_LAST_LYMPHOCYTES                  = as.numeric(H_LAST_LYMPHOCYTES),
    DIAGNOSES_NUM                       = as.numeric(DIAGNOSES_NUM),
    BG_LAST_TIDAL_VOLUME                = as.numeric(BG_LAST_TIDAL_VOLUME),
    ATC_N01AH                           = as.numeric(ATC_N01AH),
    CH_LAST_ANION_GAP                   = as.numeric(CH_LAST_ANION_GAP),
    INVASIVE_VENTILATION                = as.numeric(INVASIVE_VENTILATION),
    CH_LAST_MAGNESIUM                   = as.numeric(CH_LAST_MAGNESIUM),
    H_LAST_PLATELET_COUNT               = as.numeric(H_LAST_PLATELET_COUNT),
    ICD9_431                            = as.numeric(ICD9_431),
    BG_LAST_TEMPERATURE                 = as.numeric(BG_LAST_TEMPERATURE),
    CH_LAST_GLUCOSE                     = as.numeric(CH_LAST_GLUCOSE),
    ICD9_41401                          = as.numeric(ICD9_41401),
    BG_LAST_ALVEOLAR_ARTERIAL_GRADIENT  = as.numeric(BG_LAST_ALVEOLAR_ARTERIAL_GRADIENT),
    ADMISSION_TYPE.EMERGENCY            = as.numeric(ADMISSION_TYPE_EMERGENCY),
    H_LAST_MCHC                         = as.numeric(H_LAST_MCHC)
  )
  
  # Convert the data frame to xgb.DMatrix format
  dmatrix <- xgb.DMatrix(data = as.matrix(input_data))
  
  # Load the saved model
  model <- load_model()
  
  # Generate predictions
  preds <- predict(model, dmatrix)
  
  return(preds)
}

# Bachelor_thesis
Enhancing Clinical Decision-Making: Predictive Models for Mortality and Survival in Critical Care

## 1. Data Understanding and Preprocessing

### 1.2. Data Exploration
- **Objective**: To understand the structure and contents of each table.
- **Steps**:
  - Explore the schema of each table.
  - Identify key variables: e.g., mortality, demographic factors, and clinical data.
  - Check for missing data, outliers, and inconsistencies.
- **Tools**: R.
- **Estimated Time**: 2 weeks
- **Deadline Date**: 23-Sep-2024

### 1.3. Data Cleaning
- **Objective**: Prepare the dataset for analysis.
- **Steps**:
  - Handle missing values: Imputation or deletion.
  - Remove duplicates.
  - Normalize and standardize relevant variables (e.g., age, lab results).
  - Create derived variables if needed (e.g., ICU stay duration).
- **Tools**: R.
- **Estimated Time**: 2 weeks
- **Deadline Date**: 7-Oct-2024

## 2. Descriptive Statistical Analysis
- **Objective**: Summarize the dataset to identify general patterns and trends.
- **Steps**:
  - **Demographic Analysis**:
    - Calculate statistics for age, gender, ethnicity, etc.
    - Analyze the distribution of admission types and locations.
  - **Clinical Data Analysis**:
    - Summarize lab results (mean, median, range).
    - Frequency analysis of diagnoses (using ICD codes) and procedures.
    - Analyze ICU stays (LOS, care units, admission types).

- **Visualizations**:
  - Histograms, bar charts, and pie charts for categorical variables.
  - Box plots for continuous variables.
  
- **Example of table for numerical variables**:

  | Variable | Count | Mean | SD   | Min | Max | Q1  | Median | Q3  |
  |----------|-------|------|------|-----|-----|-----|--------|-----|
  | age      | 5231  | 65.2 | 12.5 | 18  | 90  | 58  | 65     | 75  |

- **Example of table for categorical variables**:

  | Variable | Category | Count | Percentage |
  |----------|----------|-------|------------|
  | Gender   | Male     | 2950  | 56.4%      |
  | Gender   | Female   | 2281  | 43.6%      |

- **Tools**: R (ggplot2).
- **Estimated Time**: 4 weeks
- **Deadline Date**: 4-Nov-2024

## 3. Statistical Modeling and Dependence Analysis
- **Objective**: To identify factors that are significantly associated with patient outcomes (e.g., mortality, survival days).
- **Steps**:
  - **Correlation Analysis**:
    - Use Pearson/Spearman correlation to identify relationships between continuous variables (e.g., lab results and survival days).
    - Use Chi-square tests for associations between categorical variables (e.g., gender, ethnicity) and mortality.
  - **Regression Analysis**:
    - Logistic Regression: To identify factors that influence the probability of patient mortality (HOSPITAL_EXPIRE_FLAG).
    - Cox Proportional Hazards Model: To analyze survival time and the impact of clinical factors on it.
    - Linear Regression: To examine the relationship between clinical factors and the number of days alive post-discharge.
  
- **Tools**: R.
- **Estimated Time**: 6 weeks
- **Deadline Date**: 16-Dec-2024

## 4. Predictive Analytics
- **Objective**: Develop predictive models to assist doctors in clinical decision-making.
- **Steps**:
  - **Model Selection**:
    - Random Forest: For mortality prediction based on clinical variables.
  - **Model Training and Validation**:
  - **Feature Importance**: Identify the most influential factors for predictions.
  
- **Tools**: Python.
- **Estimated Time**: 8-10 weeks
- **Deadline Date**: 7-Apr-2024

## 5. Writing the Bachelor Thesis Text
- **Tools**: Latex
- **Estimated Time**: 26 weeks
  - Initial Drafting: 22 weeks (concurrent with Steps 1 to 4)
  - Revision and Finalization: 4 weeks (after completion of Steps 1 to 4)
  
- **Deadline Date**: 5-May-2024

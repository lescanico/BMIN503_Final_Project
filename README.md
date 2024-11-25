# A Data-Driven Approach to Predicting and Optimizing Outpatient Psychiatry Resource Utilization

This project investigates psychiatry resource utilization at the Penn Behavioral Health (PBH) Outpatient Psychiatry Clinic (OPC), with a primary goal of developing a predictive model to inform and optimize mental health resource allocation. The analysis integrates datasets from Epic Analytics, implementing advanced data preprocessing, feature engineering, and modeling techniques.

## Repository Structure {#sec-repository-structure}

### **`datasets/`** {#sec-datasets}

-   Datasets required to reproduce analysis:
    -   `patient_data_anonymized.rds`: Anonymized patient data.
    -   `visit_data_anonymized.rds`: Anonymized visit data.
    -   `variable_type_lookup.rds`: Lookup table mapping dataset variables to their corresponding data types for dynamic type conversion.

### **`scripts/`** {#sec-scripts}

-   R scripts for data processing, analysis, and modeling:
    -   `data_preprocessing.R`: Code for standardizing column names, handling type conversions dynamically using `variable_type_lookup.rds`, and saving cleaned datasets.
    -   `data_integration.R`: Code to merge the patient and visit datasets using shared keys like patient_id. It ensures alignment, resolves duplicate columns, and saves the integrated dataset.
    -   `missing_data_analysis.R`: Code to analyze missing data patterns an generates summary statistics and visualizations to understand data completeness.
    -   `missing_data_handling.R`: Code to plan missing data handling based on strategies informed by the missing data analysis and `variable_type_lookup.rds`.

### **`figures/`** {#sec-figures}

-   Visualizations and graphics generated during data analysis and modeling.

### **`README.md`**

-   Documentation of the repository structure and purpose.

## Key Features {#sec-key-features}

1.  **Dynamic Data Type Handling**:
    -   Type conversions are performed dynamically using `variable_type_lookup.rds`, which maps variables to their appropriate data types.
2.  **Anonymization**:
    -   Patient and visit data are anonymized to comply with privacy standards.
3.  **Comprehensive Data Preprocessing**:
    -   Column names are standardized, unnecessary fields are removed, and datasets are aligned for consistency.
4.  **Feature Engineering**:
    -   Creation of high-impact features such as the Resource Utilization Score (RUS).
5.  **Predictive Modeling**:
    -   Training and evaluation of multiple models, including linear regression, logistic regression, and advanced machine learning methods.

## How to Run {#sec-how-to-run}

To reproduce the analysis, follow these steps:

1.  Clone the repository and navigate to the project directory.
2.  Open the R project or launch the R scripts in RStudio.
3.  Run the scripts in the order provided under @sec-scripts

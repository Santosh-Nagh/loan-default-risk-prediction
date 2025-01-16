# Predicting Loan Default Risk Using Machine Learning

## Overview
This project leverages advanced machine learning techniques to predict loan default risks for financial institutions, providing actionable insights to improve credit risk management. By analyzing key predictors such as external credit scores and debt-to-income ratios, the project demonstrates the power of predictive modeling in enhancing decision-making processes.

## Key Features
- **Tools**: R, SQL, Tableau.
- **Techniques**: Logistic Regression, Random Forest, Decision Trees, Feature Engineering.
- **Results**: Achieved an AUC of 0.9905 and 98.18% accuracy using Random Forest models.

## Repository Structure
- **`documents/`**: Contains detailed reports about the project.
  - `Loan Default Risk Prediction.pdf`: Final project report summarizing methodology, findings, and recommendations.
  - `Loan Default Risk Case Study.docx`: A more detailed case study version of the project.
- **`scripts/`**: Includes code used for data processing and modeling.
  - `R_loan_default_risk_prediction.R`: R script for data preprocessing, feature engineering, and predictive modeling.
- **`visuals/`** (optional): Charts and graphs generated from the analysis (if applicable).
- **`data/`** (optional): Placeholders for datasets or links to external dataset sources (datasets are not included due to size constraints).

## Usage Instructions
1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/loan-default-risk-prediction.git
   ```
2. Navigate to the `scripts/` folder and run the `R_loan_default_risk_prediction.R` file in RStudio.
3. View the results and outputs in the `documents/` folder.

## Datasets
The datasets used in this project are not included in the repository due to size constraints. However, the dataset can be accessed at the following link:

- [Loan Defaulter Dataset on Kaggle](https://www.kaggle.com/datasets/gauravduttakiit/loan-defaulter?select=application_data.csv)

### Dataset Overview:

1. **Application Data**:
   - **Source**: [Kaggle Dataset](https://www.kaggle.com/datasets/gauravduttakiit/loan-defaulter?select=application_data.csv)
   - **Columns**: Includes `SK_ID_CURR`, `EXT_SOURCE_1`, `EXT_SOURCE_2`, `DAYS_EMPLOYED`, etc.

2. **Previous Application Data**:
   - **Source**: [Kaggle Dataset](https://www.kaggle.com/datasets/gauravduttakiit/loan-defaulter?select=application_data.csv)
   - **Columns**: Includes `SK_ID_PREV`, `NAME_CONTRACT_TYPE`, `AMT_CREDIT`, etc.

If required, similar datasets can be used by modifying the input paths in the scripts.

## Recommendations
1. **Automated Risk-Based Screening**: Develop predictive models using machine learning to automate the screening process for loan applicants, focusing on identifying high-risk borrowers effectively.
2. **Tailored Loan Offers**: Use segmentation to group borrowers into low, medium, and high-risk categories, enabling financial institutions to personalize loan terms, interest rates, and credit offers based on risk profiles.
3. **Improved Credit Risk Metrics**: Integrate additional predictive variables, such as payment history and income volatility, to enhance model accuracy and decision-making.
4. **Behavioral and Transactional Data**: Expand data collection to include behavioral patterns and transaction histories to better predict default probabilities and refine borrower profiles.

## Contact
**Santosh Nagh Sirimalla**  
[Your LinkedIn Profile](https://www.linkedin.com/in/santoshnaghsirimalla/)  
[Your Email](mailto:santoshnagh1@gmail.com)  

# Credit-Risk-Analysis
This repository attempts to carryout a credit risk analysis using a kaggle dataset (credit_data.csv in this repository). The data can also be found at this link https://www.kaggle.com/datasets/surekharamireddy/credit-data. 

In the analysis, I employed the methods of the LASSO and Random forests to predict the variable default. Prior to analysis, the data was heavily imbalanced and the SMOTE technique was used to try balance the data. The LASSO yielded a 66% accuracy whereas the Random Forests yielded a 90% accuracy. The models however need improvement, the AUCs are relatively low at 0.68 and 0.51 for the LASSO and Random Forests respectively. 

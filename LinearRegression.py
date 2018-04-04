## Linear regression

import pandas as pd;
import numpy as np;
from sklearn import linear_model;
from sklearn.model_selection import train_test_split

##Reading from xls
lm_data = pd.ExcelFile("C:\\Personnel\\DummyData\\linear-regression.xlsx").parse("Interest rates and home prices");
print(lm_data.head(10));

##Data Cleaning
lm_data = lm_data.rename(columns={'interest rate (%)': 'interest rate',});

## Splitting data into test and train
train_data, test_data = train_test_split(lm_data, test_size=0.2);

##Modelling as linear regression

lm_model = linear_model.LinearRegression();
lm_model.fit(train_data[['interest rate']], train_data[['Median home price']]);

predicted= lm_model.predict(test_data[['interest rate']]);
#print('Intercept: \n', lm_model.intercept_)
#print('Coefficient: \n', lm_model.coef_)
print(predicted)
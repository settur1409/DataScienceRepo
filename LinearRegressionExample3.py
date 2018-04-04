## Linear regression-3

import pandas as pd;
import numpy as np;
from sklearn import linear_model;
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt
import statsmodels.formula.api as smf

lm_data = pd.ExcelFile("C:\\Personnel\\DummyData\\linear-regression.xlsx").parse("Interest rates and home prices");
lm_data = lm_data.rename(columns={'interest rate': 'ROI', 'Median home price':'Price'});
train_data, test_data = train_test_split(lm_data, test_size=0.3);

lm1 = smf.ols(formula='ROI ~ Price', data=lm_data).fit();
print(lm1.params);
print(lm1.conf_int())
print(lm1.pvalues)

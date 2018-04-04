## logistic regression

import pandas as pd;
import numpy as np;
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt
import statsmodels.api as sm

Male = 1;
Female = 0;

#male_data = np.random.randint(100, 150, size = 20);
#female_data = np.random.randint(100, 120, size = 20);


height_data = np.ndarray(20);
true_false_data = np.ndarray(20);
height_data = np.random.randint(100, 150, size = 20);
##true_false_data = ((height_data > 120) and (height_data < 151))
i = 0
for i in range(20):
    true_false_data[i] = Male if (height_data[i] > 120 and height_data[i] < 151) else Female; 

##height_data2 = np.ndarray(20,2)
##np.where((height_data > 120) & (height_data < 151))
height_df = pd.DataFrame({'Height':height_data, 'Sex':true_false_data});

train_data, test_data = train_test_split(height_df, test_size=0.3);
#logistic_model = LogisticRegression();
#logistic_model.fit(train_data[['Height']], train_data[['Sex']]);
#logistic_model.score(train_data[['Height']], train_data[['Sex']])

#predicted= logistic_model.predict(test_data[['Height']])

logit_mod = sm.logit(train_data[['Sex']], train_data[['Height']])
result = logit_mod.fit()
print(result.summary())


#plt.plot(train_data[['Height']], train_data[['Sex']])
#test_prediction = logistic_model.predict(train_data[['Height']]);
#print(test_prediction);
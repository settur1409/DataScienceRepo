## Linear regression Example-2
import pandas as pd;
import numpy as np;
from sklearn import linear_model;
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt


x_data = np.arange(50)
y_data = x_data + 2;

num_frame = pd.DataFrame({'x_val':x_data, 'y_val':y_data});

#print(num_frame[['x_val']]);
#print(num_frame[['y_val']]);

plt.plot(num_frame[['x_val']], num_frame[['y_val']])
train_data, test_data = train_test_split(num_frame, test_size=0.2);
lm_model = linear_model.LinearRegression();
lm_model.fit(train_data[['x_val']], train_data[['y_val']]);
predicted= lm_model.predict(test_data[['x_val']]);
print(predicted)
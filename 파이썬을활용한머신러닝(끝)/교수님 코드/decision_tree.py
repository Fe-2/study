# Load libraries
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import itertools
from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import train_test_split
from sklearn import metrics
from sklearn.tree import export_graphviz
from six import StringIO
import pydotplus


# Load Dataset
diab = pd.read_csv('diabetes.csv')     # https://www.kaggle.com/uciml/pima-indians-diabetes-database


# Simple Data Analysis
print(diab.isnull().sum())
sns.countplot(x='Outcome', data=diab)
plt.show()

columns = diab.columns[:8]
plt.subplots(figsize=(18, 15))
length = len(columns)
for i, j in itertools.zip_longest(columns, range(length)):
    plt.subplot(int(length/2), 3, j+1)
    plt.subplots_adjust(wspace=0.2, hspace=0.5)
    diab[i].hist(bins=20, edgecolor='black')
    plt.title(i)
plt.show()


# Data split (Train 75%, Test 25%)
train, test = train_test_split(diab, test_size=0.25, random_state=0, stratify=diab['Outcome'])
print(list(train.columns))

train_X = train[train.columns[:8]]
test_X  = test[test.columns[:8]]
train_Y = train['Outcome']
test_Y  = test['Outcome']

print(train_X.head(2))
print(train_Y.head(2))


# Decision Tree
model = DecisionTreeClassifier()
model.fit(train_X, train_Y)
prediction = model.predict(test_X)
print('The accuracy of the Decision Tree is', metrics.accuracy_score(prediction, test_Y))


# Visualize Tree
dot_data = StringIO()
export_graphviz(model, out_file=dot_data,
                filled=True, rounded=True,
                special_characters=True, feature_names=list(train.columns[:8]), class_names=['0', '1'])
graph = pydotplus.graph_from_dot_data(dot_data.getvalue())
graph.write_png('diabetes.png')

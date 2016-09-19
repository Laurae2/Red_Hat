# Requires to be completed!!! (file export, etc.)
# Super slow due to sparse matrix usage (brute forced NN)

import pandas as pd
import numpy as np
import os
from sklearn.preprocessing import LabelEncoder
from scipy.sparse import csr_matrix, hstack
import datetime
import random
from sklearn.neighbors import NearestNeighbors

#%%

os.chdir("D:/Data Science/Red Hat")
random.seed(11111)

#%%

people = pd.read_csv('people.csv', index_col='people_id')

#%%

ppl = people

def binarizer(ppled, column):
    ppled[column] = LabelEncoder().fit_transform(ppled[column])
    temp = ppled[column]
    return csr_matrix((np.ones(temp.shape[0]), 
                       (list(range(0, 189118)), temp)))

ppl.char_1 = LabelEncoder().fit_transform(ppl.char_1)
char_1 = ppl['char_1']
char_1 = csr_matrix((np.ones(char_1.shape[0]), 
                       (list(range(0, 189118)), char_1)))

#%%

ppl = people

sparsed = hstack((binarizer(ppled = ppl, column = "char_1"),
                  binarizer(ppled = ppl, column = "group_1"),
                  binarizer(ppled = ppl, column = "char_2"),
                  binarizer(ppled = ppl, column = "char_3"),
                  binarizer(ppled = ppl, column = "char_4"),
                  binarizer(ppled = ppl, column = "char_5"),
                  binarizer(ppled = ppl, column = "char_6"),
                  binarizer(ppled = ppl, column = "char_7"),
                  binarizer(ppled = ppl, column = "char_8"),
                  binarizer(ppled = ppl, column = "char_9")))

#%%

mini = np.zeros((189118, 28))
mini[:, :] = ppl.ix[:, 11:39].copy()

sparsed = hstack((sparsed,
                  mini))

#%%

mini = np.zeros((189118, 2))
temp_date = ppl.ix[:, 3].copy().str.extract('^(?P<year>.{4}).{1}(?P<month>\d{2}).{1}(?P<day>.{2})').apply(pd.to_numeric)
mini[:, 0] = 365*(temp_date.ix[:, 0]-min(temp_date.ix[:, 0])) + 30*(temp_date.ix[:, 1]) + (temp_date.ix[:, 2])
mini[:, 1] = ppl.ix[:, 39].copy()
mini[:, 0] = (mini[:, 0] - min(mini[:, 0])) / (max(mini[:, 0]) - min(mini[:, 0]))
mini[:, 1] = (mini[:, 1] - min(mini[:, 1])) / (max(mini[:, 1]) - min(mini[:, 1]))
sparsed = hstack((sparsed,
                  mini))

#%%

sparsed = sparsed.tocsr()

#%%

nbrs = NearestNeighbors(n_neighbors = 1, algorithm = 'brute', p = 1) # auto "brute" with sparse
neighs = nbrs.fit(sparsed)
predict = neighs.kneighbors(X = sparsed[0:1], n_neighbors = 8, return_distance = True)

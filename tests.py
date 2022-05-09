#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu May  5 14:55:46 2022

@author: amandaramirez
"""

import pandas as pd
from scipy import stats
import matplotlib.pyplot as plt
import numpy as np

data = pd.read_excel('true_pairs.xlsx')

#%%

yields = data[['Yield ask, at issue_SLB','Yield ask, yas_CB']].copy()
yields.columns=['SLB Yields','CB Yields']
yields['Difference'] = yields['SLB Yields'] - yields['CB Yields']

desc_stats = yields.describe()

plt.boxplot(np.array(yields.iloc[:,:2]))

plt.hist(np.array(yields.iloc[:,:1]))
plt.hist(np.array(yields.iloc[:,1]))


stats.probplot(yields['SLB Yields'], dist="norm", plot=plt)


stats.wilcoxon(yields['SLB Yields'], yields['CB Yields'])

#%%

#OLS regression 
data['Yield difference'] = data['Yield ask, at issue_SLB']-data['Yield ask, yas_CB']

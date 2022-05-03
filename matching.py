#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue May  3 21:52:18 2022

@author: amandaramirez
"""

import pandas as pd
import datetime as datetime

slb = pd.read_excel("slb updated.xlsx", sheet_name=0, decimal =',')
cb = pd.read_excel("slb updated.xlsx", sheet_name=2, decimal = ',')

slb['Issue Date'] = pd.to_datetime(slb['Issue Date'])
slb['Maturity'] = pd.to_datetime(slb['Maturity'], errors="coerce")
cb['Issue Date'] = pd.to_datetime(cb['Issue Date'])
cb['Maturity'] = pd.to_datetime(cb['Maturity'], errors="coerce")

#%%

pairs_l = []

for i in cb.iterrows():
    for j in slb.iterrows():
        if j[1][-1] == i[1][-2]:
            if (j[1].loc['Issue Date'] - i[1].loc['Issue Date'] < datetime.timedelta(1825)) \
                &  (j[1].loc['Maturity'] - i[1].loc['Maturity'] < datetime.timedelta(1095)):
                if (i[1].loc['Amt Issued'] < j[1].loc['Amt Issued']* 4) and (i[1].loc['Amt Issued'] > j[1].loc['Amt Issued']* 0.25):
                    pairs_l.append([j[1]['ID'], i[1]['ID']])

#%%

pairs = pd.DataFrame(data=pairs_l,columns=['slb_id','cb_id'])

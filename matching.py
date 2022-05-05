#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue May  3 21:52:18 2022

@author: amandaramirez
"""

import pandas as pd
import datetime as datetime

slb = pd.read_excel("slb updated.xlsx", sheet_name=0)
cb = pd.read_excel("slb updated.xlsx", sheet_name=1)


slb['Issue Date'] = pd.to_datetime(slb['Issue Date'])
slb['Maturity'] = pd.to_datetime(slb['Maturity'], errors="coerce")
slb['Amt Issued'] = pd.to_numeric(slb['Amt Issued'], errors="coerce")
cb['Issue Date'] = pd.to_datetime(cb['Issue Date'])
cb['Maturity'] = pd.to_datetime(cb['Maturity'], errors="coerce")
cb['Amt Issued'] = pd.to_numeric(cb['Amt Issued'], errors="coerce")


#%%

pairs_l = []

for i in cb.iterrows():
    for j in slb.iterrows():
        if j[1]['Issuer&MTY&CPN&CURR&Seniority'] == i[1]['Issuer&MTY&CPN&CURR&Seniority']:
            if (j[1].loc['Issue Date'] - i[1].loc['Issue Date'] < datetime.timedelta(1825)) \
                &  (j[1].loc['Maturity'] - i[1].loc['Maturity'] < datetime.timedelta(1095)):
                if (i[1].loc['Amt Issued'] < j[1].loc['Amt Issued']* 4) and (i[1].loc['Amt Issued'] > j[1].loc['Amt Issued']* 0.25):
                    pairs_l.append([j[1]['ID'], i[1]['ID']])

#%%

all_pairs = pd.DataFrame(data=pairs_l,columns=['slb_id','cb_id'])

all_pairs.to_excel('all_pairs.xlsx')


#%%

def in_boundaries(slb, cb):
    if (slb[1].loc['Issue Date'] - cb[1].loc['Issue Date'] < datetime.timedelta(1825)) \
        &  (slb[1].loc['Maturity'] - cb[1].loc['Maturity'] < datetime.timedelta(1095)):
            if (cb[1].loc['Amt Issued'] < slb[1].loc['Amt Issued']* 4) and (cb[1].loc['Amt Issued'] > slb[1].loc['Amt Issued']* 0.25):
                return True

pairs_d = {}

for i in cb.iterrows():
    for j in slb.iterrows():
        if j[1]['Issuer&MTY&CPN&CURR&Seniority'] == i[1]['Issuer&MTY&CPN&CURR&Seniority']:
            if j[1]['ID'] in pairs_d.keys():
                count = {'Issue Date':0,
                         'Maturity':0,
                         'Amt Issued':0}
                if (j[1].loc['Issue Date'] - pairs_d[j[1]['ID']][4]) > (j[1].loc['Issue Date'] - i[1].loc['Issue Date']):
                    count['Issue Date'] = 1
                if (j[1].loc['Maturity'] - pairs_d[j[1]['ID']][5]) > (j[1].loc['Maturity'] - i[1].loc['Maturity']):
                    count['Maturity'] = 1
                if (j[1].loc['Amt Issued'] - pairs_d[j[1]['ID']][6]) > (j[1].loc['Amt Issued'] - i[1].loc['Amt Issued']):
                    count['Amt Issued'] = 1
                if sum(count.values()) == 3 or (sum(count.values()) == 2 and in_boundaries(j,i)):
                    pairs_d[j[1]['ID']] = [j[1]['Issue Date'], j[1]['Maturity'],j[1]['Amt Issued'], i[1]['ID'],i[1]['Issue Date'],i[1]['Maturity'],i[1]['Amt Issued']]

            else:
                if in_boundaries(j,i):
                    pairs_d[j[1]['ID']] = [j[1]['Issue Date'], j[1]['Maturity'],j[1]['Amt Issued'], i[1]['ID'],i[1]['Issue Date'],i[1]['Maturity'],i[1]['Amt Issued']]

true_pairs = pd.DataFrame(data=pairs_d).T
true_pairs.reset_index(inplace=True)
true_pairs.columns=['SLB ID','SLB Issue Date','SLB Maturity Date','SLB Amt Issued','CB ID','CB Issue Date','CB Maturity Date','CB Amt Issued']


        
        
        
        
        
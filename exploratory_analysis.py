#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu May  5 17:34:40 2022

@author: amandaramirez
"""

import pandas as pd
import datetime as datetime


slb = pd.read_excel("slb updated.xlsx", sheet_name=0)
slb.dropna(subset=['Yield ask, at issue'],inplace=True)
slb['Issue Date'] = pd.to_datetime(slb['Issue Date'])
slb['Maturity'] = pd.to_datetime(slb['Maturity'], errors="coerce")
slb['Amt Issued'] = pd.to_numeric(slb['Amt Issued'], errors="coerce")



# This script runs regressions for the Cincinnati FC paper

# Importing required modules

import pandas as pd

# Directory info

username = ''
direc = 'C:/Users/' + username + '/Documents/Data/cincinnati/'

# Loading data

data = pd.read_csv(direc + 'data/house_transactions.csv')
fred = pd.read_csv(direc + 'data/fred.csv')

# Data prep

# Remove observations with any missing data

data = data.dropna().reset_index(drop = True)

# Calculate real house prices

# Get month/year for each transaction

def my_date(inp):
    
    a = inp.find('/')
    value = inp[:a] + inp[-5:]
    
    if value == '4/2022':
        
        value = '3/2022'
    
    return value

my = [my_date(d) for d in data['Transfer Date']]

# Get month/year for fred data

my_fred = [my_date(d) for d in fred.DATE]

# Convert prices to real dollars

# Conversion factors from https://fred.stlouisfed.org/series/CPIAUCSL with January 2020 as reference month

def get_real(idx):
    
    fid = my_fred.index(my[idx])
    fake = fred.Ratio[fid] * data.Price[idx]
    
    return fake

real_prices = [get_real(i) for i in range(len(data))]

# Add real prices to the data

data = pd.concat([data, pd.Series(real_prices, name = 'Real Price')], axis = 1)

# Save to file

data.to_csv(direc + 'data/real_house_prices.csv', index = False)


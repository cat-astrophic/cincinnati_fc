# This script scrapes the school district each house resides in from the Auditor's website

# Importing required modules

import pandas as pd
import urllib
from bs4 import BeautifulSoup as bs

# Directory info

username = ''
direc = 'C:/Users/' + username + '/Documents/Data/cincinnati/'

# Reading in the house transactions data

df = pd.read_csv('C:/Users/' + username + '/Documents/Data/cincinnati/data/house_transactions_for_scraping.csv')

# Url base

base = 'https://wedge1.hcauditor.org/view/re/'

# Initializing the storage list

districts = []
deed_type = []
acreage = []
owner_live = []
foreclosure = []

# Main loop

for i in range(len(df)):
    
    print('Scraping additional attributes for house ' + str(1+i) + ' of 174813.......') # Visualize progess
    
    try:
        
        url = base + df['Parcel Number'][i].replace('-','') + '/2021/summary'
        page = urllib.request.Request(url, headers = {'User-Agent': 'Mozilla/5.0'})  # Go to the url and get some data
        response = urllib.request.urlopen(page) # Go to the url and get some data
        soup = bs(response, 'html.parser') # Go to the url and get some data
        data = soup.find_all('tr') # Get the correct type of data
        
        try:
            
            infos = data[1].find_all('div')
            districts.append(str(infos[3])[str(infos[3]).index('">')+2:-6])
            
        except:
            
            districts.append(None)
            
        try:
            
            infos2 = data[15].find_all('td')
            deed_type.append(str(infos2[1])[4:-5])
            
        except:
            
            deed_type.append(None)
            
        try:
            
            infos3 = data[18].find_all('td')
            acreage.append(float(str(infos3[1])[4:-5]))
            
        except:
            
            acreage.append(None)
            
        try:
            
            infos4 = data[23].find_all('td')
            owner_live.append(str(infos4[1])[4:-5]) # based on receiving the Owner Occupancy Credit
            
        except:
            
            owner_live.append(None)
            
        try:
            
            infos5 = data[24].find_all('td')
            foreclosure.append(str(infos5[1])[4:-5])
            
        except:
            
            foreclosure.append(None)
            
    except:
        
        continue
    
# Merging this to the main dataframe

districts = pd.Series(districts, name = 'School District')
deed_type = pd.Series(deed_type, name = 'Deed Type')
acreage = pd.Series(acreage, name = 'Acreage')
owner_live = pd.Series(owner_live, name = 'Owner Residence')
foreclosure = pd.Series(foreclosure, name = 'Foreclosure')

df = pd.concat([df, districts, deed_type, acreage, owner_live, foreclosure], axis = 1)

# Write the dataframe to csv

df.to_csv('C:/Users/' + username + '/Documents/Data/cincinnati/data/house_transactions.csv', index = False)


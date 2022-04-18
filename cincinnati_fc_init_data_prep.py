# This script merges the cincinnati house transactions raw data files

# Importing required modules

import pandas as pd
import glob
from geopy.distance import geodesic
from geopy.geocoders import Nominatim

# Directory specification

username = ''
filepath = 'C:/Users/' + username + '/Documents/Data/cincinnati/data/raw_sales_data/'

# Create a list of all files to merge

file_list = glob.glob(filepath + '*')

# Initialize a dataframe

df = pd.DataFrame()

# Merging the data

for f in file_list:
    
    tmp = pd.read_csv(f)
    df = pd.concat([df, tmp], axis = 0).reset_index(drop = True)

# Defining a function to parse the BBB columns into meaningful data

def BBB_parser(inp):
    
    # Get the total number of rooms
    
    idx1 = inp.find('-')
    xrooms = inp[:idx1-1]
    
    # Get the number of bedrooms
    
    inp = inp[idx1+2:]
    idx2 = inp.find('-')
    xbedrooms = inp[:idx2-1]
    
    # Get the number of full baths
    
    inp = inp[idx2+2:]
    idx3 = inp.find('-')
    xfullbaths = inp[:idx3-1]
        
    # Get the number of half baths
    
    inp = inp[idx3+2:]
    xhalfbaths = inp
    
    # Aggregate results into a list and return the list
    
    outvec = [xrooms, xbedrooms, xfullbaths, xhalfbaths]
    
    return outvec

# Initializing lists to contain the parsed BBB data

rooms = []
bedrooms = []
fullbaths = []
halfbaths = []

# Using the BBB_parser function to create the house rooms data

for i in range(len(df)):
    
    out = BBB_parser(df.BBB[i])
    rooms.append(out[0])
    bedrooms.append(out[1])
    fullbaths.append(out[2])
    halfbaths.append(out[3])

# Adding this data to the main dataframe

rooms = pd.Series(rooms, name = 'Rooms')
bedrooms = pd.Series(bedrooms, name = 'Bedrooms')
fullbaths = pd.Series(fullbaths, name = 'Full Baths')
halfbaths = pd.Series(halfbaths, name = 'Half Baths')

df = pd.concat([df, rooms, bedrooms, fullbaths, halfbaths], axis = 1)

# Dropping observations with missing addresses

df = df.drop([171447,171448], axis = 0).reset_index(drop = True)

# Creating a new address column

# Creating a helper function

def addy_helper(inp):
    
    if inp[-2:] == 'PW':
        
        addy = inp[:-2] + 'PKWY, Hamilton County, OH'
        
    elif inp[-2:] == 'TL':
        
        addy = inp[:-2] + 'TRAIL, Hamilton County, OH'
        
    else:
        
        addy = inp + ', Hamilton County, OH'
        
    return addy

# Using the function to update addresses

addresses = [addy_helper(df.Address[i]) for i in range(len(df))]

# Adding the new addresses to the dataframe

df = pd.concat([df,pd.Series(addresses, name = 'Addresses')], axis = 1)

# Creating variables for the (geodesic) distance from each stadium

# Storing the addresses of the two stadiums

nippert = 'Nippert Stadium, Cincinnati, OH 45221'
mercy = '689 US-50, Milford, OH 45150'

# Initializing a geolocator via Nominatim

geolocator = Nominatim(user_agent = 'geoapiExercises')

# Getting latitude and longitude coordinates for the stadia

nippert_coords = (geolocator.geocode(nippert).latitude,geolocator.geocode(nippert).longitude)
mercy_coords = (geolocator.geocode(mercy).latitude,geolocator.geocode(mercy).longitude)

# Getting latitude and longitude coordinates for each address

coords = []

for a in range(len(df)):
    
    print('Getting coordinates for address ' + str(1+a) + ' of 174813.......') # Visualize progess
    
    try:
        
        loc = geolocator.geocode(df.Addresses[a])
        coords.append((loc.latitude,loc.longitude))
            
    except:
        
        coords.append(None)

# Initializing lists for storing distance data

nippert_distance = []
mercy_distance = []

# Calculating geodesic distances

for c in coords:
    
    print('Calculating distances for address ' + str(1+coords.index(c)) + ' of 174813.......') # Visualize progess    
    nippert_distance.append(geodesic(c,nippert_coords).km) # Distance in km
    mercy_distance.append(geodesic(c,mercy_coords).km) # Distance in km

# Adding the distance variables to the dataframe

nippert_distance = pd.Series(nippert_distance, name = 'Nippert')
mercy_distance = pd.Series(mercy_distance, name = 'Mercy')
coordsx = pd.Series(coords, name = 'Coordinates')

df = pd.concat([df, nippert_distance, mercy_distance, coordsx], axis = 1)

# Calculating the age of the house at the time of the transaction (in years)

ages = [int(df['Transfer Date'][i][-4:]) - df['Year Built'][i] for i in range(len(df))]
negative_age_flag = [1 if a < 0 else 0 for a in ages]
ages2 = [a if a >= 0 else 0 for a in ages] # Some ages are negative for sales finalized before construction was completed

ages = pd.Series(ages, name = 'Actual Age')
ages2 = pd.Series(ages2, name = 'Zero Coded Age')
negative_age_flag = pd.Series(negative_age_flag, name = 'Negative Age Flag')

# Adding ages to the main dataframe

df = pd.concat([df, ages, ages2, negative_age_flag], axis = 1)

# Dropping observations that have any of: FinSqFt == 0; Rooms == 0; Full Baths == 0;

df = df[df.Rooms > 0].reset_index(drop = True)
df = df[df['Full Baths'] > 0].reset_index(drop = True)
df = df[df.FinSqFt > 0].reset_index(drop = True)

# Dropping observations for which Nominatim was not accurate / could not find coordinates - Nippert is central in Cincinnati so this location is chosen with a 50km radius

df = df[df.Nippert < 50].reset_index(drop = True)

# Converting Amount from string to numeric

amt = []

for a in df.Amount:
    
    try:
        
        amt.append(int(a.strip('$').replace(',','')))
        
    except:
        
        amt.append(a)

amt = pd.Series(amt, name = 'Price')
df = pd.concat([df, amt], axis = 1)
df = df.dropna(axis = 0).reset_index(drop = True)

# Write the dataframe to csv

df.to_csv('C:/Users/' + username + '/Documents/Data/cincinnati/data/house_transactions_for_scraping.csv', index = False)


# This is a web scraper that scrapes the current stock price of a given company. 
# This can be used to compile a dataset of stock prices, or as an input to an investment program.

import requests
from bs4 import BeautifulSoup
import pandas as pd
import datetime
import time

def to_seconds(date):
    return time.mktime(date.timetuple())

# Define the list of company symbols or tickers
company_symbols = ['AAPL', 'GOOGL', 'MSFT', 'AMZN']

# Define the period for historical data (in this example, the last 10 years)
end_date = datetime.date.today()
start_date = end_date - datetime.timedelta(days=3650)  # 10 years

# Initialize an empty list to store data for each company
data = []

# Loop through each company and scrape historical financial data
for symbol in company_symbols:
    url = f'https://discountingcashflows.com/company/{symbol}/balance-sheet-statement/quarterly/'
    
    # Define the query parameters for the historical data
    params = {
        'period1': int(to_seconds(start_date)),
        'period2': int(to_seconds(end_date)),
        'interval': '1d',  # Daily data
        'filter': 'history',
    }

    # Send an HTTP GET request with the query parameters
    response = requests.get(url, params=params)
    
    # Check if the request was successful
    if response.status_code == 200:
        soup = BeautifulSoup(response.text, 'html.parser')
        
        # Extract historical data table
        table = soup.find('table')
        if table:
            df = pd.read_html(str(table), header=0)[0]
            
            # Add the symbol as a new column
            df['Symbol'] = symbol
            
            # Append the data to the list
            data.append(df)
        else:
            print(f"No historical data found for {symbol}")
    else:
        print(f"Failed to fetch data for {symbol}")

# Create a DataFrame from the collected data
if data:
    historical_data = pd.concat(data, ignore_index=True)
    
    # Save the historical data to a CSV file
    historical_data.to_csv('historical_financial_data.csv', index=False)
    print("Historical data scraped and saved to 'historical_financial_data.csv'")
else:
    print("No data was scraped.")

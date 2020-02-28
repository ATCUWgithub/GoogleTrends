#TrendsScraper
#YahooFinanceScraper

import DataReader from pandas.io.data 
from datetime import datetime
goog = DataReader("GOOG",  "yahoo", datetime(2000,1,1), datetime(2012,1,1))
goog["Adj Close"]


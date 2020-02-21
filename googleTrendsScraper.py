from pytrends.request import TrendReq

hl='en-US' #Host language
searchTerms = ['apple','marvel','Lebron']
geo="US"
timeframe = 'today 5-y'
tz=300 #Timezone = offset in minutes from UTC, 300 min is EST (New York)

pytrend = TrendReq()
pytrend.get_historical_interest(searchTerms, year_start=2018, month_start=1, day_start=1, hour_start=0, year_end=2018, month_end=2, day_end=1, hour_end=0, cat=0, geo='', gprop='', sleep=0)

#pytrends = TrendReq(h1, tz)
#pytrends.build_payload(searchTerms, cat=0, timeframe, geo, gprop='')
#data = pytrends.get_historical_interest(kw_list, year_start=2018, month_start=1, day_start=1, hour_start=0, year_end=2018, month_end=2, day_end=1, hour_end=0, cat=0, geo='', gprop='')

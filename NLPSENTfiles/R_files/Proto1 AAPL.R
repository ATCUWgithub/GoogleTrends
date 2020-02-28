require(quantmod)

getSymbols(c("AAPL"), from="2014-11-09", to="2019-11-03", src = "yahoo", periodicity = "weekly")
TrendDat = read.table("AAPL Data.txt",header=TRUE) 
AAPL.logret = diff(as.vector(log(APPL$APPL.Adjusted)))

TrendDat

y = %change over day after quarter
x = Sum1 Q1, Sum1 Q2, Sum3 Q3 





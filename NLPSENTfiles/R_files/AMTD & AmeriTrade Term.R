library('quantmod')
library('corrplot')
getSymbols(c("AMTD"), from="2015-01-19", to="2020-01-17", src="yahoo", periodicity = 'weekly')

data = read.csv('AMTD Trend.csv', header = T)
AMTDT = data[,c("Week", "TDAmeritrade")]

##Earnings
AMTD[53]
AMTDT[223,]
x = c(2, 15, 28, 41, 54, 67, 80, 93, 106, 119, 132, 145, 158, 171, 184, 197, 210, 223, 236, 249, 261)
AMTDC = AMTD$AMTD.Close[x] ##Quarterly Dates
AMTDO = AMTD$AMTD.Open[x]
AMTDD = (AMTDC - AMTDO)/(AMTDO)
AMTDD #Move one week after earnings result

AMTDTE = c() #Trend Sums
AMTDTE[1] = sum(AMTDT$TDAmeritrade[1:x[1]])
AMTDTE[2] = sum(AMTDT$TDAmeritrade[x[1]:x[2]])
AMTDTE[3] = sum(AMTDT$TDAmeritrade[x[2]:x[3]])
AMTDTE[4] = sum(AMTDT$TDAmeritrade[x[3]:x[4]])
AMTDTE[5] = sum(AMTDT$TDAmeritrade[x[4]:x[5]])
AMTDTE[6] = sum(AMTDT$TDAmeritrade[x[5]:x[6]])
AMTDTE[7] = sum(AMTDT$TDAmeritrade[x[6]:x[7]])
AMTDTE[8] = sum(AMTDT$TDAmeritrade[x[7]:x[8]])
AMTDTE[9] = sum(AMTDT$TDAmeritrade[x[8]:x[9]])
AMTDTE[10] = sum(AMTDT$TDAmeritrade[x[9]:x[10]])
AMTDTE[11] = sum(AMTDT$TDAmeritrade[x[10]:x[11]])
AMTDTE[12] = sum(AMTDT$TDAmeritrade[x[11]:x[12]])
AMTDTE[13] = sum(AMTDT$TDAmeritrade[x[12]:x[13]])
AMTDTE[14] = sum(AMTDT$TDAmeritrade[x[13]:x[14]])
AMTDTE[15] = sum(AMTDT$TDAmeritrade[x[14]:x[15]])
AMTDTE[16] = sum(AMTDT$TDAmeritrade[x[15]:x[16]])
AMTDTE[17] = sum(AMTDT$TDAmeritrade[x[16]:x[17]])
AMTDTE[18] = sum(AMTDT$TDAmeritrade[x[17]:x[18]])
AMTDTE[19] = sum(AMTDT$TDAmeritrade[x[18]:x[19]])
AMTDTE[20] = sum(AMTDT$TDAmeritrade[x[19]:x[20]])
AMTDTE[21] = sum(AMTDT$TDAmeritrade[x[20]:x[21]])

##Breaking down by quarter Q1
y = c(1, 5, 9, 13, 17, 21)
AMTDQ1D = AMTDD[y]
AMTDTQ1 = AMTDTE[y]
AMTDTQ1
AMTDQ1D2 = AMTDQ1D[2:5]

AMTDTQ10.log = diff(as.vector(log(AMTDTQ1)))
length(AMTDTQ10.log)
AMTDTQ1.log = AMTDTQ10.log[1:4]
AMTDQ1D2[1] ##price change on 1st event, 
AMTDTQ1.log[1] ##Trends change for second event - from 1st event 
fitQ1L <- lm(AMTDQ1D2 ~ AMTDTQ1.log)
summary(fitQ1L)
 ratesALQ1 <- data.frame(AMTDQ1D2, AMTDTQ1.log)
corrplot.mixed(cor(ratesALQ1), upper = "ellipse")

fitQ1L

Q1FuncL <- function(x){
  y = -0.04138*x - 0.02034
  y
}

AMTDTE[21]
AMTDTE[17]
AMTDTEV = c(AMTDTE[21], AMTDTE[17])
AMTDTENOW = diff(as.vector(log(AMTDTEV)))
AMTDTENOW
val = AMTDTENOW
val
Q1FuncL(val)
 AMTDQ1D2
AMTDTQ10.log
##Breaking down by quarter Q2
y = c(4, 8, 12, 16, 20)
AMTDQ1D = AMTDD[y]
AMTDTQ2 = AMTDTE[y]

AMTDQ2D2 = AMTDQ1D[2:5]
AMTDTQ2.log = diff(as.vector(log(AMTDTQ1)))

fitQ2L <- lm(AMTDQ2D2 ~ AMTDTQ2.log)
summary(fitQ2L)
ratesALQ2 <- data.frame(AMTDQ2D2, AMTDTQ2.log)
corrplot.mixed(cor(ratesALQ2), upper = "ellipse")

fitQ2L

Q2FuncL <- function(x){
  y = -0.003427*x + 0.049751
  y
}

AMTDTE[20]
AMTDTE[16]
AMTDTEV = c(AMTDTE[20], AMTDTE[16])

AMTDTENOW = diff(as.vector(log(AMTDTEV)))
AMTDTENOW
Q2FuncL(AMTDTENOW)
AMTDQ2D2[4]

##2014 - 2019 Q1 Analysis Data Monthly
##0 = earnings 1/21/2014
## sum = 1, 2, 3, 4 months
##1 = earnings 4/23/2014
## sum = 4,5,6,7 months

getSymbols(c("AMTD"), from="2014-01-19", to="2019-01-18", src="yahoo", periodicity = 'weekly')
data2 = read.csv('AMTD Trends 2014-2019.csv', header = T)
AMTDT2 = data2[,c("Week", "TDAmeritrade")]
length(AMTDT2$Week)
AMTDT2$Week[41]
x = c(1, 14, 27, 40, 53, 66, 79, 92, 105, 118, 131, 144, 157, 170, 183, 196, 209, 222, 235, 248, 260)
## 13
length(x)
xE = c(1, 14, 27, 41, 53, 66, 79, 93, 105, 118, 131, 145, 157, 170, 183, 197, 210, 223, 236, 249, 261)
trendSum = c()
trendSum[1] = sum(AMTDT2$TDAmeritrade[x[1]:x[2]])
trendSum[2] = sum(AMTDT2$TDAmeritrade[x[2]:x[3]])
trendSum[3] = sum(AMTDT2$TDAmeritrade[x[3]:x[4]])
trendSum[4] = sum(AMTDT2$TDAmeritrade[x[4]:x[5]])
trendSum[5] = sum(AMTDT2$TDAmeritrade[x[5]:x[6]])
trendSum[6] = sum(AMTDT2$TDAmeritrade[x[6]:x[7]])
trendSum[7] = sum(AMTDT2$TDAmeritrade[x[7]:x[8]])
trendSum[8] = sum(AMTDT2$TDAmeritrade[x[8]:x[9]])
trendSum[9] = sum(AMTDT2$TDAmeritrade[x[9]:x[10]])
trendSum[10] = sum(AMTDT2$TDAmeritrade[x[10]:x[11]])
trendSum[11] = sum(AMTDT2$TDAmeritrade[x[11]:x[12]])
trendSum[12] = sum(AMTDT2$TDAmeritrade[x[12]:x[13]])
trendSum[13] = sum(AMTDT2$TDAmeritrade[x[13]:x[14]])
trendSum[14] = sum(AMTDT2$TDAmeritrade[x[14]:x[15]])
trendSum[15] = sum(AMTDT2$TDAmeritrade[x[15]:x[16]])
trendSum[16] = sum(AMTDT2$TDAmeritrade[x[16]:x[17]])
trendSum[17] = sum(AMTDT2$TDAmeritrade[x[17]:x[18]])
trendSum[18] = sum(AMTDT2$TDAmeritrade[x[18]:x[19]])
trendSum[19] = sum(AMTDT2$TDAmeritrade[x[19]:x[20]])
trendSum[20] = sum(AMTDT2$TDAmeritrade[x[20]:x[21]])

AMTDC2 = AMTD$AMTD.Close[xE] ##Quarterly Dates
AMTDO2 = AMTD$AMTD.Open[xE]
AMTDD2 = (AMTDC2 - AMTDO2)/(AMTDO2)

##Breaking down by quarter Q1
y = c(4,8,12,16,20)
yE = c(5,9,13,17,21)
AMTDQ1D2 = AMTDD2[yE]
AMTDTrendQ1 = trendSum[y]
AMTDTrendQ1
AMTDQ1D2 = AMTDQ1D2[2:4]
AMTDQ1D2

AMTDTQ10.log = diff(as.vector(log(AMTDTrendQ1)))
AMTDTQ10.log
AMTDTQ10.log = AMTDTQ10.log[1:3]
fitQ1LY <- lm(AMTDQ1D2$AMTD.Close ~ AMTDTQ10.log)
summary(fitQ1LY)
ratesALQ2 <- data.frame(AMTDQ1D2, AMTDTQ10.log)
corrplot.mixed(cor(ratesALQ2), upper = "ellipse")

##lfit=loess(AMTDQ1D2$AMTD.Close~AMTDTQ10.log + AMTDTQ10.log, control = loess.control(surface = "direct"))
##Yh = predict(lfit, 0.08712293)

fitQ1LY
Q1FuncL <- function(x){
  y = 0.02612*x -0.01037
  y
}

Q1FuncL(0.08712293)
Q1FuncL(0.297)

## Breaking Down by Q2
y = c(1,5,9,13,17)
yE = c(2,6,10,14,18)
AMTDQ2D2 = AMTDD2[yE]
AMTDTrendQ2 = trendSum[y]
AMTDTrendQ2
AMTDQ2D2 = AMTDQ2D[2:4]
AMTDQ2D2

AMTDTQ10.log = diff(as.vector(log(AMTDTrendQ1)))
AMTDTQ10.log
AMTDTQ10.log = AMTDTQ10.log[1:3]
fitQ2L <- lm(AMTDQ1D2$AMTD.Close ~ AMTDTQ10.log)
summary(fitQ2L)
ratesALQ2 <- data.frame(AMTDQ1D2, AMTDTQ10.log)
corrplot.mixed(cor(ratesALQ2), upper = "ellipse")


fitQ1LY
Q1FuncL <- function(x){
  y = 0.0678*x -0.01183
  y
}

Q1FuncL(0.08712293)

Q1FuncPoly <- function(x){
  y = -0.1442*x^2 + 0.0382*x - 0.0037
  y
}

Q1FuncPoly(0.08712293)


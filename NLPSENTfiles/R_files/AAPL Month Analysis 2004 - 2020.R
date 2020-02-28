library('quantmod')
library('corrplot')
getSymbols(c("AAPL"), from="2009-10-15", to="2020-01-12", src="yahoo", periodicity = 'weekly')

data = read.csv('Apple 2004.csv', header = T)
AAPL.trends = data[,c("Month", "Apple")]

##Extract Earnings Dates ()
##Requires Adjustment 
AAPL[1]
length(AAPL$AAPL.Close)
x = seq(1,535, by = 14)
length(x)
AAPL.Earnings = AAPL[x]
AAPL.Earnings
AAPL.Earnings[42]
AAPL.Earnings[43]
AAPL[3*13+1+1-13]

AAPL.Earnings[3,] = AAPL[3*13+1+1-13]
AAPL.Earnings[7,] = AAPL[7*13+1+1-13]
AAPL.Earnings[11,] = AAPL[11*13+1+1-13]
AAPL.Earnings[17,] = AAPL[17*13+1+1-13]
AAPL.Earnings[18,] = AAPL[18*13+1+1-13]
AAPL.Earnings[20,] = AAPL[20*13+1+1-13]
AAPL.Earnings[21,] = AAPL[21*13+1+1-13]
AAPL.Earnings[22,] = AAPL[22*13+1+1-13]
AAPL.Earnings[24,] = AAPL[24*13+1+1-13]
AAPL.Earnings[25,] = AAPL[25*13+1+1-13]
AAPL.Earnings[26,] = AAPL[26*13+1+1-13]
AAPL.Earnings[28,] = AAPL[28*13+1+1-13]
AAPL.Earnings[29,] = AAPL[29*13+1+1-13]
AAPL.Earnings[30,] = AAPL[30*13+1+1-13]
AAPL.Earnings[32,] = AAPL[32*13+1+1-13]
AAPL.Earnings[33,] = AAPL[33*13+1+1-13]
AAPL.Earnings[34,] = AAPL[34*13+1+1-13]

AAPL.Earnings[19,] = AAPL[19*13+1+1+1-13]
AAPL.Earnings[23,] = AAPL[23*13+1+1+1-13]
AAPL.Earnings[27,] = AAPL[27*13+1+1+1-13]
AAPL.Earnings[31,] = AAPL[31*13+1+1+1-13]
AAPL.Earnings[35,] = AAPL[35*13+1+1+1-13]
AAPL.Earnings[36,] = AAPL[36*13+1+1+1-13]
AAPL.Earnings[37,] = AAPL[37*13+1+1+1-13]
AAPL.Earnings[38,] = AAPL[38*13+1+1+1-13]
AAPL.Earnings[39,] = AAPL[39*13+1+1+1-13]
AAPL.Earnings[40,] = AAPL[40*13+1+1+1-13]
AAPL.Earnings[41,] = AAPL[41*13+1+1+1-13]
AAPL.Earnings[42,] = AAPL[42*13+1+1+1-13]
AAPL.Earnings[43,] = AAPL[43*13+1+1+1-13]

##1,2,4,5,6,8,9,10,12,13,14,15,16,
##3,7,11, 17,18,20,21,22,24,25,26,28,29,30,32,33,34
##19,23,27,31,35,36,37,38,39,40,41,42,43
length(AAPL.Earnings)
x = seq(1, 43, by = 1)
AAPL.EarningsClose = AAPL.Earnings$AAPL.Close[x] ##Quarterly Dates
AAPL.EarningsOpen = AAPL.Earnings$AAPL.Open[x]
AAPL.EarningsChange = (AAPL.EarningsClose - AAPL.EarningsOpen)/(AAPL.EarningsOpen)

##TrendQuarterlySum
##7/21/2009 = 5,6,7
##10/27/2009 = 8,7,9
y = seq(61,193, by = 3)
length(y)
AAPL.trends$Month[190]
AAPL.trendSum = c()
y[1]
y[2]
AAPL.trendSum[1] = sum(AAPL.trends$TDAmeritrade[62:64]) ## 2-4
AAPL.trendSum[2] = sum(AAPL.trends$TDAmeritrade[65:67]) ## 5 - 7
AAPL.trendSum[3] = sum(AAPL.trends$TDAmeritrade[68:70]) ## 8 - 10
AAPL.trendSum[4] = sum(AAPL.trends$TDAmeritrade[71:73]) ## 11 - 1
AAPL.trendSum[5] = sum(AAPL.trends$TDAmeritrade[74:76]) ## 2 - 4
AAPL.trendSum[6] = sum(AAPL.trends$TDAmeritrade[77:79])
AAPL.trendSum[7] = sum(AAPL.trends$TDAmeritrade[80:82])
AAPL.trendSum[8] = sum(AAPL.trends$TDAmeritrade[83:85])
AAPL.trendSum[9] = sum(AAPL.trends$TDAmeritrade[86:88])
AAPL.trendSum[10] = sum(AAPL.trends$TDAmeritrade[89:91])
AAPL.trendSum[11] = sum(AAPL.trends$TDAmeritrade[92:94])
AAPL.trendSum[12] = sum(AAPL.trends$TDAmeritrade[95:97])
AAPL.trendSum[13] = sum(AAPL.trends$TDAmeritrade[98:100])
AAPL.trendSum[14] = sum(AAPL.trends$TDAmeritrade[101:103])
AAPL.trendSum[15] = sum(AAPL.trends$TDAmeritrade[104:106])
AAPL.trendSum[16] = sum(AAPL.trends$TDAmeritrade[107:109])
AAPL.trendSum[17] = sum(AAPL.trends$TDAmeritrade[110:112])
AAPL.trendSum[18] = sum(AAPL.trends$TDAmeritrade[113:115])
AAPL.trendSum[19] = sum(AAPL.trends$TDAmeritrade[116:118])
AAPL.trendSum[20] = sum(AAPL.trends$TDAmeritrade[119:121])
AAPL.trendSum[21] = sum(AAPL.trends$TDAmeritrade[122:124])
AAPL.trendSum[22] = sum(AAPL.trends$TDAmeritrade[125:127])
AAPL.trendSum[23] = sum(AAPL.trends$TDAmeritrade[128:130])
AAPL.trendSum[24] = sum(AAPL.trends$TDAmeritrade[131:133])
AAPL.trendSum[25] = sum(AAPL.trends$TDAmeritrade[134:136])
AAPL.trendSum[26] = sum(AAPL.trends$TDAmeritrade[137:139])
AAPL.trendSum[27] = sum(AAPL.trends$TDAmeritrade[140:142])
AAPL.trendSum[28] = sum(AAPL.trends$TDAmeritrade[143:145])
AAPL.trendSum[29] = sum(AAPL.trends$TDAmeritrade[146:148])
AAPL.trendSum[30] = sum(AAPL.trends$TDAmeritrade[149:151])
AAPL.trendSum[31] = sum(AAPL.trends$TDAmeritrade[152:154])
AAPL.trendSum[32] = sum(AAPL.trends$TDAmeritrade[155:157])
AAPL.trendSum[33] = sum(AAPL.trends$TDAmeritrade[158:160])
AAPL.trendSum[34] = sum(AAPL.trends$TDAmeritrade[161:163])
AAPL.trendSum[35] = sum(AAPL.trends$TDAmeritrade[164:166])
AAPL.trendSum[36] = sum(AAPL.trends$TDAmeritrade[167:169])
AAPL.trendSum[37] = sum(AAPL.trends$TDAmeritrade[170:172])
AAPL.trendSum[38] = sum(AAPL.trends$TDAmeritrade[173:175])
AAPL.trendSum[39] = sum(AAPL.trends$TDAmeritrade[176:178])
AAPL.trendSum[40] = sum(AAPL.trends$TDAmeritrade[179:181])
AAPL.trendSum[41] = sum(AAPL.trends$TDAmeritrade[182:184])
AAPL.trendSum[42] = sum(AAPL.trends$TDAmeritrade[185:187])
AAPL.trendSum[43] = sum(AAPL.trends$TDAmeritrade[188:190])
AAPL.trendSum[44] = sum(AAPL.trends$TDAmeritrade[191:193])

##Quarter 1, April (4)
numQ1 = seq(1, 44, by = 4)
AAPL.trendSumQ1 = AAPL.trendSum[numQ1]

AAPL.trendSumQ1.log = diff(as.vector(log(AAPL.trendSumQ1)))
length(AAPL.trendSumQ1.log)
num2Q1 = seq(5, 43, by = 4)

AAPL.EarningsChange.Q1 = AAPL.EarningsChange[num2Q1]
AAPL.trendSumQ1.C = diff(as.vector(AAPL.trendSumQ1))

fit <- lm(AAPL.EarningsChange.Q1 ~ AAPL.trendSumQ1.log)
fitC <- lm(AAPL.EarningsChange.Q1 ~ AAPL.trendSumQ1.C)

summary(fit)
ratesAQ1 <- data.frame(AAPL.EarningsChange.Q1, AAPL.trendSumQ1.log)
corrplot.mixed(cor(ratesAQ1), upper = "ellipse")

summary(fitC)
ratesAQ1C <- data.frame(AAPL.EarningsChange.Q1, AAPL.trendSumQ1.C)
corrplot.mixed(cor(ratesAQ1C), upper = "ellipse")

##Quarter 2, July (7)
numQ2 = seq(2, 44, by = 4)
AAPL.trendSumQ2 = AAPL.trendSum[numQ2]

AAPL.trendSumQ2.log = diff(as.vector(log(AAPL.trendSumQ2)))
AAPL.trendSumQ2.C = diff(as.vector(AAPL.trendSumQ2))

length(AAPL.trendSumQ2.log)
num2Q2 = seq(6, 43, by = 4)

AAPL.EarningsChange.Q2 = AAPL.EarningsChange[num2Q2]

fit2 <- lm(AAPL.EarningsChange.Q2 ~ AAPL.trendSumQ2.log)
fit2
summary(fit2)
ratesAQ2 <- data.frame(AAPL.EarningsChange.Q2, AAPL.trendSumQ2.log)
corrplot.mixed(cor(ratesAQ2), upper = "ellipse")

fitC2 <- lm(AAPL.EarningsChange.Q1 ~ AAPL.trendSumQ1.C)
summary(fitC2)
ratesAQ2C <- data.frame(AAPL.EarningsChange.Q2, AAPL.trendSumQ2.C)
corrplot.mixed(cor(ratesAQ2C), upper = "ellipse")

##Quarter 3 October (10)
numQ3 = seq(3, 44, by = 4)
AAPL.trendSumQ3 = AAPL.trendSum[numQ3]

AAPL.trendSumQ3.log = diff(as.vector(log(AAPL.trendSumQ3)))
AAPL.trendSumQ3.C = diff(as.vector(AAPL.trendSumQ3))

length(AAPL.trendSumQ3.log)
num2Q3 = seq(7, 43, by = 4)

AAPL.EarningsChange.Q3 = AAPL.EarningsChange[num2Q3]

fit3 <- lm(AAPL.EarningsChange.Q3 ~ AAPL.trendSumQ3.log)
fit3
summary(fit3)
ratesAQ3 <- data.frame(AAPL.EarningsChange.Q3, AAPL.trendSumQ3.log)
corrplot.mixed(cor(ratesAQ3), upper = "ellipse")

fitC3 <- lm(AAPL.EarningsChange.Q3 ~ AAPL.trendSumQ3.C)
summary(fitC3)
ratesAQ3C <- data.frame(AAPL.EarningsChange.Q3, AAPL.trendSumQ3.C)
corrplot.mixed(cor(ratesAQ3C), upper = "ellipse")

##Quarter 4, January (1)
numQ4 = seq(4, 43, by = 4)
AAPL.trendSumQ4 = AAPL.trendSum[numQ4]

AAPL.trendSumQ4.log = diff(as.vector(log(AAPL.trendSumQ4)))
AAPL.trendSumQ4.C = diff(as.vector(AAPL.trendSumQ4))

num2Q4 = seq(8, 43, by = 4)

AAPL.EarningsChange.Q4 = AAPL.EarningsChange[num2Q4]

fit4 <- lm(AAPL.EarningsChange.Q4 ~ AAPL.trendSumQ4.log)
fit4
summary(fit4)
ratesAQ4 <- data.frame(AAPL.EarningsChange.Q4, AAPL.trendSumQ4.log)
corrplot.mixed(cor(ratesAQ4), upper = "ellipse")

fitC4 <- lm(AAPL.EarningsChange.Q4 ~ AAPL.trendSumQ4.C)
summary(fitC4)
ratesAQ4C <- data.frame(AAPL.EarningsChange.Q4, AAPL.trendSumQ4.C)
corrplot.mixed(cor(ratesAQ4C), upper = "ellipse")

plot(AAPL.EarningsChange.Q4$AAPL.Close)


## Not Dividing By Quarter

AAPL.trendSumT = AAPL.trendSum[1:43] ##1 = 3 months before 4th 2009, 43 = data collected past most recent earning
AAPL.trendSumT.log = diff(as.vector(log(AAPL.trendSumT)))
AAPL.trendSumT.C = diff(as.vector(AAPL.trendSumT))

length(AAPL.trendSumT.log)
length(AAPL.EarningsChange)

AAPL.EarningsChangeT = AAPL.EarningsChange[2:43] ##43 = previous earnings date

fitT <- lm(AAPL.EarningsChangeT ~ AAPL.trendSumT.log)
fitT
summary(fitT)
ratesAT.log <- data.frame(AAPL.EarningsChangeT, AAPL.trendSumT.log)
corrplot.mixed(cor(ratesAT.log), upper = "ellipse")

fitCT<- lm(AAPL.EarningsChangeT ~ AAPL.trendSumT.C)
summary(fitCT)
ratesAT <- data.frame(AAPL.EarningsChangeT, AAPL.trendSumT.C)
corrplot.mixed(cor(ratesAT), upper = "ellipse")

##Testing
AAPL.trendSumQ4.C
AAPL.EarningsChange.Q4

AAPL.trendSumT.C
AAPL.EarningsChangeT

##Polynomial Q4
AAPL.trendSum[44] - AAPL.trendSum[40]
PolyFunc = function(x){
  y = -2E+11*x^6 + 5E+08*x^5 + 6E+08*x^4 + 503853*x^3 - 363782*x^2 - 105.03*x + 31.203
  y
}
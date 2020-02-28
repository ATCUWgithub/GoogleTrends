library('quantmod')
library('corrplot')
getSymbols(c("AMTD"), from="2009-04-20", to="2020-01-17", src="yahoo", periodicity = 'weekly')

data = read.csv('AMTD 2004 .csv', header = T)
AMTD.trends = data[,c("Month", "TDAmeritrade")]

##Extract Earnings Dates
##First Earnings = 4/21/2009
AMTD[547]
length(AMTD$AMTD.Close)
x = seq(1,561, by = 13)
length(x)
AMTD.Earnings = AMTD[x]
AMTD.Earnings[44]
AMTD.Earnings[43]
AMTD[3*13+1+1-13]

AMTD.Earnings[3,] = AMTD[3*13+1+1-13]
AMTD.Earnings[7,] = AMTD[7*13+1+1-13]
AMTD.Earnings[11,] = AMTD[11*13+1+1-13]
AMTD.Earnings[17,] = AMTD[17*13+1+1-13]
AMTD.Earnings[18,] = AMTD[18*13+1+1-13]
AMTD.Earnings[20,] = AMTD[20*13+1+1-13]
AMTD.Earnings[21,] = AMTD[21*13+1+1-13]
AMTD.Earnings[22,] = AMTD[22*13+1+1-13]
AMTD.Earnings[24,] = AMTD[24*13+1+1-13]
AMTD.Earnings[25,] = AMTD[25*13+1+1-13]
AMTD.Earnings[26,] = AMTD[26*13+1+1-13]
AMTD.Earnings[28,] = AMTD[28*13+1+1-13]
AMTD.Earnings[29,] = AMTD[29*13+1+1-13]
AMTD.Earnings[30,] = AMTD[30*13+1+1-13]
AMTD.Earnings[32,] = AMTD[32*13+1+1-13]
AMTD.Earnings[33,] = AMTD[33*13+1+1-13]
AMTD.Earnings[34,] = AMTD[34*13+1+1-13]

AMTD.Earnings[19,] = AMTD[19*13+1+1+1-13]
AMTD.Earnings[23,] = AMTD[23*13+1+1+1-13]
AMTD.Earnings[27,] = AMTD[27*13+1+1+1-13]
AMTD.Earnings[31,] = AMTD[31*13+1+1+1-13]
AMTD.Earnings[35,] = AMTD[35*13+1+1+1-13]
AMTD.Earnings[36,] = AMTD[36*13+1+1+1-13]
AMTD.Earnings[37,] = AMTD[37*13+1+1+1-13]
AMTD.Earnings[38,] = AMTD[38*13+1+1+1-13]
AMTD.Earnings[39,] = AMTD[39*13+1+1+1-13]
AMTD.Earnings[40,] = AMTD[40*13+1+1+1-13]
AMTD.Earnings[41,] = AMTD[41*13+1+1+1-13]
AMTD.Earnings[42,] = AMTD[42*13+1+1+1-13]
AMTD.Earnings[43,] = AMTD[43*13+1+1+1-13]

##1,2,4,5,6,8,9,10,12,13,14,15,16,
##3,7,11, 17,18,20,21,22,24,25,26,28,29,30,32,33,34
##19,23,27,31,35,36,37,38,39,40,41,42,43
length(AMTD.Earnings)
x = seq(1, 43, by = 1)
AMTD.EarningsClose = AMTD.Earnings$AMTD.Close[x] ##Quarterly Dates
AMTD.EarningsOpen = AMTD.Earnings$AMTD.Open[x]
AMTD.EarningsChange = (AMTD.EarningsClose - AMTD.EarningsOpen)/(AMTD.EarningsOpen)

##TrendQuarterlySum
##7/21/2009 = 5,6,7
##10/27/2009 = 8,7,9
y = seq(61,193, by = 3)
length(y)
AMTD.trends$Month[190]
AMTD.trendSum = c()
y[1]
y[2]
AMTD.trendSum[1] = sum(AMTD.trends$TDAmeritrade[62:64]) ## 2-4
AMTD.trendSum[2] = sum(AMTD.trends$TDAmeritrade[65:67]) ## 5 - 7
AMTD.trendSum[3] = sum(AMTD.trends$TDAmeritrade[68:70]) ## 8 - 10
AMTD.trendSum[4] = sum(AMTD.trends$TDAmeritrade[71:73]) ## 11 - 1
AMTD.trendSum[5] = sum(AMTD.trends$TDAmeritrade[74:76]) ## 2 - 4
AMTD.trendSum[6] = sum(AMTD.trends$TDAmeritrade[77:79])
AMTD.trendSum[7] = sum(AMTD.trends$TDAmeritrade[80:82])
AMTD.trendSum[8] = sum(AMTD.trends$TDAmeritrade[83:85])
AMTD.trendSum[9] = sum(AMTD.trends$TDAmeritrade[86:88])
AMTD.trendSum[10] = sum(AMTD.trends$TDAmeritrade[89:91])
AMTD.trendSum[11] = sum(AMTD.trends$TDAmeritrade[92:94])
AMTD.trendSum[12] = sum(AMTD.trends$TDAmeritrade[95:97])
AMTD.trendSum[13] = sum(AMTD.trends$TDAmeritrade[98:100])
AMTD.trendSum[14] = sum(AMTD.trends$TDAmeritrade[101:103])
AMTD.trendSum[15] = sum(AMTD.trends$TDAmeritrade[104:106])
AMTD.trendSum[16] = sum(AMTD.trends$TDAmeritrade[107:109])
AMTD.trendSum[17] = sum(AMTD.trends$TDAmeritrade[110:112])
AMTD.trendSum[18] = sum(AMTD.trends$TDAmeritrade[113:115])
AMTD.trendSum[19] = sum(AMTD.trends$TDAmeritrade[116:118])
AMTD.trendSum[20] = sum(AMTD.trends$TDAmeritrade[119:121])
AMTD.trendSum[21] = sum(AMTD.trends$TDAmeritrade[122:124])
AMTD.trendSum[22] = sum(AMTD.trends$TDAmeritrade[125:127])
AMTD.trendSum[23] = sum(AMTD.trends$TDAmeritrade[128:130])
AMTD.trendSum[24] = sum(AMTD.trends$TDAmeritrade[131:133])
AMTD.trendSum[25] = sum(AMTD.trends$TDAmeritrade[134:136])
AMTD.trendSum[26] = sum(AMTD.trends$TDAmeritrade[137:139])
AMTD.trendSum[27] = sum(AMTD.trends$TDAmeritrade[140:142])
AMTD.trendSum[28] = sum(AMTD.trends$TDAmeritrade[143:145])
AMTD.trendSum[29] = sum(AMTD.trends$TDAmeritrade[146:148])
AMTD.trendSum[30] = sum(AMTD.trends$TDAmeritrade[149:151])
AMTD.trendSum[31] = sum(AMTD.trends$TDAmeritrade[152:154])
AMTD.trendSum[32] = sum(AMTD.trends$TDAmeritrade[155:157])
AMTD.trendSum[33] = sum(AMTD.trends$TDAmeritrade[158:160])
AMTD.trendSum[34] = sum(AMTD.trends$TDAmeritrade[161:163])
AMTD.trendSum[35] = sum(AMTD.trends$TDAmeritrade[164:166])
AMTD.trendSum[36] = sum(AMTD.trends$TDAmeritrade[167:169])
AMTD.trendSum[37] = sum(AMTD.trends$TDAmeritrade[170:172])
AMTD.trendSum[38] = sum(AMTD.trends$TDAmeritrade[173:175])
AMTD.trendSum[39] = sum(AMTD.trends$TDAmeritrade[176:178])
AMTD.trendSum[40] = sum(AMTD.trends$TDAmeritrade[179:181])
AMTD.trendSum[41] = sum(AMTD.trends$TDAmeritrade[182:184])
AMTD.trendSum[42] = sum(AMTD.trends$TDAmeritrade[185:187])
AMTD.trendSum[43] = sum(AMTD.trends$TDAmeritrade[188:190])
AMTD.trendSum[44] = sum(AMTD.trends$TDAmeritrade[191:193])

##Quarter 1, April (4)
numQ1 = seq(1, 44, by = 4)
AMTD.trendSumQ1 = AMTD.trendSum[numQ1]

AMTD.trendSumQ1.log = diff(as.vector(log(AMTD.trendSumQ1)))
length(AMTD.trendSumQ1.log)
num2Q1 = seq(5, 43, by = 4)

AMTD.EarningsChange.Q1 = AMTD.EarningsChange[num2Q1]
AMTD.trendSumQ1.C = diff(as.vector(AMTD.trendSumQ1))

fit <- lm(AMTD.EarningsChange.Q1 ~ AMTD.trendSumQ1.log)
fitC <- lm(AMTD.EarningsChange.Q1 ~ AMTD.trendSumQ1.C)

summary(fit)
ratesAQ1 <- data.frame(AMTD.EarningsChange.Q1, AMTD.trendSumQ1.log)
corrplot.mixed(cor(ratesAQ1), upper = "ellipse")

summary(fitC)
ratesAQ1C <- data.frame(AMTD.EarningsChange.Q1, AMTD.trendSumQ1.C)
corrplot.mixed(cor(ratesAQ1C), upper = "ellipse")

##Quarter 2, July (7)
numQ2 = seq(2, 44, by = 4)
AMTD.trendSumQ2 = AMTD.trendSum[numQ2]

AMTD.trendSumQ2.log = diff(as.vector(log(AMTD.trendSumQ2)))
AMTD.trendSumQ2.C = diff(as.vector(AMTD.trendSumQ2))

length(AMTD.trendSumQ2.log)
num2Q2 = seq(6, 43, by = 4)

AMTD.EarningsChange.Q2 = AMTD.EarningsChange[num2Q2]

fit2 <- lm(AMTD.EarningsChange.Q2 ~ AMTD.trendSumQ2.log)
fit2
summary(fit2)
ratesAQ2 <- data.frame(AMTD.EarningsChange.Q2, AMTD.trendSumQ2.log)
corrplot.mixed(cor(ratesAQ2), upper = "ellipse")

fitC2 <- lm(AMTD.EarningsChange.Q1 ~ AMTD.trendSumQ1.C)
summary(fitC2)
ratesAQ2C <- data.frame(AMTD.EarningsChange.Q2, AMTD.trendSumQ2.C)
corrplot.mixed(cor(ratesAQ2C), upper = "ellipse")

##Quarter 3 October (10)
numQ3 = seq(3, 44, by = 4)
AMTD.trendSumQ3 = AMTD.trendSum[numQ3]

AMTD.trendSumQ3.log = diff(as.vector(log(AMTD.trendSumQ3)))
AMTD.trendSumQ3.C = diff(as.vector(AMTD.trendSumQ3))

length(AMTD.trendSumQ3.log)
num2Q3 = seq(7, 43, by = 4)

AMTD.EarningsChange.Q3 = AMTD.EarningsChange[num2Q3]

fit3 <- lm(AMTD.EarningsChange.Q3 ~ AMTD.trendSumQ3.log)
fit3
summary(fit3)
ratesAQ3 <- data.frame(AMTD.EarningsChange.Q3, AMTD.trendSumQ3.log)
corrplot.mixed(cor(ratesAQ3), upper = "ellipse")

fitC3 <- lm(AMTD.EarningsChange.Q3 ~ AMTD.trendSumQ3.C)
summary(fitC3)
ratesAQ3C <- data.frame(AMTD.EarningsChange.Q3, AMTD.trendSumQ3.C)
corrplot.mixed(cor(ratesAQ3C), upper = "ellipse")

##Quarter 4, January (1)
numQ4 = seq(4, 43, by = 4)
AMTD.trendSumQ4 = AMTD.trendSum[numQ4]

AMTD.trendSumQ4.log = diff(as.vector(log(AMTD.trendSumQ4)))
AMTD.trendSumQ4.C = diff(as.vector(AMTD.trendSumQ4))

num2Q4 = seq(8, 43, by = 4)

AMTD.EarningsChange.Q4 = AMTD.EarningsChange[num2Q4]

fit4 <- lm(AMTD.EarningsChange.Q4 ~ AMTD.trendSumQ4.log)
fit4
summary(fit4)
ratesAQ4 <- data.frame(AMTD.EarningsChange.Q4, AMTD.trendSumQ4.log)
corrplot.mixed(cor(ratesAQ4), upper = "ellipse")

fitC4 <- lm(AMTD.EarningsChange.Q4 ~ AMTD.trendSumQ4.C)
summary(fitC4)
ratesAQ4C <- data.frame(AMTD.EarningsChange.Q4, AMTD.trendSumQ4.C)
corrplot.mixed(cor(ratesAQ4C), upper = "ellipse")

plot(AMTD.EarningsChange.Q4$AMTD.Close)


## Not Dividing By Quarter

AMTD.trendSumT = AMTD.trendSum[1:43] ##1 = 3 months before 4th 2009, 43 = data collected past most recent earning
AMTD.trendSumT.log = diff(as.vector(log(AMTD.trendSumT)))
AMTD.trendSumT.C = diff(as.vector(AMTD.trendSumT))

length(AMTD.trendSumT.log)
length(AMTD.EarningsChange)

AMTD.EarningsChangeT = AMTD.EarningsChange[2:43] ##43 = previous earnings date

fitT <- lm(AMTD.EarningsChangeT ~ AMTD.trendSumT.log)
fitT
summary(fitT)
ratesAT.log <- data.frame(AMTD.EarningsChangeT, AMTD.trendSumT.log)
corrplot.mixed(cor(ratesAT.log), upper = "ellipse")

fitCT<- lm(AMTD.EarningsChangeT ~ AMTD.trendSumT.C)
summary(fitCT)
ratesAT <- data.frame(AMTD.EarningsChangeT, AMTD.trendSumT.C)
corrplot.mixed(cor(ratesAT), upper = "ellipse")

##Testing
AMTD.trendSumQ4.C
AMTD.EarningsChange.Q4

AMTD.trendSumT.C
AMTD.EarningsChangeT

##Polynomial Q4
AMTD.trendSum[44] - AMTD.trendSum[40]
PolyFunc = function(x){
  y = -2E+11*x^6 + 5E+08*x^5 + 6E+08*x^4 + 503853*x^3 - 363782*x^2 - 105.03*x + 31.203
  y
}

PolyFunc(45)

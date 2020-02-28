library('quantmod')
library('corrplot')
getSymbols(c("SBUX"), from="2015-04-23", to="2020-01-23", src="yahoo", periodicity = 'weekly')

data = read.csv('Starbucks_Trends_5Y.csv', header = T)
Starbucks.trends = data[,c("Week", "Starbucks")]

#First Earning = 04/23/2015
#First TrendDate = 01/25/2015 (perfect 3 months before earnings)
SBUX[40]
num = c(1, 14, 28, 40, 53, 66, 81, 93, 106, 119, 133, 145, 158, 171, 185, 197, 210, 223, 237)
Starbucks.Earnings = SBUX[num]
Starbucks.Earnings.Close = Starbucks.Earnings$SBUX.Close
Starbucks.Earnings.Open = Starbucks.Earnings$SBUX.Open
Starbucks.Earnings.Change = (Starbucks.Earnings.Close - Starbucks.Earnings.Open)/(Starbucks.Earnings.Open)
num[18]
#Trends Sum Quarterly
Starbucks.trends[261,]
Starbucks.trends.sum = c()
Starbucks.trends.sum[1] = sum(Starbucks.trends$Starbucks[1:12]) #up to 4/19/2019
Starbucks.trends.sum[2] = sum(Starbucks.trends$Starbucks[13:25])
Starbucks.trends.sum[3] = sum(Starbucks.trends$Starbucks[26:39])
Starbucks.trends.sum[4] = sum(Starbucks.trends$Starbucks[40:52])
Starbucks.trends.sum[5] = sum(Starbucks.trends$Starbucks[53:64])
Starbucks.trends.sum[6] = sum(Starbucks.trends$Starbucks[65:77])
Starbucks.trends.sum[7] = sum(Starbucks.trends$Starbucks[78:92])
Starbucks.trends.sum[8] = sum(Starbucks.trends$Starbucks[93:104])
Starbucks.trends.sum[9] = sum(Starbucks.trends$Starbucks[105:117])
Starbucks.trends.sum[10] = sum(Starbucks.trends$Starbucks[118:130])
Starbucks.trends.sum[11] = sum(Starbucks.trends$Starbucks[131:144])
Starbucks.trends.sum[12] = sum(Starbucks.trends$Starbucks[145:156])
Starbucks.trends.sum[13] = sum(Starbucks.trends$Starbucks[157:169])
Starbucks.trends.sum[14] = sum(Starbucks.trends$Starbucks[170:182])
Starbucks.trends.sum[15] = sum(Starbucks.trends$Starbucks[183:196])
Starbucks.trends.sum[16] = sum(Starbucks.trends$Starbucks[197:208])
Starbucks.trends.sum[17] = sum(Starbucks.trends$Starbucks[209:221])
Starbucks.trends.sum[18] = sum(Starbucks.trends$Starbucks[222:234])
Starbucks.trends.sum[19] = sum(Starbucks.trends$Starbucks[235:248])

# Starbucks.trends.sum[1] = sum(Starbucks.trends$Starbucks[1:num[2] - 2]) #up to 4/19/2019
# Starbucks.trends.sum[2] = sum(Starbucks.trends$Starbucks[num[2]-1:num[3]-3])
# Starbucks.trends.sum[3] = sum(Starbucks.trends$Starbucks[num[3]-2:num[4]-1])
# Starbucks.trends.sum[4] = sum(Starbucks.trends$Starbucks[num[4]:num[5]-1])
# Starbucks.trends.sum[5] = sum(Starbucks.trends$Starbucks[num[5]:num[6]-2])
# Starbucks.trends.sum[6] = sum(Starbucks.trends$Starbucks[num[6]-1:num[7]-4])
# Starbucks.trends.sum[7] = sum(Starbucks.trends$Starbucks[num[7]-3:num[8]-1])
# Starbucks.trends.sum[8] = sum(Starbucks.trends$Starbucks[num[8]:num[9]-2])
# Starbucks.trends.sum[9] = sum(Starbucks.trends$Starbucks[num[9]-1:num[10]-2])
# Starbucks.trends.sum[10] = sum(Starbucks.trends$Starbucks[num[10]-1:num[11]-3])
# Starbucks.trends.sum[11] = sum(Starbucks.trends$Starbucks[num[11]-2:num[12]-1])
# Starbucks.trends.sum[12] = sum(Starbucks.trends$Starbucks[num[12]:num[13]-2])
# Starbucks.trends.sum[13] = sum(Starbucks.trends$Starbucks[num[13]-1:num[14]-2])
# Starbucks.trends.sum[14] = sum(Starbucks.trends$Starbucks[num[14]-1:num[15]-3])
# Starbucks.trends.sum[15] = sum(Starbucks.trends$Starbucks[num[15]-2:num[16]-1])
# Starbucks.trends.sum[16] = sum(Starbucks.trends$Starbucks[num[16]:num[17]-2])
# Starbucks.trends.sum[17] = sum(Starbucks.trends$Starbucks[num[17]-1:num[18]-2])
# Starbucks.trends.sum[18] = sum(Starbucks.trends$Starbucks[num[18]-1:num[19]-3])
# Starbucks.trends.sum[19] = sum(Starbucks.trends$Starbucks[num[19]-2: 248])

#Breaking down by Q1, 04/20
Q1.Dates = c(1,5,9,13,17) # y for earnings = y for trends
Sbux.Q1.Earnings = Starbucks.Earnings.Change[Q1.Dates]
Sbux.Q1.Trends = Starbucks.trends.sum[Q1.Dates]

Sbux.Q1.Trends.Change = diff(as.vector(Sbux.Q1.Trends))
Sbux.Q1.Trends.Change.Log = diff(as.vector(log(Sbux.Q1.Trends)))

length(Sbux.Q1.Trends.Change)
length(Sbux.Q1.Earnings)

Sbux.Q1.Earnings = Sbux.Q1.Earnings[2:5]

Sbux.Fit.Q1 = lm(Sbux.Q1.Earnings ~ Sbux.Q1.Trends.Change)
summary(Sbux.Fit.Q1)
Sbux.Corr.Q1 <- data.frame(Sbux.Q1.Earnings, Sbux.Q1.Trends.Change)
corrplot.mixed(cor(Sbux.Corr.Q1), upper = "ellipse")

Sbux.Fit.Q1.Log = lm(Sbux.Q1.Earnings ~ Sbux.Q1.Trends.Change.Log)
summary(Sbux.Fit.Q1.Log)
Sbux.Corr.Q1.Log <- data.frame(Sbux.Q1.Earnings, Sbux.Q1.Trends.Change.Log)
corrplot.mixed(cor(Sbux.Corr.Q1.Log), upper = "ellipse")

#Breaking down by Q2, 07/20
Q2.Dates = c(2,6,10,14,18) # y for earnings = y for trends
Sbux.Q2.Earnings = Starbucks.Earnings.Change[Q2.Dates]
Sbux.Q2.Trends = Starbucks.trends.sum[Q2.Dates]

Sbux.Q2.Trends.Change = diff(as.vector(Sbux.Q2.Trends))
Sbux.Q2.Trends.Change.Log = diff(as.vector(log(Sbux.Q2.Trends)))

length(Sbux.Q2.Trends.Change)
length(Sbux.Q2.Earnings)

Sbux.Q2.Earnings = Sbux.Q2.Earnings[2:5]

Sbux.Fit.Q2 = lm(Sbux.Q2.Earnings ~ Sbux.Q2.Trends.Change)
summary(Sbux.Fit.Q2)
Sbux.Corr.Q2 <- data.frame(Sbux.Q2.Earnings, Sbux.Q2.Trends.Change)
corrplot.mixed(cor(Sbux.Corr.Q2), upper = "ellipse")

Sbux.Fit.Q2.Log = lm(Sbux.Q2.Earnings ~ Sbux.Q2.Trends.Change.Log)
summary(Sbux.Fit.Q2.Log)
Sbux.Corr.Q2.Log <- data.frame(Sbux.Q2.Earnings, Sbux.Q2.Trends.Change.Log)
corrplot.mixed(cor(Sbux.Corr.Q2.Log), upper = "ellipse")

#Breaking Down by Q3, 10/26
Q3.Dates = c(3,7,11,15,19) # y for earnings = y for trends
Sbux.Q3.Earnings = Starbucks.Earnings.Change[Q3.Dates]
Sbux.Q3.Trends = Starbucks.trends.sum[Q3.Dates]

Sbux.Q3.Trends.Change = diff(as.vector(Sbux.Q3.Trends))
Sbux.Q3.Trends.Change.Log = diff(as.vector(log(Sbux.Q3.Trends)))

length(Sbux.Q3.Trends.Change)
length(Sbux.Q3.Earnings)

Sbux.Q3.Earnings = Sbux.Q3.Earnings[2:5]

Sbux.Fit.Q3 = lm(Sbux.Q3.Earnings ~ Sbux.Q3.Trends.Change)
summary(Sbux.Fit.Q3)
Sbux.Corr.Q3 <- data.frame(Sbux.Q3.Earnings, Sbux.Q3.Trends.Change)
corrplot.mixed(cor(Sbux.Corr.Q3), upper = "ellipse")

Sbux.Fit.Q3.Log = lm(Sbux.Q3.Earnings ~ Sbux.Q3.Trends.Change.Log)
summary(Sbux.Fit.Q3.Log)
Sbux.Corr.Q3.Log <- data.frame(Sbux.Q3.Earnings, Sbux.Q3.Trends.Change.Log)
corrplot.mixed(cor(Sbux.Corr.Q3.Log), upper = "ellipse")

#Breaking Down by Q4, 01/18
Q4.Dates = c(4,8,12,16) # y for earnings = y for trends
Sbux.Q4.Earnings = Starbucks.Earnings.Change[Q4.Dates]
Sbux.Q4.Trends = Starbucks.trends.sum[Q4.Dates]

Sbux.Q4.Trends.Change = diff(as.vector(Sbux.Q4.Trends))
Sbux.Q4.Trends.Change.Log = diff(as.vector(log(Sbux.Q4.Trends)))

length(Sbux.Q4.Trends.Change)
length(Sbux.Q4.Earnings)

Sbux.Q4.Earnings = Sbux.Q4.Earnings[2:4]

Sbux.Fit.Q4 = lm(Sbux.Q4.Earnings ~ Sbux.Q4.Trends.Change)
Sbux.Fit.Q4
summary(Sbux.Fit.Q4)
Sbux.Corr.Q4 <- data.frame(Sbux.Q4.Earnings, Sbux.Q4.Trends.Change)
corrplot.mixed(cor(Sbux.Corr.Q4), upper = "ellipse")

Sbux.Fit.Q4.Log = lm(Sbux.Q4.Earnings ~ Sbux.Q4.Trends.Change.Log)
summary(Sbux.Fit.Q4.Log)
Sbux.Corr.Q4.Log <- data.frame(Sbux.Q4.Earnings, Sbux.Q4.Trends.Change.Log)
corrplot.mixed(cor(Sbux.Corr.Q4.Log), upper = "ellipse")

#Regressing all earnings behind each other
Sbux.All.Earnings = Starbucks.Earnings.Change
Starbucks.trends.sum = Starbucks.trends.sum[1:19]
Sbux.All.Trends.Change = diff(as.vector(Starbucks.trends.sum))
Sbux.All.Trends.Change.Log = diff(as.vector(log(Starbucks.trends.sum)))

length(Sbux.All.Earnings)
Sbux.All.Earnings = Sbux.All.Earnings[2:19]
length(Sbux.All.Trends.Change)

Sbux.Fit.All = lm(Sbux.All.Earnings ~ Sbux.All.Trends.Change)
Sbux.Fit.All
summary(Sbux.Fit.All)
Sbux.Corr.All <- data.frame(Sbux.All.Earnings, Sbux.All.Trends.Change)
corrplot.mixed(cor(Sbux.Corr.All), upper = "ellipse")

Sbux.Fit.All.Log = lm(Sbux.All.Earnings ~ Sbux.All.Trends.Change.Log)
summary(Sbux.Fit.All.Log)
Sbux.Corr.All.Log <- data.frame(Sbux.All.Earnings, Sbux.All.Trends.Change.Log)
corrplot.mixed(cor(Sbux.Corr.All.Log), upper = "ellipse")

Sbux.All.Earnings
Sbux.All.Trends.Change

#Prediction for upcoming earning 
#Q4
Sbux.Q4.Earnings
Sbux.Q4.Trends.Change
Sbux.Q4.Trends.Change.Log
Starbucks.trends.sum[20] = sum(Starbucks.trends$Starbucks[249:261])
Sbux.Q4.Trends[4]
Starbucks.trends.sum[20]
Sbux.Q4.Trends.NowDiff = Starbucks.trends.sum[20] - Sbux.Q4.Trends[4]
Sbux.Q4.Trends.NowDiff
Sbux.Q4.Trends.NowDiff.Log = log(Starbucks.trends.sum[20] - Sbux.Q4.Trends[4])
Sbux.Q4.Trends.NowDiff.Log
Q4Func <- function(x){
  y = 0.0002318*x - 0.0129544
  y
}

Sbux.i = c(Sbux.Q4.Trends[4], Starbucks.trends.sum[20])
Sbux.ii = diff(as.vector(log(Sbux.i)))
Sbux.iii = diff(as.vector(Sbux.i))
Sbux.ii
Sbux.iii
Q4Func(Sbux.Q4.Trends.NowDiff)

Q4FuncPoly <- function(x){
  y = 6E-05*x^2 + 0.0022*x - 0.1517
  y
}
Q4FuncPolyLog <- function(x){
  y = y = 38.956*x^2 + 1.8203*x - 0.1682
  y
}

Q4FuncPoly(Sbux.iii)
Q4FuncPolyLog(Sbux.ii)


#Simple Polynomial Regression based on earnings in the past 5 years

PolyFunc <- function(x){
  y = 1E-07*x^6 - 1E-05*x^5 + 0.0004*x^4 - 0.0052*x^3 + 0.0361*x^2 - 0.1258*x + 0.1654
  y
}
PolyFunc(20)

#Linear Regression from Excel for All
Starbucks.trends.sum[20] = sum(Starbucks.trends$Starbucks[249:261])
Sbux.All.Trends.NowDiff = Starbucks.trends.sum[20] - Starbucks.trends.sum[19]
Sbux.All.Trends.NowDiff
AllFuncExcelLin <- function(x){
  y = 0.0003*x + 9E-05
  y
}
AllFuncExcelLin(Sbux.All.Trends.NowDiff)

AllFuncLin <- function(x){
  y = 2.596e-04*x + 8.924e-05
  y
}
AllFuncLin(Sbux.All.Trends.NowDiff)

## Multiple Linear Regression for change in interest from previous quarter + a year ago
##Q4
Sbux.All.Trends.Change = diff(as.vector(Starbucks.trends.sum))
Sbux.All.Trends.Change[3]
Q4.immediate.num = c(3,7,11,15)
Sbux.All.Trends.Change.Q4 = Sbux.All.Trends.Change[Q4.immediate.num]
length(Sbux.Q4.Earnings)
Sbux.All.Trends.Change.Q4 = Sbux.All.Trends.Change.Q4[2:4]
Sbux.Fit.Q4.Plus = lm(Sbux.Q4.Earnings ~ Sbux.Q4.Trends.Change + Sbux.All.Trends.Change.Q4)
summary(Sbux.Fit.Q4.Plus)
Sbux.Fit.Q4.Plus

MultFunc.Q4 <- function(x,m){
  y = -0.0001131*x + 0.0085181*m + 0.3788803
  y
}

MultFunc.Q4(142,8)

Sbux.Fit.Q4.All = lm(Sbux.Q4.Earnings ~ Sbux.All.Trends.Change.Q4)
summary(Sbux.Fit.Q4.All)
Sbux.Fit.Q4.All

Func.Q4.All = function(x){ #better than 2 I believe
  y = 0.007993*x + 0.354729
  y
}

Func.Q4.All.2 = function(x){
  y = 2.96e-04*x + 8.924e-05
  y
}

Sbux.Q4.Earnings
Sbux.All.Trends.Change.Q4
Func.Q4.All(8)
Func.Q4.All.2(8)

Correl2 <- data.frame(Sbux.Q4.Earnings,Sbux.All.Trends.Change.Q4,Sbux.Q4.Trends.Change)
pairs(~Sbux.Q4.Earnings+Sbux.All.Trends.Change.Q4+Sbux.Q4.Trends.Change)
corrplot.mixed(cor(Correl2), upper = "ellipse")


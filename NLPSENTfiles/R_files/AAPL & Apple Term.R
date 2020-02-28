library('quantmod')
library('corrplot')
getSymbols(c("AAPL"), from="2015-01-19", to="2020-01-12", src="yahoo", periodicity = 'weekly')
AAPL.logret = diff(as.vector(log(AAPL$AAPL.Adjusted)))
AAPLA = diff(as.vector(AAPL$AAPL.Adjusted))
data = read.csv('AAPL TrendsC.csv', header = T)
AAPLT = data[,c("Week", "Apple")]
AAPLTA = diff(as.vector(AAPLT$Apple))
AAPLT.log = diff(as.vector(log(AAPLT$Apple)))

fitL <- lm(AAPL.logret ~ AAPLT.log)
fit <- lm(AAPLA ~ AAPLTA)

summary(fit)
summary(fitL)

length(AAPL$AAPL.Adjusted)
length(AAPLT$Apple)

AAPL
AAPL[260]
AAPLT[261,]

rates <- data.frame(AAPL.logret, AAPLT.log )
pairs(~AAPL.logret + AAPLT.log)
corrplot.mixed(cor(rates), upper = "ellipse")

##Shift in weeks, remove 2 weeks from applestock, remove 2 weeks from end of trends
AAPL2 = AAPL[3:260]
AAPLT2 = AAPLT[1:258,]

AAPLTA2 = diff(as.vector(AAPLT2$Apple))
AAPLT2.log = diff(as.vector(log(AAPLT2$Apple)))
AAPL2.logret = diff(as.vector(log(AAPL2$AAPL.Adjusted)))
AAPLA2 = diff(as.vector(AAPL2$AAPL.Adjusted))
fitL2 <- lm(AAPL2.logret ~ AAPLT2.log)
fit2 <- lm(AAPLA2 ~ AAPLTA2)

summary(fitL2)
summary(fit2)

rates2 <- data.frame(AAPL2.logret, AAPLT2.log )
corrplot.mixed(cor(rates2), upper = "ellipse")

##Shift in weeks, remove 4 weeks from applestock, remove 4 weeks from end of trends
AAPL4 = AAPL[5:260]
AAPLT4 = AAPLT[1:256,]

AAPLTA4 = diff(as.vector(AAPLT4$Apple))
AAPLT4.log = diff(as.vector(log(AAPLT4$Apple)))
AAPL4.logret = diff(as.vector(log(AAPL4$AAPL.Adjusted)))
AAPLA4 = diff(as.vector(AAPL4$AAPL.Adjusted))
fitL4 <- lm(AAPL4.logret ~ AAPLT4.log)
fit4 <- lm(AAPLA4 ~ AAPLTA4)

summary(fitL4)
summary(fit4)

rates4 <- data.frame(AAPL4.logret, AAPLT4.log )
corrplot.mixed(cor(rates4), upper = "ellipse")

##Utilizing Sums
##y1 = 1 week move Q1 announced, x1 = Q1 market Trend Sum, 
#EaringsDates = x
#Subtract by 13
AAPL[250]
AAPL[237]
AAPL[224]
AAPL[211]
AAPL[198]
AAPL[185]
AAPL[172]
x = c(3, 16, 29, 42, 55, 68, 81, 94, 107, 120, 133, 159, 172, 185, 198, 211, 224, 237, 250)
AAPLEC = AAPL$AAPL.Close[x] ##Quarterly Dates
AAPLEO = AAPL$AAPL.Open[x]
AAPLED = (AAPLEC - AAPLEO)/(AAPLEO)
AAPLED #Move one week after earnings result

AAPLTE = c() #Trend Sums
AAPLTE[1] = sum(AAPLT$Apple[1:x[1]]) 
AAPLTE[2] = sum(AAPLT$Apple[x[1]:x[2]])
AAPLTE[3] = sum(AAPLT$Apple[x[2]:x[3]])
AAPLTE[4] = sum(AAPLT$Apple[x[3]:x[4]])
AAPLTE[5] = sum(AAPLT$Apple[x[4]:x[5]])
AAPLTE[6] = sum(AAPLT$Apple[x[5]:x[6]])
AAPLTE[7] = sum(AAPLT$Apple[x[6]:x[7]])
AAPLTE[8] = sum(AAPLT$Apple[x[7]:x[8]])
AAPLTE[9] = sum(AAPLT$Apple[x[8]:x[9]])
AAPLTE[10] = sum(AAPLT$Apple[x[9]:x[10]])
AAPLTE[11] = sum(AAPLT$Apple[x[10]:x[11]])
AAPLTE[12] = sum(AAPLT$Apple[x[11]:x[12]])
AAPLTE[13] = sum(AAPLT$Apple[x[12]:x[13]])
AAPLTE[14] = sum(AAPLT$Apple[x[13]:x[14]])
AAPLTE[15] = sum(AAPLT$Apple[x[14]:x[15]])
AAPLTE[16] = sum(AAPLT$Apple[x[15]:x[16]])
AAPLTE[17] = sum(AAPLT$Apple[x[16]:x[17]])
AAPLTE[18] = sum(AAPLT$Apple[x[17]:x[18]])
AAPLTE[19] = sum(AAPLT$Apple[x[18]:x[19]])

?diff
length(AAPLE)
length(AAPLTE)

AAPLE
fitA <- lm(AAPLED[2:19] ~ AAPLTE[2:19])
summary(fitA)

ratesA <- data.frame(AAPLED[2:19], AAPLTE[2:19])
corrplot.mixed(cor(ratesA), upper = "ellipse")

AAPLED.logret = AAPL.logret[x]

AAPLTE.log = diff(as.vector(log(AAPLTE)))
length(AAPLED)
length(AAPLTE.log)
fitAL <- lm(AAPLED ~ AAPLTE.log)

summary(fitAL)
ratesAL <- data.frame(AAPLE.logret, AAPLTE.log)
corrplot.mixed(cor(ratesAL), upper = "ellipse")

plot(fitAL)

AAPLTE.log
AAPLTE

##Breaking down by quarter: Q1
y = c(3, 7, 11, 15, 19)
AAPLQ1D = AAPLED[y]
AAPLTQ1 = AAPLTE[y]
AAPLQ1D

fitQ1 <- lm(AAPLQ1D ~ AAPLTQ1)
summary(fitQ1)
ratesAQ1 <- data.frame(AAPLQ1D, AAPLTQ1)
corrplot.mixed(cor(ratesAQ1), upper = "ellipse")

##AAPLQ1.logret = AAPLE.logret[y]
AAPLQ1D2 = AAPLQ1D[2:5]
AAPLTQ1.log = diff(as.vector(log(AAPLTQ1)))

fitQ1L <- lm(AAPLQ1D2 ~ AAPLTQ1.log)
summary(fitQ1L)
ratesALQ1 <- data.frame(AAPLQ1D2, AAPLTQ1.log)
corrplot.mixed(cor(ratesALQ1), upper = "ellipse")

##Breaking Down by Q2
m = c(2, 6, 10, 14, 18)
AAPLQ2D = AAPLED[m]
AAPLTQ2 = AAPLTE[m]
AAPLQ2D

fitQ2 <- lm(AAPLQ2D ~ AAPLTQ2)
summary(fitQ2)
ratesAQ2 <- data.frame(AAPLQ2D, AAPLTQ2)
corrplot.mixed(cor(ratesAQ2), upper = "ellipse")

AAPLQ2D2 = AAPLQ2D[2:5]
AAPLTQ2.log = diff(as.vector(log(AAPLTQ2)))

fitQ2L <- lm(AAPLQ2D2 ~ AAPLTQ2.log)
summary(fitQ2L)
ratesALQ1 <- data.frame(AAPLQ1D2, AAPLTQ1.log)
corrplot.mixed(cor(ratesALQ1), upper = "ellipse")

##Breaking Down by Q3
z = c(1, 5, 9, 13, 17)
AAPLQ3D = AAPLED[z]
AAPLTQ3 = AAPLTE[z]
AAPLQ3D

fitQ3 <- lm(AAPLQ3D ~ AAPLTQ3)
summary(fitQ3)
ratesAQ3 <- data.frame(AAPLQ3D, AAPLTQ3)
corrplot.mixed(cor(ratesAQ3), upper = "ellipse")

AAPLQ3D2 = AAPLQ3D[2:5]
AAPLTQ3.log = diff(as.vector(log(AAPLTQ3)))

AAPLTQ3.log

fitQ3L <- lm(AAPLQ3D2 ~ AAPLTQ3.log)
summary(fitQ3L)
ratesALQ3 <- data.frame(AAPLQ3D2, AAPLTQ3.log)
corrplot.mixed(cor(ratesALQ3), upper = "ellipse")
fitQ3L

##Breaking Down by Q4
n = c(4, 8, 12, 16)
AAPLQ4D = AAPLED[n]
AAPLTQ4 = AAPLTE[n]
AAPLQ4D

fitQ4 <- lm(AAPLQ4D ~ AAPLTQ4)
summary(fitQ4)
ratesAQ4 <- data.frame(AAPLQ4D, AAPLTQ4)
corrplot.mixed(cor(ratesAQ4), upper = "ellipse")

AAPLQ4D2 = AAPLQ4D[2:4]
AAPLTQ4.log = diff(as.vector(log(AAPLTQ4)))

fitQ4L <- lm(AAPLQ4D2 ~ AAPLTQ4.log)
fitQ4L
summary(fitQ4L)
ratesALQ4 <- data.frame(AAPLQ4D2, AAPLTQ4.log)
corrplot.mixed(cor(ratesALQ4), upper = "ellipse")

##Q3L & Q4L appear most successful
Q3FuncL <- function(x){
  y = -0.05892*x + 0.07931
  y
}

Q4FuncL <- function(x){
  y = -0.090768*x -0.005097
  y
}

Q3Func(2)

##Estimation for Most Recent Quarter, #20 AAPLTE, part of q4 defined
length(AAPLT$Apple)
AAPLTE[20] = sum(AAPLT$Apple[250:260])
AAPLTE[20]
AAPLTE[16]
AAPLTEV = c(AAPLTE[20], AAPLTE[16])

AAPLTENOW = diff(as.vector(log(AAPLTEV)))
AAPLTENOW
Q4FuncL(AAPLTENOW)

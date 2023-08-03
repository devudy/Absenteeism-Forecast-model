
# Package installed
install.packages("dplyr")
install.packages("ggcorrplot")
install.packages("ggplot2")
install.packages("forecast") 
install.packages("tseries")
install.packages("DescTools")

# Libraries used for the program
library(psych) 
library(dplyr)
library(ggcorrplot)
library(ggplot2)
library(forecast)
library(tseries)
library(DescTools)

# Loading the data
setwd("C:/Users/dupad/Documents/Time Series")
absenteeismData = read.csv("absenteeismatwork.csv", header=TRUE)
absenteeismData

# Feature engineering year column
year_initial = 2007
year <- c(year_initial)
for (i in 2:nrow(absenteeismData)){
  if(absenteeismData$month[i] - absenteeismData$month[i-1]  == -11 ){
    year_initial <- year_initial +1
    year <- c(year, year_initial)
  }
  else {year <- c(year, year_initial)}
} 
absenteeismData$year <- year
absenteeismData


# Splitting the dataset into train and test
absenteeismtrain=slice_head(absenteeismData,n=600)
head(absenteeismtrain)
class(absenteeismtrain)

absenteeismtest=slice_tail(absenteeismData,n=137)
head(absenteeismtest)
class(absenteeismtest)

#Create summary table for dataset
head(absenteeismtrain)
dim(absenteeismtrain)
describe(absenteeismtrain)
summary(absenteeismtrain)

# Explore the dataset dataset for outliers
boxplot(log(subset(absenteeismtrain, select= -c(year)))[,-1], horizontal=TRUE, main="IQR")

# Histograms and density lines
par(mfrow=c(3, 3))
cnames <- dimnames(absenteeismtrain)[[2]]
for (i in 1:9) {
  hist(absenteeismtrain[,i],main=cnames[i], probability=TRUE, col="lightblue", border="white",  cex.main=2)
  d <- density(absenteeismtrain[,i])
  lines(d, col="blue")
}

# Correlations between variables
absentcorr <- cor(absenteeismtrain, use="complete.obs")
round(absentcorr,2)

# Visualizing data correlation in color code 
ggcorrplot(absentcorr, title= "Correlations between variables",
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)

#scatterplot for highly correlated variables
par(mfrow=c(2,1))
plot(absenteeismtrain$absenttime,absenteeismtrain$transexp)
plot(absenteeismtrain$absenttime,absenteeismtrain$children)

# Dependent feature inspection
hist(absenteeismtrain$absenttime,main="Frequency Distribution of Absent_time",xlab="absenttime", probability=TRUE, col="lightblue", border="white",  cex.main=2)
d <- density(absenteeismtrain$absenttime)
lines(d, col="blue")

#Creating time series of train and test dataset by summarizing variable values as mean and grouping by year and month
absenteeismtrain_ts <- as.ts(
  absenteeismtrain %>%
    group_by(year, month)%>%
    summarise_all(funs(mean))
)
absenteeismtrain_ts
head(absenteeismtrain_ts)
class(absenteeismtrain_ts)

absenteeismtest_ts <- as.ts(
  absenteeismtest %>%
    group_by(year, month)%>%
    summarise_all(funs(mean))
)
absenteeismtest_ts
head(absenteeismtest_ts)
class(absenteeismtest_ts)

# Time series of every features
plot.ts(absenteeismtrain_ts[,4:11],main="Time series plots", type="l",cex.main=2)

# Correlation of absenttime with other features
cor(absenteeismtrain_ts[,1:10], absenteeismtrain_ts[,11])

#Stationary test on absenttime
adf.test(absenteeismtrain_ts[,11])
kpss.test(absenteeismtrain_ts[,11])
Box.test(absenteeismtrain_ts[,11], type="Ljung-Box")

#Auto correlation functions ACF and PACF test
acf(absenteeismtrain_ts[,11])
pacf(absenteeismtrain_ts[,11])
PlotACF(absenteeismtrain_ts[,11],main="Autocorrelation plots")

#Fitting ARIMA Model
# Arima without any regressor
arima_1 <- auto.arima(absenteeismtrain_ts[,11])
arima_1
#Checking residuals for arima model
qqnorm(arima_1$residuals)
qqline(arima_1$residuals, distribution = qnorm)
checkresiduals(arima_1)

# Arima with children regressor
arima_2 <- auto.arima(absenteeismtrain_ts[,11], xreg=absenteeismtrain_ts[,9])
arima_2
#Checking residuals for arima model
qqnorm(arima_2$residuals)
qqline(arima_2$residuals, distribution = qnorm)
checkresiduals(arima_2)

# Arima with transexp regressor
arima_3 <- auto.arima(absenteeismtrain_ts[,11], xreg=absenteeismtrain_ts[,5])
arima_3
#Checking residuals for arima model
qqnorm(arima_1$residuals)
qqline(arima_1$residuals, distribution = qnorm)
checkresiduals(arima_1)

# Arima with children and transexp regressor
arima_4 <- auto.arima(absenteeismtrain_ts[,11], xreg=absenteeismtrain_ts[,9]+absenteeismtrain_ts[,5])
arima_4
#Checking residuals for arima model
qqnorm(arima_4$residuals)
qqline(arima_4$residuals, distribution = qnorm)
checkresiduals(arima_4)


#TSLM Model with features having high correlation with absenttime
# Tslm model with childen feature
tslm_1<-tslm(absenttime~childen, absenteeismtrain_ts)
tslm_1

# Tslm model with transexp feature
tslm_2<-tslm(absenttime~transexp, absenteeismtrain_ts)
tslm_2
#Checking residuals for TSLM model
qqnorm(tslm_2$residuals)
qqline(tslm_2$residuals, distribution = qnorm)
checkresiduals(tslm_2)

# Neural network test
nnetarfit<-nnetar(absenteeismtrain_ts[,"absenttime"])
nnetarfit
#Checking residuals for Neural network model
qqnorm(as.vector(residuals(nnetarfit)),main="")
qqline(nnetarfit$residuals,distribution=qnorm)
checkresiduals(nnetarfit)
PlotACF(nnetarfit$residuals)

#Forecasting
# whole data set converted to time series for forecasting
absenteeismData_ts <- as.ts(
  absenteeismData %>%
    group_by(year, month)%>%
    summarise_all(funs(mean))
)
absenteeismData_ts
# Neural network model forecast
nnetarfit %>%
  forecast(h=5) %>%
  autoplot() + autolayer(absenteeismData_ts[,"absenttime"])

#forecasted values
forecast(nnetarfit,6)

library(tseries)
library(forecast)
library(fpp)
library(DMwR2)
library(rugarch)
library(FinTS)
library(ggplot2)
library(e1071) #skewness

###############################Import data#33####################################
Data <- read.csv(file.choose())
Exc <- Data$Close
Exc <- ts(rev(Exc), start = 2020, frequency = 262); Exc
plot(Exc, col="dodgerblue4", ylab="USD/CNY",
     main="The Exchange Rate USD/CNY")

################################Take return#####################################
return <- diff(log(Exc))
plot(return,col="dodgerblue4", ylab="The diff(log(Data))",
     main="The Return of Exchange Rate " )
        boxplot(return, main = "Return of Exchange Rate USD/CNY",
        col = "lightsalmon",
        border = "brown")

################################################################################
########Replace outliers with the 5 percentile and the 95 percentile value#####
  fun <- function(x){
      quantiles <- quantile( x, c(.05, .95 ) )
      b <- boxplot(x, plot = FALSE)
          x[ x %in% b$out < quantiles[1] ] <- quantiles[1]
              x[ x %in% b$out > quantiles[2] ] <- quantiles[2]
                 x
                   }
       return1 <- fun( return )
       par(mfrow = c(1,2))
       boxplot(return, main = "Return with Outliers",
               col = "lightsalmon",
               border = "brown")
       boxplot(return1, main = "Return without Outliers",
               col = "lightsalmon",
               border = "brown")


###################################Histogram###################################
       par(mfrow = c(1,1))
     h<-hist(return1, col="lightsteelblue1", xlab="The Return of Exchange Rate USD/CNY",
        main="Histogram with Normal Curve", breaks = 21)
         xfit<-seq(min(return1),max(return1),length=40)
          yfit<-dnorm(xfit,mean=mean(return1),sd=sd(return1))
           yfit <- yfit*diff(h$mids[1:2])*length(return1)
            lines(xfit, yfit, col="lightsalmon", lwd=2)
          

##############################Descriptive Statistic###############################
            Des <- function(x){
               a <- mean(x)
               b <- skewness(x) 
               c <- kurtosis(x)
               d <- sd(x)
               e <- jarque.bera.test(x)$p.value
                  l <- rbind(c('mean','skewness','kurtosis','Stdv','J-B Test'),c(a,b,c,d,e) )
              returnValue(l)
            }
    Descriptive <- Des(return1)
    Descriptive

##############################Check Stationary####################################
adf1 = adf.test(Exc, k = 12); adf1
adf2 = adf.test(return1, k = 12); adf2


#############################################################
##############Check Structural Break########################

library(strucchange)
plot.ts(return1)
model1 <- Fstats(return1~1, from = 0.0000001); model1
sctest(model1)
strucchange::breakpoints(return1~1)

################################ FIT ARIMA######################################
par(mfrow = c(1,2))
acf(return1, main = "ACF for Return")
pacf(return1, main = "PACF for Return")

###############################################################################
#############################Estimate ARIMA####################################
arima0 = arima(return1, order = c(0,0,0)); arima0; BIC(arima0)

arima1 = arima(return1, order = c(1,0,0)); arima1; BIC(arima1)
   coeftest(arima4)

arima2 = arima(return1, order = c(2,0,0)); arima2; BIC(arima2)
      coeftest(arima2)

arima3 = arima(return1, order = c(0,0,1)); arima3; BIC(arima3)
     coeftest(arima3)

arima4 = arima(return1, order = c(1,0,1)); arima4; BIC(arima4)
      coeftest(arima4)

arima5 = arima(return1, order = c(2,0,1)); arima5; BIC(arima5)
       coeftest(arima5)

arima6 = arima(return1, order = c(0,0,2)); arima6; BIC(arima6)
        coeftest(arima6)

arima7 = arima(return1, order = c(1,0,2)); arima7; BIC(arima7)
          coeftest(arima7)

arima8 = arima(return1, order = c(2,0,2)); arima8; BIC(arima8)
          coeftest(arima8)

arima9 = arima(return1, order = c(4,0,1)); arima9; BIC(arima9)
           coeftest(arima9)

#########################Diognostic############################################
Box.test(resid(arima0), type="Ljung", lag =36)
Box.test(resid(arima1), type="Ljung", lag =36)
Box.test(resid(arima2), type="Ljung", lag =36)
Box.test(resid(arima3), type="Ljung", lag =36)
Box.test(resid(arima4), type="Ljung", lag =36)
Box.test(resid(arima5), type="Ljung", lag =36)
Box.test(resid(arima6), type="Ljung", lag =36)
Box.test(resid(arima7), type="Ljung", lag =36)
Box.test(resid(arima8), type="Ljung", lag =36)
Box.test(resid(arima9), type="Ljung", lag =36)

############################In-Sample Forecast################################
model1 <- return1
train_series <- model1[1:251]
test_series <- model1[252:261]
arima4<- arima(train_series, order = c(0,0,1))
fcast1 <- forecast(arima4, h =10)
library(forecast)
test_forecast(actual = return1, forecast.obj = fcast1, test= test_series)
accuracy(arima4)

##########################Out-of-Sample Forecast ARIMA##########################
forecastA <- forecast(arima3, h =10)
forecast()
autoplot(forecastA)

###############################################################################
###########################Check ARCH Effect###################################
resid <- arima3$residuals
resid1 <- resid^2
ArchTest(resid1)


## Estimate EGarch
spec1 = ugarchspec(variance.model = list(model = 'eGARCH', garchOrder = c(1, 1)), distribution = 'std',
                   mean.model = list(armaOrder = c(0,0), include.mean = TRUE))

     egarch1<- ugarchfit(spec1,diff.Exc, solver = 'solnp')
         egarch1
         
         
forecast1 <- ugarchforecast(egarch1, data = NULL, n.ahead = 10); 
         forecast1
k <- sigma(forecast1)



        
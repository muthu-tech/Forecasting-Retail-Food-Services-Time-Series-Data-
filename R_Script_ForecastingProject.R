> setwd("C:/Users/muthu/Desktop/ECON 5337/Project")
> rawData = read.csv('RSAFSNA.csv')
> retail=ts(rawData[,2],start=c(1992,1),frequency=12 )
> retailHold=ts(rawData[,2],start=c(1992,1),end= c(2016,11),frequency=12 )
> retail=ts(rawData[,2],start=c(1992,1),frequency=12 )
> retailHold=ts(rawData[,2],start=c(1992,1),end= c(2016,11),frequency=12 )
> plot(retail)
> plot(log(retail))
> library(urca)
Warning message:
package ‘urca’ was built under R version 3.3.3 
> adfTest = ur.df(log(retailHold),type ='trend' ,lags= 24, selectlags='AIC')
> summary(adfTest)
> library(forecast)
Warning message:
package ‘forecast’ was built under R version 3.3.3 
> rho = acf(diff(log(retailHold)),48)
> rho
> smallR = pacf(diff(log(retailHold)),48)
> smallR
> Arima(log(retailHold),order =c(1,1,1),seasonal= c(1,0,0))
> Arima(log(retailHold),order =c(2,1,1),seasonal= c(1,0,0))
> Arima(log(retailHold),order =c(3,1,1),seasonal= c(1,0,0))
> Arima(log(retailHold),order =c(3,1,2),seasonal= c(1,0,0))
> Arima(log(retailHold),order =c(1,1,1),seasonal= c(1,0,1))
> Arima(log(retailHold),order =c(2,1,1),seasonal= c(1,0,1))
> Arima(log(retailHold),order =c(3,1,1),seasonal= c(1,0,1))
> Arima(log(retailHold),order =c(3,1,2),seasonal= c(1,0,1))
> Arima(log(retailHold),order =c(1,1,1),seasonal= c(2,0,1))
> Arima(log(retailHold),order =c(2,1,1),seasonal= c(2,0,1))
> Arima(log(retailHold),order =c(3,1,1),seasonal= c(2,0,1))
> Arima(log(retailHold),order =c(3,1,2),seasonal= c(2,0,1))
>finalModel = Arima(log(retailHold),order =c(3,1,2),seasonal= c(2,0,1))
> diagnosticCheck = acf(finalModel$residuals,48)
> Box.test(finalModel$residuals, 36, type ='Ljung-Box')
> Box.test(finalModel$residuals, 24, type ='Ljung-Box')
> library(forecast)
> foreHold = forecast.Arima(finalModel,lambda = 0 ,h =4)
> foreHold
> upperTS = ts(foreHold$upper[,2], start = c(2016,12),frequency=12)
> lowerTS = ts(foreHold$lower[,2], start = c(2016,12),frequency=12)
> plot(cbind(foreHold$mean,upperTS,lowerTS,window(retailHold,start=c(2016,8))),plot.type="single",ylab="Forecast",col=c("blue","black","black","green"),lty=c("solid","dashed","dashed","solid"))
> modelForecast = Arima(log(retail),order=c(3,1,2),seasonal=c(2,0,1))
> modelForecast
> forecast
> forecast = forecast.Arima(modelForecast,lambda = 0 ,h =2)
Warning message:
In InvBoxCox(pred$pred, lambda, biasadj, var(residuals(object),  :
  biasadj information not found, defaulting to FALSE.
> forecast
> upperTS = ts(forecast$upper[,2], start = c(2017,4),frequency=12)
> upperTS

> lowerTS = ts(forecast$lower[,2], start = c(2017,4),frequency=12)
> lowerTS
> plot(cbind(forecast$mean,upperTS,lowerTS,window(retail,start=c(2016,2))),plot.type="single",ylab="Forecast",col=c("blue","black","black","green"),lty=c("solid","dashed","dashed","solid"))


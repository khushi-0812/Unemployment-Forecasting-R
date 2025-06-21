library(forecast)
install.packages("lmtest")
library(lmtest)
data=read.csv("C:/Users/91911/Downloads/UNEMPLOY.csv")
data


# Create a time series object
ts_data = ts(data$UNEMPLOY, start = c(1948,1) , frequency = 12);ts_data



## Plot the time series
plot(ts_data, type = "l", col = "red", xlab = "Time", ylab = "Thousands Of Persons", main = "Monthly Unemployment Level Rate Over Time")

## ACF plot
acf(ts_data,main="ACF plot", col = "red")

## PACF plot
pacf(ts_data,main="PACF plot",col="red")

## ADF test
install.packages("tseries")
library(tseries)
adf_result=adf.test(ts_data);adf_result
if(adf_result$p.value<0.05)
{
cat("The time series is stationary(reject the null hypothesis).\n")
}else{
cat("The time series is non stationary(fail to reject the null hypothesis).\n")
}


## Durbin-Waston test
ar1_model = Arima(ts_data, order = c(1, 0, 0), seasonal = c(0, 0, 0))
residuals_ar1 = residuals(ar1_model)
dw_test_result = dwtest(residuals_ar1 ~ 1)
print(dw_test_result)



## Fitting an AR model
results=data.frame(Lag=integer(),AIC=numeric(),BIC=numeric(),AR_coefficient=list())
for(lag in 1:5){
ar_model=Arima(ts_data,order=c(lag,0,0))
AR_AIC=AIC(ar_model)
AR_BIC=BIC(ar_model)
AR_coefs=coef(ar_model)
results = rbind(results, data.frame(Lag = lag, AIC = AR_AIC ,BIC = AR_BIC, Coefficients = I(list(AR_coefs))))
}
results
min_aic_ar_model =results[which.min(results$AIC), ];min_aic_ar_model
min_bic_ar_model = results[which.min(results$BIC), ];min_bic_ar_model
write.csv(results,file='AR_result.csv')


## Fitting an MA model
results2=data.frame(Lag2=integer(),MA_AIC=numeric(),MA_BIC=numeric(),MA_coefficient=list())
for(lag2 in 1:5){
ma_model=Arima(ts_data,order=c(0,0,lag2))
ma_AIC=AIC(ma_model)
ma_BIC=BIC(ma_model)
ma_coefs=coef(ma_model)
results2 = rbind(results2, data.frame(Lag2 = lag2, MA_AIC = ma_AIC ,MA_BIC = ma_BIC, Coefficients = I(list(ma_coefs))))
}
results2
min_aic_ma_model =results2[which.min(results2$MA_AIC), ];min_aic_ma_model
min_bic_ma_model = results2[which.min(results2$MA_BIC), ];min_bic_ma_model
write.csv(results2,file='MA_result.csv')













## Fitting an ARIMA model
model=auto.arima(ts_data);model

## Residual analysis
residual_series = residuals(model)
print(residual_series)
plot(residual_series,col="red",main="Residual series plot")

acf(residual_series,main="ACF Plot for residual series",col="red")

## qq plot
qqnorm(residual_series)
qqline(residual_series, col = "red") ## add a reference line ##

## BOX test
Box_test=Box.test(residual_series, lag = 10, type = "Ljung-Box");Box_test
if(Box_test$p.value<0.05)
{
cat("There is significant autocorrelation in the residuals.\n")
}else{
cat("autocorrelation is not present in the residuals.\n")
}


## Forecast the next 10 periods 
forecast_values = forecast(model, h = 10)
forecast_values
plot(forecast_values,col="red")







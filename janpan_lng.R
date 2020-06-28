########################################################################################
# This is a small part of the code I wrote before to conduct time series analysis on Janpan's Oil & Liquid Natural Gas imports from 1997 to 2019
# Email: ruoshui.li@duke.edu
# Last edit on April 12th 2020

# Input data: Liquid Natural Gas imports data to Japan
#             from 1997.1 to 2019.3 (latest data); 

##############################################################################################################

library(forecast)
library(tseries) 
library(sarima)
library(Kendall)
library(smooth)

lng <- read.csv('Japan_LNG.csv')

# first plot two data series to have a look
lng_ts <- ts(lng[,2],start=c(1997,1),frequency = 12)
plot(lng_ts, xlab = 'Time', ylab = 'LNG imports (kL)',main='Liquid Natural Gas imports to Japan from 1997 to 2019')  ## slightly increase trend

##################### Part 1 ##########################

lng_before <- window(lng_ts, c(1997,1), c(2011,3))
lng_fit <- window(lng_before, c(1997,1), c(2010,3))
lng_test <- window(lng_before, c(2010,4), c(2011,3))
lng_comparison <- window(lng_ts, c(2011,4), c(2014,4))

decomp=decompose(lng_fit)
deseasonal_lng=seasadj(decomp)

###### firts on raw data
adf.test(lng_fit,alternative='stationary') 
lng_test_dif= diff(lng_fit,1)
adf.test(lng_test_dif,alternative='stationary')
# plot acf and pacf
par(mfrow=c(1,3))
plot(lng_fit, type='l', xlab = 'Time', ylab = 'LNG imports (kL)',main='Plot of LNG imports to Japan from 1997 to 2011')
Acf(lng_fit,lag.max=40,main='ACF of LNG imports') 
Pacf(lng_fit,lag.max=40,main='PACF of LNG imports') 
# use observed p,q to fit data
fit_model_lng_0=Arima(lng_fit,order=c(1, 1, 0),seasonal=c(1,0,0),include.drift = FALSE)
lng_for_0=forecast(fit_model_lng_0, h=12)
par(mfrow=c(1,1))
plot(lng_for_0, xlab='Time',ylab='Amount', main='LNG imports forecast from observed SARIMA model')
# test accuracy
accuracy(lng_for_0$mean, lng_test)
# now autofit and use arima function to forecast raw data series
fit_model_lng_1 <- auto.arima(lng_fit)
lng_for_1=forecast(fit_model_lng_1, h=12)
par(mfrow=c(1,1))
plot(lng_for_1, xlab='Time',ylab='Amount', main='LNG imports forecast from auto fitted SARIMA model')
## test accuracy
accuracy(lng_for_1$mean, lng_test)

###### then on deseasonal data
adf.test(deseasonal_lng,alternative='stationary') 
deseasonal_lng_dif= diff(deseasonal_lng,1)
adf.test(deseasonal_lng_dif,alternative='stationary')
# plot acf and pacf
par(mfrow=c(1,3))
plot(deseasonal_lng, type='l', xlab = 'Time', ylab = 'LNG imports (kL)',main='Deseasonalized LNG imports to Japan from 1997 to 2019')
Acf(deseasonal_lng,lag.max=40,main='ACF of deseasonalized LNG imports') 
Pacf(deseasonal_lng,lag.max=40,main='PACF of deseasonalized LNG imports') 
# use observed p,q to fit data
fit_model_lng_2=Arima(deseasonal_lng,order=c(1, 1, 0),include.drift = TRUE)
lng_for_2=forecast(fit_model_lng_2, h=12)
par(mfrow=c(1,1))
plot(lng_for_2, xlab='Time',ylab='Amount',main='Deseasonalized LNG imports trend forecast from observed ARIMA model')
# add seasonal component back
decomp_all=decompose(lng_ts)
seasonal_part=decomp_all$seasonal
seasonal_test=seasonal_part[4:15]
lng_for_2_2=lng_for_2$mean+seasonal_test
plot(lng_for_2, xlab='Time',ylab='Amount',main="Deseasonalized LNG imports forecast from observed ARIMA model")
lines(lng_for_2_2, col='green',lwd=2)
accuracy(lng_for_2_2, lng_test)
# now autofit and use arima function to forecast on deseasoned data
fit_model_lng_3=auto.arima(deseasonal_lng, max.D=0, max.P=0, max.Q=0)
lng_for_3=forecast(fit_model_lng_3, h=12)
# add seasonal part back 
lng_for_3_2=lng_for_3$mean+seasonal_test
plot(lng_for_3, xlab='Time',ylab='Amount',main="Deseasonalized LNG imports forecast from auto fitted ARIMA model")
lines(lng_for_3_2, col='green',lwd=2)
accuracy(lng_for_3_2, lng_test)


###### Seasonal naive forecast
fit_snai_lng=snaive(lng_fit,h=12)
print(fit_snai_lng)
plot(fit_snai_lng,xlab='Time',ylab='Amount',main="LNG imports forecast from Seasonal Naive model")
accuracy(fit_snai_lng$mean, lng_test)

###### SMA model
fit_mamodel_lng=sma(lng_fit,h=12, holdout=FALSE, silent=FALSE,order=2)
summary(fit_mamodel_lng)
accuracy(fit_mamodel_lng$forecast, lng_test)

###### Exponential model in State Spacing
fit_exmodel_lng=es(lng_fit,model="ZZZ",h=12, holdout=FALSE, silent=FALSE)
summary(fit_exmodel_lng)
accuracy(fit_exmodel_lng$forecast, lng_test)

###### State space model
fit_ss_lng=StructTS(lng_fit,type="BSM")
for_ss_lng=forecast(fit_ss_lng,h=12)
plot(for_ss_lng)
accuracy(for_ss_lng$mean, lng_test)

##### Put all forecasts together
plot(times, as.vector(lng_before), type='l',xlim=c(as.Date("2010/4/1"),as.Date("2011/3/1")), ylim= c(3200000,  8200000), col='black', xlab='Time',ylab='Amount(kL)',main="Comparison of forecasted LNG imports to Japan from 2010 to 2011", xaxt="n")
lines(times[160:171],as.vector(lng_for_3_2), col='blue', lwd=1.5)
lines(times[160:171],as.vector(lng_for_1$mean), col='green', lwd=1.5)
lines(times[160:171],as.vector(fit_snai_lng$mean), col='purple', lwd=1.5)
lines(times[160:171],as.vector(fit_mamodel_lng$forecast), col='red', lwd=1.5)
lines(times[160:171],as.vector(fit_exmodel_lng$forecast), col='pink', lwd=1.5)
lines(times[160:171],as.vector(for_ss_lng$mean), col='yellow', lwd=1.5)
xtick<-seq(as.Date("2010/4/1"),as.Date("2011/3/1"), by='quarter')
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], labels = xtick, pos=1, xpd = TRUE)
legend("bottomright",
       legend=c("Raw data", "ARIMA model", "SARIMA model",
                "Seasonal Naive model", "Simple MA model", "Exponential Smoothing model",
                "State Spacing model"), 
       col=c('black', 'blue','green','purple','red', "pink",'yellow'),
       ncol=2,text.width=40,lwd=2,cex=0.7,bty='n')


# decide to use Auto fitted Arima model to forecast LNG imports after 2011.3
fit_lng_2011=auto.arima(lng_before, max.D=0, max.P=0, max.Q=0)
print(fit_lng_2011)
# gives ARIMA(1,1,2)
lng_for_2011=forecast(fit_lng_2011, h=36)
# add seasonal part back
decomp=decompose(lng_before)
seasonal_comp=decomp$seasonal[4:39]
lng_for_2011_2=lng_for_2011$mean+seasonal_comp
plot(lng_before, ylim=c(3200000, 8000000), xlim=c(1997, 2015),xlab='Time',ylab='Amount',main=c(paste("Comparison between actual LNG imports and"), paste("forecasted amounts assuming no Fukushima Event"), paste("from 2011 to 2014")))
lines(lng_for_2011_2, col='green',lwd=2)
lines(lng_comparison, col='blue', lwd=2) 
legend("topleft",
       legend=c('Raw data', 'Forecasted imports', 'Actual imports'),
       col=c('black', 'green','blue'),
       ncol=1, text.width=5, lwd=2,cex=0.8,bty='n')



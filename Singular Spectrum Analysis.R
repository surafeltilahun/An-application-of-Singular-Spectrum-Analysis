

SSA1<-ssa(train.split,L=76)#decompose the time series using ssa, here L is window size
plot(SSA1)# Plot of eigenvalues
plot(SSA1, type = "vectors", idx=1:18)#plot eigenvecors, here  'idx' argument specifies the lenght of vectors of desired
# Plot of elementary reconstructed series
# Here ’groups’ argument specifies the grouping
plot(wcor(SSA1),grid =16)#plot w-correlation, here grid denote index that vector consided for this analysis
plot(SSA1, type = "paired", idx = 1:18, plot.contrib = FALSE)#plot paired aigenvectors




#first stage reconstrunction
res1 <- reconstruct(SSA1, groups = list(c(1,8),c(2,3),c(4,5),c(6,7),c(9,10),c(12,13),c(15,16)))#reconstruct the series using pairs of vectors, here groups denote the indices of vectors of interst


trend<-ssa((residuals(SSA1))^2,L=76)#extract residuals from the decomposed series
rsd <- sqrt(reconstruct(trend, groups=list(c(1,8)))$F1)#reconstract the series using the trend components
plot(residuals(SSA1), type="l",ylab="Residuals") #extracts the seasonality from trend and put it in envelop
lines(rsd, type="l",col="red",lwd=0.4)
lines(-rsd, type="l",col="red",lwd=0.4)
plot(train.split-residuals(SSA1))


res1<-reconstruct(SSA1,groups = c(1,8))#reconstruct the decomposed series using the trend components
p<-periodogram(residuals(res1))#inspect the frequence in the residual of trend. or extract the seasonality from trend
dd = data.frame(freq=p$freq, spec=p$spec)#put the frequency and spec on the series in dataframe
order = dd[order(-dd$spec),]#arrange the frequencies in order
top2 = head(order,7)#print the first 7 frequencies and specs
time = 1/top2$f#change the frequencey into period/time
plot(dd,type="l",lwd=2,xlab="Frequency (Hz)",xlim=c(0,0.55))#plot the frequency of seasonality in series


#Forecast on training and test dataset

fit.arima.visitors<-auto.arima(train.visitors)#fit arima
forecast.arima.train<-forecast(fit.arima.visitors,h=36) %>% accuracy(visitors)#Forecast total visitors arrival in NZ from 2016 to 2019. h denot the forecasting horizon of interest
ARIMA<-forecast.arima.train[,c("MAPE","MASE")]# print MAPE and MASE of ARIMA forecast
checkresiduals(fit.arima.visitors)#check the residual of arima
shapiro.test(residuals(fit.arima.visitors))#check the normality of fitted arima model

fit.ets<-ets(train.visitors)#fit ets forecastin model
forecast.ets.train<-forecast(fit.ets,h=36) %>% accuracy(visitors)#forecast total visitors arrival in NZ from 2016 to 2019. h denote the forecasting horizon of intrest
EST<-forecast.ets.train[,c("MAPE","MASE")]#print MAPE and MASE of ets forecast

SSA1<-ssa(train.split,L=51)#fit ssa forecasting technique
ssa.forecast.train<-forecast(SSA1, groups = list(1:16), method = "recurrent", len =36) %>% accuracy(visitors)#forecast total visitors arrival in NZ from 2016 to 2019. h denote the forecasting horizon of interest, groups denote the indices of eigenvectors of interes
SSA<-ssa.forecast.train[,c("MAPE","MASE")]#print MAPE and MASE of ssa forecast


fit.ssa.full<-ssa(visitors,L=76)#fit ssa on total visitors arrival original data set, 'L' is window size
full.forecase.ssa<-forecast(fit.ssa.full,h=36,groups=list(1:16),method="recurrent")#forecast total visitors arrival in NZ, from 2019 to 2022. groups denote the desired eigenvectros to forecast the series
plot(cbind(visitors,full.forecase.ssa$fitted,full.forecase.ssa$mean),plot.type = "single", col = c("black","orangered1","deepskyblue"), ylab = "Total visitors arrival in NZ",xlim=c(2000,2022),level=c(80,95))#plot the forecast of the series 
op <- par(cex = 1.2)#customize the size of the legend on the plot
legend(2000,5.8e+05,legend=c("Original series","Training","Forecast"),col=c("black","orangered1","deepskyblue"),lty=1:3,cex=0.6)#attach legend on the plot

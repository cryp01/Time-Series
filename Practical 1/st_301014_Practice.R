# Time Series Practise 1
# 23-11-2014

# Series 1
#
# Number of vehicles registered in Catalonia. 
# Monthly data for 1970.1 to 2009.12. 
# Source: General Directorate of Traffic 

# install.packages("tseries")
library(tseries)


matric<-ts(read.table("matric.dat"),start=1970,frequency=12)
plot.ts(matric,col=4)

par(mfrow=c(2,1))   
acf(matric,ylim=c(-1,1))
pacf(matric,ylim=c(-1,1))

# transformació de les dades - Transformation of data 

# tendencia lineal - Linear trend

trend<-lm(matric~c(1:length(matric)))
matric_lt<-ts(residuals(trend),start=1970,frequency=12)
par(mfrow=c(1,1))       
plot.ts(matric_lt,col=4)

par(mfrow=c(2,1))   
acf(matric_lt,ylim=c(-1,1))
pacf(matric_lt,ylim=c(-1,1))

# diferència - difference

dmatric=diff(matric,12)
par(mfrow=c(1,1))   
ts.plot(dmatric,col=4)

par(mfrow=c(2,1))   
acf(dmatric,ylim=c(-1,1))
pacf(dmatric,ylim=c(-1,1))

# Estimació - Estimation

model<-arima(dmatric,c(3,1,1))
model

# Anàlisi residus - Residual Analysis

par(mfrow=c(2,1))
acf(model$residuals,ylim=c(-1,1))
pacf(model$residuals,ylim=c(-1,1))

layout(matrix(1, nrow = 1, ncol = 1))   
ts.plot(dmatric,model$residuals, col=c(1,2))

# Predicció - Prediction 

modelf<-predict(model,n.ahead=24)
par(mfrow=c(1,1))
ts.plot(dmatric,modelf$pred, col=c(1,4))
lines(modelf$pred+2*modelf$se, col="red")
lines(modelf$pred-2*modelf$se, col="red")
modelf

# Sèrie 2
#
# "Anomaly" in global mean temperature (difference relative to a 
# reference value set in analyzes of climate change).  
# Monthly data from 1880.1 to 2009.9. 
# 
# Source: Hadley Centre of the UK Met Office 

had<-ts(read.table("had.dat"),start=1880,frequency=12)
par(mfrow=c(1,1))   
plot.ts(had,col=c(4))

par(mfrow=c(2,1))   
acf(had,ylim=c(-1,1))
pacf(had,ylim=c(-1,1))

# tendencia lineal - linear trend

trend<-lm(had~c(1:length(had)))
had_lt<-ts(residuals(trend),start=1880,frequency=12)
par(mfrow=c(1,1))   
plot.ts(had_lt,col=c(4))

par(mfrow=c(2,1))   
acf(had_lt,ylim=c(-1,1))
pacf(had_lt,ylim=c(-1,1))

# diferència - difference

dhad=diff(had,12)
par(mfrow=c(1,1))   
ts.plot(dhad,col=4)

par(mfrow=c(2,1))   
acf(dhad,ylim=c(-1,1))
pacf(dhad,ylim=c(-1,1))

# Diferència estacional - Seasonal Difference

ddhad<-diff(dhad,lag=12)
par(mfrow=c(1,1))
plot(ddhad,col=4)

par(mfrow=c(2,1))
acf(ddhad,ylim=c(-1,1))
pacf(ddhad,ylim=c(-1,1))

par(mfrow=c(2,1))
acf(ddhad,ylim=c(-1,1),lag=100)
pacf(ddhad,ylim=c(-1,1),lag=100)

# Estimació - Estimation

model2<-arima(ddhad,c(2,0,1))
model2

# Anàlisi residus - Residual Analysis

par(mfrow=c(2,1))
acf(model2$residuals,ylim=c(-1,1))
pacf(model2$residuals,ylim=c(-1,1))

layout(matrix(1, nrow = 1, ncol = 1))   
ts.plot(ddhad,model2$residuals, col=c(1,2))


# Predicció - Prediction 

model2f<-predict(model2,n.ahead=30)
par(mfrow=c(1,1))
ts.plot(ddhad,model2f$pred, col=c(1,4))
lines(model2f$pred+2*model2f$se, col="red")
lines(model2f$pred-2*model2f$se, col="red")
model2f




# Sèrie 3
#
# Deaths in road accidents in the US. 
# Monthly data from 1973.1 to 1978.12

accid<-ts(read.table("accidents.dat"),start=1973,frequency=12)
par(mfrow=c(1,1))
plot.ts(accid,col=c(4))

par(mfrow=c(2,1))   
acf(accid,ylim=c(-1,1))
pacf(accid,ylim=c(-1,1))

# tendencia lineal - linear trend

trend<-lm(accid~c(1:length(accid)))
accid_lt<-ts(residuals(trend),start=1880,frequency=12)
par(mfrow=c(1,1))   
plot.ts(accid_lt,col=c(4))

par(mfrow=c(2,1))   
acf(accid_lt,ylim=c(-1,1),lag=200)
pacf(accid_lt,ylim=c(-1,1), lag=200)

# diferència - difference

daccid=diff(accid,12)
par(mfrow=c(1,1))   
ts.plot(daccid,col=4)

par(mfrow=c(2,1))   
acf(daccid,ylim=c(-1,1),lag=200)
pacf(daccid,ylim=c(-1,1),lag=200)

# Diferència estacional - Seasonal Difference

ddaccid<-diff(daccid,lag=12)
par(mfrow=c(1,1))
plot(ddaccid,col=4)

par(mfrow=c(2,1))
acf(ddaccid,ylim=c(-1,1))
pacf(ddaccid,ylim=c(-1,1))

par(mfrow=c(2,1))
acf(ddaccid,ylim=c(-1,1),lag=200)
pacf(ddaccid,ylim=c(-1,1),lag=200)

# Estimació - Estimation

model3<-arima(accid,c(1,0,1))
model3

# Anàlisi residus - Residual Analysis

par(mfrow=c(2,1))
acf(model3$residuals,ylim=c(-1,1))
pacf(model3$residuals,ylim=c(-1,1)
     

# Predicció - Prediction 

model3f<-predict(model3,n.ahead=12)
par(mfrow=c(1,1))
ts.plot(accid,model3f$pred, col=c(1,4))
lines(model3f$pred+2*model3f$se, col="red")
lines(model3f$pred-2*model3f$se, col="red")
model3f






#######


# ARIMA(3,1,1) 
arima(dmatric, order = c(3,0,1))

# ARIMA(3,1,1) forecasting
mydata.arima311 <- arima(dmatric, order = c(3,0,1))
mydata.pred1 <- predict(mydata.arima311, n.ahead=24)
par(mfrow=c(1,1))
plot (dmatric)
lines(mydata.pred1$pred, col="blue")
lines(mydata.pred1$pred+2*mydata.pred1$se, col="red")
lines(mydata.pred1$pred-2*mydata.pred1$se, col="red")
mydata.pred1
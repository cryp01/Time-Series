# SÈRIES TEMPORALS
# 11-12-2014


# COINTEGRACIÓN


library(lmtest)

#France

par(mfrow=c(3,3))

CPIFR<-read.csv("serie3.dat")
CPIFR<-ts(CPIFR,start=1988.01,frequency=12)
par(mfrow=c(2,1))
ts.plot(CPIFR,col=c(4))

par(mfrow=c(2,1))
acf(CPIFR,ylim=c(-1,1),main="CPIFR")
pacf(CPIFR,ylim=c(-1,1),main="CPIFR")


#Italy

CPIIT<-read.csv("practicaCSV-Italy.csv")
CPIIT<-ts(CPIIT,start=1981,frequency=12)
par(mfrow=c(2,1))
ts.plot(CPIIT,col=c(4))

par(mfrow=c(2,1))
acf(CPIIT,ylim=c(-1,1),main="CPIIT")
pacf(CPIIT,ylim=c(-1,1),main="CPIIT")

#France/Italy Exchange - TC

TC<-read.csv("practicaCSV-TC.csv")
TC<-ts(TC,start=1981,frequency=12)
par(mfrow=c(2,1))
ts.plot(TC,col=c(4))

par(mfrow=c(2,1))
acf(TC,ylim=c(-1,1),main="TC")
pacf(TC,ylim=c(-1,1),main="TC")

#log
#France_Italy_TC

lCPIFR<-log(CPIFR)
lCPIIT<-log(CPIIT)
lTC<-log(TC)

par(mfrow=c(3,3))
ts.plot(lCPIFR,col=c(4))
ts.plot(lCPIIT,col=c(4))
ts.plot(lTC,col=c(4))
acf(lCPIFR,ylim=c(-1,1),main="lCPIFR")
acf(lCPIIT,ylim=c(-1,1),main="lCPIIT")
acf(lTC,ylim=c(-1,1),main="lTC")
pacf(lCPIFR,ylim=c(-1,1),main="lCPIFR")
pacf(lCPIIT,ylim=c(-1,1),main="lCPIIT")
pacf(lTC,ylim=c(-1,1),main="lTC")

#Log value of France, Italy and TC
par(mfrow=c(1,1))
ts.plot(lCPIFR,lCPIIT,lTC,col=c(2,3,4))

library(urca)

# Test ADF

#France
summary(ur.df(lCPIFR,"trend",lags=0))
summary(ur.df(lCPIFR,"drift",lags=0))
summary(ur.df(lCPIFR,"none",lags=0))

#Italy
summary(ur.df(lCPIIT,"trend",lags=0))
summary(ur.df(lCPIIT,"drift",lags=0))
summary(ur.df(lCPIIT,"none",lags=0))

#Fr&It Exchange
summary(ur.df(lTC,"trend",lags=0))
summary(ur.df(lTC,"drift",lags=0))
summary(ur.df(lTC,"none",lags=0))

#Fr&It Exchange
DFTC<-diff(lTC)
par(mfrow=c(3,1))
ts.plot(DFTC,col=c(4))
acf(diff(DFTC),ylim=c(-1,1),main="TC Difference")
pacf(diff(DFTC),ylim=c(-1,1),main="TC Difference")

#Fr&It Exchange - Difference
summary(ur.df(DFTC,"trend",lags=0))
summary(ur.df(DFTC,"drift",lags=0))
summary(ur.df(DFTC,"none",lags=0))


FrV<-read.table("practicaCSV-France.csv")
il<-ts(il,start=1981,frequency=12)

i3<-read.table("practicaCSV-TC.csv")
i3<-ts(i3,start=1981,frequency=12)

par(mfrow=c(1,1))
ts.plot(i3,il,col=c(1,2))

par(mfrow=c(2,1))
acf(i3,ylim=c(-1,1),main="i3")
pacf(i3,ylim=c(-1,1),main="i3")

acf(il,ylim=c(-1,1),main="il")
pacf(il,ylim=c(-1,1),main="il")

# Diferenciación de las series

par(mfrow=c(2,2))
acf(diff(i3),ylim=c(-1,1),main="Diferències i3")
acf(diff(il),ylim=c(-1,1),main="Diferències il")
pacf(diff(i3),ylim=c(-1,1),main="Diferències i3")
pacf(diff(il),ylim=c(-1,1),main="Diferències il")

# Test grado de integración

library(urca)

# Test ADF

summary(ur.df(i3,"trend",lags=0))
summary(ur.df(i3,"drift",lags=0))
summary(ur.df(i3,"none",lags=0))


# Test Phillips-Perron

summary(ur.pp(i3p,"Z-tau"))
summary(ur.pp(i3,"Z-alpha"))
summary(ur.pp(i3,"Z-tau","trend"))
summary(ur.pp(i3, "Z-alpha","trend"))

# Test KPSS
summary(ur.kpss(i3,"mu"))
summary(ur.kpss(i3,"tau"))


# Modelo en diferencias ?

par(mfrow=c(2,1))
ts.plot(diff(i3),diff(il),col=c(1,2))
ts.plot(i3,il,col=c(1,2))


difer<-il-i3

par(mfrow=c(1,1))
ts.plot(difer)

summary(ur.df(difer,"drift",lags=5))

# EJEMPLO 2 - PRECIO CAFE

col<-read.table("cafe_col.dat")
col<-ts(col,start=1963,frequency=4)

alt<-read.table("cafe_alt.dat")
alt<-ts(alt,start=1963,frequency=4)

ts.plot(col,alt,col=c(1,2))

par(mfrow=c(2,1))
acf(col,ylim=c(-1,1),main="col")
pacf(col,ylim=c(-1,1),main="col")

acf(alt,ylim=c(-1,1),main="alt")
pacf(alt,ylim=c(-1,1),main="alt")

# Test grado integración

summary(ur.df(col,"trend",lags=5))
summary(ur.df(col,"drift",lags=5))

summary(ur.df(alt,"trend",lags=5))
summary(ur.df(alt, "drift",lags=5))

# Test cointegración

model<-lm(alt~col)
summary(model)
res<-ts(residuals(model))

par(mfrow=c(1,1))
ts.plot(res)

par(mfrow=c(2,1))
acf(res,ylim=c(-1,1),main="residus model lineal alt=f(col)")
pacf(res,ylim=c(-1,1),main="residus model lineal alt=f(col)")

summary(ur.df(res,"trend",lags=5))
summary(ur.df(res,"drift",lags=5))
summary(ur.df(res,"none",lags=5))

# Estimación MCE

mce<-lm(diff(alt)~res[1:length(res)-1]+diff(col))
summary(mce)


# EJEMPLO 3 - datos globales de temperatura

# Hansen-Lebedeff global temperature
# 1880 - 1987

hl<-ts(read.table("HL.dat"),start=1880,frequency=1)

par(mfrow=c(2,1))
acf(hl,ylim=c(-1,1))
pacf(hl,ylim=c(-1,1))

summary(ur.df(hl,"trend",lags=0))
summary(ur.kpss(hl, "tau"))

# Folland land and marine global temperature
# 1880 - 1987

folland<-ts(read.table("Folland.dat"),start=1880,frequency=1)

par(mfrow=c(2,1))
acf(folland,ylim=c(-1,1))
pacf(folland,ylim=c(-1,1))

summary(ur.df(folland,"trend",lags=0))
summary(ur.kpss(folland, "tau"))

# EJERCICIO: COMPLETAR ANÁLISIS

# ................


# EJEMPLO 4 - M2=F(PIB,IPC,I3)

m2<-read.table("m2.dat")
m2<-ts(m2,start=1959,frequency=4)

pib<-read.table("pib.dat")
pib<-ts(pib,start=1959,frequency=4)

ipc<-read.table("ipc.dat")
ipc<-ts(ipc,start=1959,frequency=4)

i3<-read.table("i3.dat")
i3<-ts(i3,start=1959,frequency=4)

par(mfrow=c(2,2))
ts.plot(m2,main="m2")
ts.plot(pib,main="pib")
ts.plot(ipc,main="ipc")
ts.plot(i3,main="i3")

hist(m2)
hist(pib)
hist(ipc)
hist(i3)

hist(log(m2))
hist(log(pib))
hist(log(ipc))
hist(log(i3))

lm2=log(m2)
lpib=log(pib)
lipc=log(ipc)

par(mfrow=c(2,1))
acf(lpib,ylim=c(-1,1),main="lpib")
pacf(lpib,ylim=c(-1,1),main="lpib")


# completar análisis integración 

# ................



# análisis cointegración

model<-lm(lm2~lpib+lipc+i3)
summary(model)
res<-ts(residuals(model))

par(mfrow=c(1,1))
ts.plot(res)

par(mfrow=c(2,1))
acf(res,ylim=c(-1,1),main="residus model lineal alt=f(col)")
pacf(res,ylim=c(-1,1),main="residus model lineal alt=f(col)")


# estimación MCE

# ................



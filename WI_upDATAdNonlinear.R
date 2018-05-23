# loading libraries
library(BAYSTAR)
library(tseriesChaos)
library(forecast) # allows forecasting
library(tseries) # adf.tests and ARIMA models
library(stats) # AIC
library(vars) # VAR and Varselect
library(ggplot2) # better plots
library(gridExtra) # more ggplot options
library(ggfortify) # more gpglot options
library(varhandle) # unfactorizes data
library(tsDyn)
library(tseriesChaos)

#### Lyme disease Data ####
WI09 <- c(16, 23, 26, 36, 115, 464, 647, 286, 148, 82, 60, 45)
WI10 <- c(39, 30, 38, 72, 233, 821, 699, 239, 131, 133, 44, 32)
WI11 <- c(39, 21, 38, 67, 152, 589, 832, 346, 156, 88, 63, 34)
WI12 <- c(41, 33, 57, 93, 200, 453, 341, 108, 83, 38, 25, 15)
WI13 <- c(20, 11, 14, 13, 109, 398, 594, 284, 167, 71, 28, 17)
WI14 <- c(16, 9, 15, 27, 85, 254, 320, 115, 78, 45, 11, 9)
WI15 <- c(13, 8, 11, 33, 144, 376, 373, 142, 77, 74, 28, 22)
WI16 <- c(9, 14, 28, 57, 127, 433, 438, 146, 80, 91, 47, 21)

WI <- c(WI09, WI10, WI11, WI12, WI13, WI14, WI15, WI16)

Year <- rep(x = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016),
            each = 12)
Month <- rep(x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
             times = 8)

#### Google Trends Data ####
origin <- read.table("trend_data.txt", sep = "\t", skip = 2, header = TRUE, stringsAsFactors = FALSE)
trend <- unfactor(origin[61:156,])

#### Master Set ####
master <- data.frame(cbind(trend, WI))
names(master) = c("Date", "Trend", "Reports")

# declaring time series data
lyme.ts <- ts(master$Reports,
           start = c(2009, 1),
           frequency = 12)

master.ts <- ts(master[-1],
             start = c(2009, 1),
             frequency = 12)

Searches.ts <- ts(master$Trend,
                start = c(2009, 1),
                frequency = 12)

lyme.a <- window(x = lyme.ts,
                 start = c(2009,1),
                 end = c(2014, 12))

#### Initial Observations ####
# initial plots
Lyme.plot <- autoplot(lyme.ts, ts.colour="blue", ts.linetype = 'dashed') +
  ggtitle("Confirmed Cases of Lyme's Disease in Wisconsin") +
  xlab("Year") +
  ylab("Reported Cases")

Searches.plot <- autoplot(Searches.ts,ts.colour="red", ts.linetype = 'dashed') +
  ggtitle("Google Searches of Lyme's searches") +
  xlab("Year") +
  ylab("Searhces")

grid.arrange(Lyme.plot, Searches.plot, nrow=2)

#Season plots
Lyme.season.plot <- ggseasonplot(lyme.ts, year.labels = TRUE, year.labels.left = TRUE) +
  ylab("Confirmed Cases") + ggtitle("Seasonailty for confirmed Lyme's disease cases in Wisconcin")
Search.season.plot <-ggseasonplot(Searches.ts, year.labels = TRUE, year.labels.left = TRUE)
+ ylab("Google Searches") + ggtitle("Seasonailty for google searches for 'Lyme's Disease'")

grid.arrange(Lyme.season.plot, Search.season.plot, nrow = 2)

#Polar plot
Lyme.Polar <- ggseasonplot(lyme.ts, polar=TRUE) + ylab("Confirmed Cases") + ggtitle("Confirmed Cases in Wisconsin")
Searches.Polar <- ggseasonplot(Searches.ts, polar=TRUE) + ylab("Searhces") + ggtitle("Google Searches")

grid.arrange(Lyme.Polar, Searches.Polar, nrow = 2)

#SubSeries Plots
Lyme.subplot <- ggsubseriesplot(lyme.ts) + ylab("Confirmed Cases") + ggtitle("Confirmed Cases in Wisconsin")
Searches.subplot <- ggsubseriesplot(Searches.ts) + ylab("Google searches") + ggtitle("Google Searches")

grid.arrange(Lyme.subplot, Searches.subplot, nrow = 2)

# Autocorrelation
ggAcf(lyme.ts, lag = 100)
ggPacf(lyme.ts, lag = 100)

# stationality
adf.test(x = lyme.ts)
ndiffs(lyme.ts)

adf.test(x = Searches.ts)
ndiffs(Searches.ts)

#### Univariate Modeling ####

## Simple Methods ##
# plotting fit (add actual values)
fit.mean <- autoplot(meanf(y = lyme.a, h = 24), series = "Mean", PI = FALSE)
fit.naive <- autoplot(naive(y = lyme.a, h = 24), series = "Naive", PI = FALSE)
fit.snaive <- autoplot(snaive(y = lyme.a, h = 24), series = "S-Naive", PI = FALSE)
fit.drift <- autoplot(rwf(y = lyme.a, h = 24, drift = TRUE), series = "Drift", PI = FALSE)

grid.arrange(fit.mean, fit.naive, fit.snaive, fit.drift, ncol = 2)

# plotting residuals (missing values in bottom-left plot)
res.mean <- autoplot(residuals(meanf(lyme.a)), series = "Mean")
res.naive <- autoplot(residuals(naive(lyme.a)), series = "Naive")
res.snaive <- autoplot(residuals(snaive(lyme.a)), series = "S-Naive")
res.drift <- autoplot(residuals(rwf(lyme.a, drift = TRUE)), series = "Drift")

grid.arrange(res.mean, res.naive, res.snaive, res.drift, ncol = 2)

# plotting acf
acf.mean <- ggAcf(residuals(meanf(lyme.a)), series = "Mean")
acf.naive <- ggAcf(residuals(naive(lyme.a)), series = "Naive")
acf.snaive <- ggAcf(residuals(snaive(lyme.a)), series = "S-Naive")
acf.drift <- ggAcf(residuals(rwf(lyme.a, drift = TRUE)), series = "Drift")

grid.arrange(acf.mean, acf.naive, acf.snaive, acf.drift, ncol = 2)

## Seasonal Linear Model ##
# fitting model
fit.lm <- tslm(formula = lyme.a  ~ season)
summary(fit.lm)

# plotting fit (ggplot this)
lyme.prd <- fit.lm$fit
lyme.f <- forecast(fit.lm, h=24, level = 95)

test <- plot(lyme.f,col = "blue", lwd = 2)
lines(lyme.ts, lwd = 2)
lines(lyme.prd, col = "red", lwd = 2)

# residuals/acf
checkresiduals(fit.lm)

## AR(1) ##



## auto.arima ##
# fitting model
fit.aa <- auto.arima(lyme.ts)
res.aa <- residuals(fit.aa)

# plotting fit (ggplot this)
lyme.f2 <- forecast(fit.aa, h = 24, level = 95)
plot(lyme.f2, col = "blue", lwd = 2) # run normal AR models too

# residuals/acf
checkresiduals(fit.aa)

#### Multivariate Modeling ####

## Simple Linear Regression ##

# data plots
autoplot(master.ts[,c("Trend", "Reports")]) +
  ylab("Counts/Popularity") + xlab("Years")

# linear model
fit.lm2 <- tslm(formula = Reports ~ Trend,
               data = master.ts)
summary(fit.lm2)

# linear model plot
master.ts %>% as.data.frame() %>%
  ggplot(aes(x = Trend, y = Reports)) + 
  ylab("Total Reports of Lyme Disease") + 
  xlab("Google Trend Popularity for Lyme Disease") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# forecasting
fit.lm2.prd <- fit.lm2$fit
fit.lm2.f <- forecast(fit.lm2$fit, h=24, level = 95)

checkresiduals(fit.lm2)

## VAR ##
fit.var <- VARselect(y = master.ts, lag.max = 50) # unfinished
fit.var$selection



#NONLINEAR STUFF MY BOII
plot(lyme.ts)

lyme.log <- log10(lyme.ts)
plot(lyme.log)
hist(lyme.log)
par(mfrow=c(2,1), mar = c(2,4,0,0))
acf(lyme.log)
pacf(lyme.log)
ndiffs(lyme.log)

#AR 
lyme.ar <- linear(lyme.log,m=2)
lyme.ar
plot(lyme.ar)
checkresiduals(lyme.ar)
AIC(lyme.ar)

#Setar where mL =1
lyme.setar <- setar(lyme.log, m =2, mL = 2, mH = 2, thDelay = 1)
lyme.setar
plot(lyme.setar)
checkresiduals(lyme.setar)
AIC(lyme.setar)

#Setar where mL = 2
lyme.setar2 <- setar(lyme.log, m =2, mL = 1, mH=2, thDelay = 1)
lyme.setar2
plot(lyme.setar2)
checkresiduals(lyme.setar2)
AIC(lyme.setar2)

lyme.setar3 <- setar(lyme.log, m =2, thDelay = 1, th = 1.20)
lyme.setar3
plot(lyme.setar3)
checkresiduals(lyme.setar3)
AIC(lyme.setar3)


mod<-list()
mod[["linear"]] <- linear(lyme.log, m =3)
mod[["setar"]] <- setar(lyme.log, m =3, thDelay = 1)
mod[["lstar"]] <- lstar(lyme.log, m =3, thDelay = 1)
mod[["nneTs"]] <- nnetTs(lyme.log, m=3, size= 3)
mod[["aar"]] <- aar(lyme.log, m =3)

sapply(mod, AIC)
sapply(mod,MAPE)

set.seed(10)
mod.test <- list()
lyme.log.train <- window(lyme.log, end = 2014)
lyme.log.test <- window(lyme.log, start = 2016)

mod.test[["linear"]] <- linear(lyme.log.train, m =3)
mod.test[["setar"]] <- setar(lyme.log.train, m =3, thDelay = 1)
mod.test[["lstar"]] <- lstar(lyme.log.train, m =3, thDelay = 1, trace = FALSE,
                             control = list(maxit=1e5) )
mod.test[["nneTs"]] <- nnetTs(lyme.log.train, m=3, size= 3, control = list(maxit=1e5))
mod.test[["aar"]] <- aar(lyme.log.train, m =3)

frc.test<-lapply(mod.test, predict, n.ahead=10)

plot(lyme.log.test, ylim = range(lyme.log))
for(i in 1:length(frc.test)) lines(frc.test[[i]], lty = i+1, col = i +1)
legend(2015, 2,5, lty = 1:(length(frc.test)+1) col =1:length(frc.test)+1),
legend = c("observed", names(frc.test)))


mods<-list()
grid <- selectSETAR(lyme.log, m  = 22, thDelay = 1, criterion = "AIC")
print(grid)


lyme.setar4 <- setar(lyme.log, mL =22, mH =15, thDelay = 1, th = 1.579784)
lyme.setar4
plot(lyme.setar4)
checkresiduals(lyme.setar4)
AIC(lyme.setar4)

recurr(lyme.log, m = 3, d =1, levels = c(0,0.2,1))
lag.plot(lyme.log, lags = 3, layout=c(1,3))

autopairs(lyme.log, lag =1, type = "regression")




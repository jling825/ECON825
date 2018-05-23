# loading libraries
library(forecast) # allows forecasting
library(tseries) # adf.tests and ARIMA models
library(stats) # AIC
library(vars) # VAR and Varselect
library(ggplot2) # better plots
library(gridExtra) # more ggplot options
library(ggfortify) # more gpglot options
library(varhandle) # unfactorizes data

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

# log Reports
master$Reports = log(master$Reports)

# log Google Trends
master$Trend = log(master$Trend)

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
                 end = c(2014, 12))

master.a <- window(x = master.ts,
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
Search.season.plot <-ggseasonplot(Searches.ts, year.labels = TRUE, year.labels.left = TRUE) +
  ylab("Google Searches") + ggtitle("Seasonailty for google searches for 'Lyme's Disease'")

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
# plotting fit
fit.mean <- autoplot(meanf(y = lyme.a, h = 24), seires = "Mean", PI = FALSE) +
  ylab("Confirmed Cases") + ggtitle("Mean Fit Model")
fit.naive <- autoplot(naive(y = lyme.a, h = 24), series = "Naive", PI = FALSE) +
  ylab("Confirmed Cases") + ggtitle("Naive Fit Model")
fit.snaive <- autoplot(snaive(y = lyme.a, h = 24), series = "S-Naive", PI = FALSE) +
  ylab("Confirmed Cases") + ggtitle("Seasonal-Naive Fit Model")
fit.drift <- autoplot(rwf(y = lyme.a, h = 24, drift = TRUE), series = "Drift", PI = FALSE) +
  ylab("Confirmed Cases") + ggtitle("Random Walk Fit Model")

grid.arrange(fit.mean, fit.naive, fit.snaive, fit.drift, ncol = 2)

checkresiduals(meanf(y = lyme.a, h = 24))
checkresiduals(naive(y = lyme.a, h = 24))
checkresiduals(snaive(y = lyme.a, h = 24))
checkresiduals(rwf(y = lyme.a, h = 24, drift = TRUE))

## Seasonal Linear Model ##
# fitting model
fit.lm <- tslm(formula = lyme.a  ~ season)
summary(fit.lm)

# plotting fit
lyme.prd <- fit.lm$fit
lyme.f <- forecast(fit.lm, h=24, level = 95)

plot(lyme.f,col = "blue", lwd = 2,
     ylab = "Confirmed Cases", xlab = "Year",
     main = "Seasonal Linear Model")
lines(lyme.ts, lwd = 2)
lines(lyme.prd, col = "red", lwd = 2)

# residuals/acf
checkresiduals(fit.lm)

## AR(1) ##
fit.ar1 <- arima(lyme.a, order = c(1,0,0))
summary(fit.ar1)

# plotting fit
ar1.prd <- fitted(fit.ar1)
ar1.f <- forecast(fit.ar1, h = 24, lwd = 2)

plot(ar1.f, col = "blue", lwd = 2,
     ylab = "Confirmed Cases", xlab = "Year",
     main = "Seasonal Linear Model")
lines(lyme.ts, lwd = 2)
lines(ar1.prd, col = "red", lwd = 2)

# residuals/acf
checkresiduals(fit.ar1)

## AR(2) ##
fit.ar2 <- arima(lyme.a, order = c(2,0,0))
summary(fit.ar2)

# plotting fit
ar2.prd <- fitted(fit.ar2)
ar2.f <- forecast(fit.ar2, h = 24, lwd = 2)

plot(ar2.f, col = "blue", lwd = 2,
     ylab = "Confirmed Cases", xlab = "Year",
     main = "Seasonal Linear Model")
lines(lyme.ts, lwd = 2)
lines(ar2.prd, col = "red", lwd = 2)

# residuals/acf
checkresiduals(fit.ar2)

## auto.arima ##
# fitting model
fit.aa <- auto.arima(y = lyme.a, stationary = TRUE)
summary(fit.aa)

# plotting fit
aa.prd <- fit.aa$fit
aa.f <- forecast(fit.aa, h = 24, level = 95)

plot(aa.f, col = "blue", lwd = 2,
     ylab = "Confirmed Cases", xlab = "Year",
     main = "Seasonal Linear Model")
lines(lyme.ts, lwd = 2)
lines(aa.prd, col = "red", lwd = 2)

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

# residuals/acf
checkresiduals(fit.lm2)

## VAR ##
# finding optimal lags
VARselect(y = master.a, lag.max = 10, type = "const")[["selection"]]

var1 <- VAR(y = master.a, p = 3)
serial.test(x = var1, lags.pt = 10, type = "PT.asymptotic")

var2 <- VAR(y = master.a, p = 6)
serial.test(x = var2, lags.pt = 10, type = "PT.asymptotic")

var3 <- VAR(y = master.a, p = 8)
serial.test(x = var3, lags.pt = 10, type = "PT.asymptotic")

# plotting forecast
autoplot(forecast(var1, h = 24)) + xlab("Year")
summary(var1)

# plotting fit
plot(resid(var1))

plot(irf(var1, n.ahead = 24))

#### AIC ####
AIC(fit.lm)
AIC(fit.ar1)
AIC(fit.ar2)
AIC(fit.aa)

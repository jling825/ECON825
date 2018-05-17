# loading libraries
library(forecast)
library(tseries)
library(stats)
library(vars)
library(ggplot2)
library(gridExtra)
library(ggfortify)
library(varhandle)

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
Lyme.season.plot <- ggseasonplot(lyme.ts, year.labels = TRUE, year.labels.left = TRUE)+ylab("Confirmed Cases") + ggtitle("Seasonailty for confirmed Lyme's disease cases in Wisconcin")
Search.season.plot <-ggseasonplot(Searches.ts, year.labels = TRUE, year.labels.left = TRUE)+ylab("Google Searches") + ggtitle("Seasonailty for google searches for 'Lyme's Disease'")

grid.arrange(Lyme.season.plot, Search.season.plot, nrow= 2)

#Polar plot
Lyme.Polar <- ggseasonplot(lyme.ts, polar=TRUE) +
  ylab("Confirmed Cases") +
  ggtitle("Confirmed Cases in Wisconsin")
Searches.Polar <- ggseasonplot(Searches.ts, polar=TRUE) +
  ylab("Searhces") +
  ggtitle("Google Searches")

grid.arrange(Lyme.Polar, Searches.Polar, nrow= 2)

#SubSeries Plots
Lyme.subplot <- ggsubseriesplot(lyme.ts) +
  ylab("Confirmed Cases") +
  ggtitle("Confirmed Cases in Wisconsin")

Searches.subplot <- ggsubseriesplot(Searches.ts) + 
  ylab("Google searches") +
  ggtitle("Google Searches")

grid.arrange(Lyme.subplot, Searches.subplot, nrow= 2)

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

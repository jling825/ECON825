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

#### Scatterplot ####
qplot(lyme.ts, Searches.ts, xlab("Google Trends") + xlab("Reports"))

#### Seasonality ####
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

# stationality
adf.test(x = lyme.ts)
ndiffs(lyme.ts)

adf.test(x = Searches.ts)
ndiffs(Searches.ts)

#### Autocorrelation ####
ggAcf(lyme.ts, lag = 100)
ggPacf(lyme.ts, lag = 100)

#### Simple Forecasting Methods ####
# plotting forecasts
mean.method <- autoplot(meanf(y = lyme.a, h = 24), series = "Mean", PI = FALSE)
naive.method <- autoplot(naive(y = lyme.a, h = 24), series = "Naive", PI = FALSE)
snaive.method <- autoplot(snaive(y = lyme.a, h = 24), series = "S-Naive", PI = FALSE)
drift.method <- autoplot(rwf(y = lyme.a, h = 24, drift = TRUE), series = "Drift", PI = FALSE)

grid.arrange(mean.method, naive.method, snaive.method, drift.method, ncol = 2)

# plotting residuals
mean.res <- autoplot(residuals(meanf(lyme.a)))
naive.res <- autoplot(residuals(naive(lyme.a)))
snaive.res <- autoplot(residuals(snaive(lyme.a)))
drift.res <- autoplot(residuals(rwf(lyme.a, drift = TRUE)))

grid.arrange(mean.res, naive.res, snaive.res, drift.res, ncol = 2)

# plotting acf
mean.acf <- ggAcf(residuals(meanf(lyme.a)))
naive.acf <- ggAcf(residuals(naive(lyme.a)))
snaive.acf <- ggAcf(residuals(snaive(lyme.a)))
drift.acf <- ggAcf(residuals(rwf(lyme.a, drift = TRUE)))

grid.arrange(mean.acf, naive.acf, snaive.acf, drift.acf, ncol = 2)

##Seasonal LM ##
# linear regression
lyme.lm <- tslm(formula = lyme.a  ~ season)
summary(lyme.lm)

# fitted values
lyme.prd <- lyme.lm$fit

# forecast
lyme.f <- forecast(lyme.lm, h=24, level = 95)

# plotting data (change to gpg5elot)
test <- plot(lyme.f,col = "blue", lwd = 2)
lines(lyme.ts, lwd = 2)
lines(lyme.prd, col = "red", lwd = 2)

## auto.arima ##
fit <- auto.arima(lyme.ts)
res <- residuals(fit)

#### Modeling ####
lyme.f2 <- forecast(fit, h = 24, level = 95)
plot(lyme.f2, col = "blue", lwd = 2) # run normal AR models too

plot(res, col = "red", lwd = 2)
ggAcf(res)
ggPacf(res)

# VAR
VARselect(y = master.ts, lag.max = 50) # unfinished

#### Multivariate Modeling ####

## Simple Linear Regression ##

# data plots
autoplot(master.ts[,c("Trend", "Reports")]) +
  ylab("Counts/Popularity") + xlab("Years")

# linear model
fit.lm <- tslm(formula = Reports ~ Trend,
               data = master.ts)
summary(fit.lm)

# linear model plot
master.ts %>% as.data.frame() %>%
  ggplot(aes(x = Trend, y = Reports)) + 
  ylab("Total Reports of Lyme Disease") + 
  xlab("Google Trend Popularity for Lyme Disease") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)



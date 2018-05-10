# loading libraries
install.packages("vars")
install.packages("gridExtra")
install.packages("ggfortify")

library(forecast)
library(stats)
library(stargazer)
library(vars)
library(ggplot2)
library(gridExtra)
library(ggfortify)

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

WI2 <- WI / 8.32

Year <- rep(x = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016),
            each = 12)
Month <- rep(x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
             times = 8)

origin <- read.csv("trend_data.csv", skip = 2, header = TRUE)
trend <- origin[61:156,]

master <- data.frame(cbind(trend, WI, WI2))
names(master) = c("Date", "Trend", "Reports", "W.Reports")


# declaring time series data
lyme.ts <- ts(master$Reports,
           start = c(2009, 1),
           frequency = 12)

master.ts <- ts(master[-2],
             start = c(2009, 1),
             frequency = 12)

Searches.ts <- ts(master$Trend,
                start = c(2009, 1),
                frequency = 12)

# initial plots

Lyme.plot <- autoplot(lyme.ts, ts.colour="blue", ts.linetype = 'dashed') +
ggtitle("Confirmed Cases of Lyme's Disease in Wisconsin") +
  xlab("Year") +
  ylab("Reported Cases")
Lyme.plot
Searches.plot <- autoplot(Searches.ts,ts.colour="red", ts.linetype = 'dashed') +
  ggtitle("Google Searches of Lyme's searches") +
  xlab("Year") +
  ylab("Searhces")
grid.arrange(Lyme.plot, Searches.plot, nrow=2)


#### Trends ####
# training data
lyme.a<-window(lyme, start = c(2009,1), end = c(2014,12))

# linear regression
lyme.lm <- tslm(formula = lyme.a ~ trend)
print(summary(lyme.lm))

lyme.f1 <- forecast(lyme.lm, h = 24, level = 95)
lyme.prd1 <- lyme.lm$fit

plot(lyme.f1)
lines(lyme.prd1)
lines(lyme)

# quadratic
lyme.qt <- tslm(lyme.a~trend+I(trend*trend))
print(summary(lyme.qt))

lyme.f2<- forecast(lyme.qt, h = 24, level = 95)
lyme.prd2<-lyme.qt$fit

plot(lyme.f2)
lines(lyme.prd2)
lines(lyme)

# log-linear
loglyme <- log(lyme)
loglyme.a<-window(loglyme, start = c(2009,1), end = c(2014,12))

lyme.log <- tslm(loglyme.a~trend)
print(summary(lyme.log))

lyme.f3 <-forecast(lyme.log, h = 24,level = 95)

lyme.prd3 <- lyme.log$fit
plot(lyme.f3)
lines(loglyme)
lines(lyme.prd3)

# AIC and BIC scores

data.frame(AIC = c(AIC(lyme.lm),AIC(lyme.qt), AIC(lyme.log)), 
           BIC = c(BIC(lyme.lm), BIC(lyme.qt), BIC(lyme.log)), row.names = 
             c("Linear", "Quadratic", "Log"))

#### Seasonality ####
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

# initial plots
gghistogram(master$Reports)
barplot(master$Reports)

# linear regression
lyme.lm <- tslm(formula = lyme.a  ~ season)
summary(lyme.lm)

# fitted values
lyme.prd <- lyme.lm$fit

# forecast
lyme.f <- forecast(lyme.lm, h=24, level = 95)

# plotting data
plot(lyme.f,col = "blue", lwd = 2)
lines(lyme, lwd = 2)
lines(lyme.prd, col = "red", lwd = 2)


#### Modeling ####

# stationality
adf.test(x = lyme)
ndiffs(lyme)

# acf and pacf
ggAcf(lyme, lag = 100)
ggPacf(lyme, lag = 100)

# fitting model
fit <- auto.arima(lyme)
res <- residuals(fit)

lyme.f2 <- forecast(fit, h = 24, level = 95)
plot(lyme.f2, col = "blue", lwd = "2")


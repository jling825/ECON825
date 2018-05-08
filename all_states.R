# loading libraries
library(forecast)
library(stats)
library(stargazer)

# reading data
lyme <- read.csv(file = "Lyme_Disease.csv", skip = 2, header = TRUE)
attach(lyme)

#### CREATING VARIABLES ####

# US total
us.lyme <- ts(data = lyme$Lyme.disease...United.States.,
              start = c(2004, 1),
              frequency = 12)

us.tick <- ts(data = lyme$Tick...United.States.,
              start = c(2004, 1),
              frequency = 12)

# Connecticut total
ct.lyme <- ts(data = lyme$Lyme.disease...Connecticut.,
              start = c(2004, 1),
              frequency = 12)

ct.tick <- ts(data = lyme$Tick...Connecticut.,
              start = c(2004, 1),
              frequency = 12)

# Delaware total
de.lyme <- ts(data = lyme$Lyme.disease...Delaware.,
              start = c(2004, 1),
              frequency = 12)

de.tick <- ts(data = lyme$Tick...Delaware.,
              start = c(2004, 1),
              frequency = 12)

# Maine total
me.lyme <- ts(data = lyme$Lyme.disease...Maine.,
              start = c(2004, 1),
              frequency = 12)

me.tick <- ts(data = lyme$Tick...Maine.,
              start = c(2004, 1),
              frequency = 12)

# Maryland total
md.lyme <- ts(data = lyme$Lyme.disease...Maryland.,
              start = c(2004, 1),
              frequency = 12)

md.tick <- ts(data = lyme$Tick...Maryland.,
              start = c(2004, 1),
              frequency = 12)

# Massachusetts total
ma.lyme <- ts(data = lyme$Lyme.disease...Massachusetts.,
              start = c(2004, 1),
              frequency = 12)

ma.tick <- ts(data = lyme$Tick...Massachusetts.,
              start = c(2004, 1),
              frequency = 12)

# Minnesota total
mn.lyme <- ts(data = lyme$Lyme.disease...Minnesota.,
              start = c(2004, 1),
              frequency = 12)

mn.tick <- ts(data = lyme$Tick...Minnesota.,
              start = c(2004, 1),
              frequency = 12)

# New Hampshire total
nh.lyme <- ts(data = lyme$Lyme.disease...New.Hampshire.,
              start = c(2004, 1),
              frequency = 12)

nh.tick <- ts(data = lyme$Tick...New.Hampshire.,
              start = c(2004, 1),
              frequency = 12)

# New Jersey total
nj.lyme <- ts(data = lyme$Lyme.disease...New.Jersey.,
              start = c(2004, 1),
              frequency = 12)

nj.tick <- ts(data = lyme$Tick...New.Jersey.,
              start = c(2004, 1),
              frequency = 12)

# New York total
ny.lyme <- ts(data = lyme$Lyme.disease...New.York.,
              start = c(2004, 1),
              frequency = 12)

ny.tick <- ts(data = lyme$Tick...New.York.,
              start = c(2004, 1),
              frequency = 12)

# Pennsylvania total
pa.lyme <- ts(data = lyme$Lyme.disease...Pennsylvania.,
              start = c(2004, 1),
              frequency = 12)

pa.tick <- ts(data = lyme$Tick...Pennsylvania.,
              start = c(2004, 1),
              frequency = 12)

# Rhode Island total
ri.lyme <- ts(data = lyme$Lyme.disease...Rhode.Island.,
              start = c(2004, 1),
              frequency = 12)

ri.tick <- ts(data = lyme$Tick...Rhode.Island.,
              start = c(2004, 1),
              frequency = 12)

# Vermont total
vt.lyme <- ts(data = lyme$Lyme.disease...Vermont.,
              start = c(2004, 1),
              frequency = 12)

vt.tick <- ts(data = lyme$Tick...Vermont.,
              start = c(2004, 1),
              frequency = 12)

# Virginia total
va.lyme <- ts(data = lyme$Lyme.disease...Virginia.,
              start = c(2004, 1),
              frequency = 12)

va.tick <- ts(data = lyme$Tick...Virginia.,
              start = c(2004, 1),
              frequency = 12)

# Wisconsin total
wi.lyme <- ts(data = lyme$Lyme.disease...Wisconsin.,
              start = c(2004, 1),
              frequency = 12)

wi.tick <- ts(data = lyme$Tick...Wisconsin.,
              start = c(2004, 1),
              frequency = 12)

#### PLOTS ####
#par(mfrow = c(1,1))
par(mfrow = c(3,5))

# United States
plot(x = us.lyme,
     col = "red",
     lty = 1,
     lwd = 2,
     ylim = c(0, 100),
     xlab = "Time",
     ylab = "Search Popularity",
     main = "United States")

lines(x = us.tick,
      col = "blue",
      lty = 2,
      lwd = 2)

grid(col = "light grey",
     lwd = 1)

# Connecticut
plot(x = ct.lyme,
     col = "red",
     lty = 1,
     lwd = 2,
     ylim = c(0, 100),
     xlab = "Time",
     ylab = "Search Popularity",
     main = "Connecticut")

lines(x = ct.tick,
      col = "blue",
      lty = 2,
      lwd = 2)

grid(col = "light grey",
     lwd = 1)

# Delaware
plot(x = de.lyme,
     col = "red",
     lty = 1,
     lwd = 2,
     ylim = c(0, 100),
     xlab = "Time",
     ylab = "Search Popularity",
     main = "Delaware")

lines(x = de.tick,
      col = "blue",
      lty = 2,
      lwd = 2)

grid(col = "light grey",
     lwd = 1)

# Maine
plot(x = me.lyme,
     col = "red",
     lty = 1,
     lwd = 2,
     ylim = c(0, 100),
     xlab = "Time",
     ylab = "Search Popularity",
     main = "Maine")

lines(x = me.tick,
      col = "blue",
      lty = 2,
      lwd = 2)

grid(col = "light grey",
     lwd = 1)

# Maryland
plot(x = md.lyme,
     col = "red",
     lty = 1,
     lwd = 2,
     ylim = c(0, 100),
     xlab = "Time",
     ylab = "Search Popularity",
     main = "Maryland")

lines(x = md.tick,
      col = "blue",
      lty = 2,
      lwd = 2)

grid(col = "light grey",
     lwd = 1)

# Massachusetts
plot(x = ma.lyme,
     col = "red",
     lty = 1,
     lwd = 2,
     ylim = c(0, 100),
     xlab = "Time",
     ylab = "Search Popularity",
     main = "Massachusetts")

lines(x = ma.tick,
      col = "blue",
      lty = 2,
      lwd = 2)

grid(col = "light grey",
     lwd = 1)

# Minnesota
plot(x = mn.lyme,
     col = "red",
     lty = 1,
     lwd = 2,
     ylim = c(0, 100),
     xlab = "Time",
     ylab = "Search Popularity",
     main = "Minnesota")

lines(x = mn.tick,
      col = "blue",
      lty = 2,
      lwd = 2)

grid(col = "light grey",
     lwd = 1)

# New Hapshire
plot(x = nh.lyme,
     col = "red",
     lty = 1,
     lwd = 2,
     ylim = c(0, 100),
     xlab = "Time",
     ylab = "Search Popularity",
     main = "New Hampshire")

lines(x = nh.tick,
      col = "blue",
      lty = 2,
      lwd = 2)

grid(col = "light grey",
     lwd = 1)

# New Jersey
plot(x = nj.lyme,
     col = "red",
     lty = 1,
     lwd = 2,
     ylim = c(0, 100),
     xlab = "Time",
     ylab = "Search Popularity",
     main = "New Jersey")

lines(x = nj.tick,
      col = "blue",
      lty = 2,
      lwd = 2)

grid(col = "light grey",
     lwd = 1)

# New York
plot(x = ny.lyme,
     col = "red",
     lty = 1,
     lwd = 2,
     ylim = c(0, 100),
     xlab = "Time",
     ylab = "Search Popularity",
     main = "New York")

lines(x = ny.tick,
      col = "blue",
      lty = 2,
      lwd = 2)

grid(col = "light grey",
     lwd = 1)

# Pennsylvania
plot(x = pa.lyme,
     col = "red",
     lty = 1,
     lwd = 2,
     ylim = c(0, 100),
     xlab = "Time",
     ylab = "Search Popularity",
     main = "Pensylvania")

lines(x = pa.tick,
      col = "blue",
      lty = 2,
      lwd = 2)

grid(col = "light grey",
     lwd = 1)

# Rhode Island
plot(x = ri.lyme,
     col = "red",
     lty = 1,
     lwd = 2,
     ylim = c(0, 100),
     xlab = "Time",
     ylab = "Search Popularity",
     main = "Rhode Island")

lines(x = ri.tick,
      col = "blue",
      lty = 2,
      lwd = 2)

grid(col = "light grey",
     lwd = 1)

# Vermont
plot(x = vt.lyme,
     col = "red",
     lty = 1,
     lwd = 2,
     ylim = c(0, 100),
     xlab = "Time",
     ylab = "Search Popularity",
     main = "Vermont")

lines(x = vt.tick,
      col = "blue",
      lty = 2,
      lwd = 2)

grid(col = "light grey",
     lwd = 1)

# Virginia
plot(x = va.lyme,
     col = "red",
     lty = 1,
     lwd = 2,
     ylim = c(0, 100),
     xlab = "Time",
     ylab = "Search Popularity",
     main = "Virginia")

lines(x = va.tick,
      col = "blue",
      lty = 2,
      lwd = 2)

grid(col = "light grey",
     lwd = 1)

# Wisconsin
plot(x = wi.lyme,
     col = "red",
     lty = 1,
     lwd = 2,
     ylim = c(0, 100),
     xlab = "Time",
     ylab = "Search Popularity",
     main = "Wisconsin")

lines(x = wi.tick,
      col = "blue",
      lty = 2,
      lwd = 2)

grid(col = "light grey",
     lwd = 1)

#### US TIME SERIES ####
# checking for trends and seasonality
us.lyme.train <- window(x = us.lyme,
                       start = c(2004, 1),
                       end = c(2014, 1))

us.lyme.lm <- tslm(us.lyme.train~trend + season)
summary(us.lyme.lm)

us.lyme.test <- forecast(object = us.lyme.lm,
                         h = 48,
                         level = 95)

us.lyme.pred <- us.lyme.lm$fit

# plotting
plot(us.lyme.test, col = "blue", lwd = 2)
lines(us.lyme.pred, col = "red", lwd = 2)
lines(us.lyme, col = "green", lwd = 2)

# loading libraries
library(dplyr)
library(lubridate)

# reading in sf crime data
sfcrime <- read.csv("sfcrime.csv")
names(sfcrime)

# subsetting data
sfcrime.small <- sfcrime[,c("Date", "PdDistrict")]
attach(sfcrime.small)

# reformatting dates
sfcrime.small$Date <- as.Date(Date, format = "%m/%d/%Y")

# creating district-related subsets
summary(PdDistrict)

bayview <- sfcrime.small[which(sfcrime.small$PdDistrict == "BAYVIEW"),]
central <- sfcrime.small[which(sfcrime.small$PdDistrict == "CENTRAL"),]
ingleside <- sfcrime.small[which(sfcrime.small$PdDistrict == "INGLESIDE"),]
mission <- sfcrime.small[which(sfcrime.small$PdDistrict == "MISSION"),]
northern <- sfcrime.small[which(sfcrime.small$PdDistrict == "NORTHERN"),]
park <- sfcrime.small[which(sfcrime.small$PdDistrict == "PARK"),]
richmond <- sfcrime.small[which(sfcrime.small$PdDistrict == "RICHMOND"),]
southern <- sfcrime.small[which(sfcrime.small$PdDistrict == "SOUTHERN"),]
taraval <- sfcrime.small[which(sfcrime.small$PdDistrict == "TARAVAL"),]
tenderloin <- sfcrime.small[which(sfcrime.small$PdDistrict == "TENDERLOIN"),]

nrow(sfcrime.small) # one district not recorded
nrow(bayview) + nrow(central) + nrow(ingleside) + nrow(mission) + nrow(northern) + nrow(park) + nrow(richmond) + nrow(southern) + nrow(taraval) + nrow(tenderloin)
test <- sfcrime.small[which(sfcrime.small$PdDistrict != "BAYVIEW" & sfcrime.small$PdDistrict != "CENTRAL" & sfcrime.small$PdDistrict != "INGLESIDE" & sfcrime.small$PdDistrict != "MISSION" & sfcrime.small$PdDistrict != "NORTHERN" & sfcrime.small$PdDistrict != "PARK" & sfcrime.small$PdDistrict != "RICHMOND" & sfcrime.small$PdDistrict != "SOUTHERN" & sfcrime.small$PdDistrict != "TARAVAL" & sfcrime.small$PdDistrict != "TENDERLOIN") ,]

# aggreagating incidents
bayview2 <- bayview %>% group_by(yr = year(Date), mon = month(Date)) %>% summarise(Bayview = n())
central2 <- central %>% group_by(yr = year(Date), mon = month(Date)) %>% summarise(Central = n())
ingleside2 <- ingleside %>% group_by(yr = year(Date), mon = month(Date)) %>% summarise(Ingleside = n())
mission2 <- mission %>% group_by(yr = year(Date), mon = month(Date)) %>% summarise(Mission = n())
northern2 <- northern %>% group_by(yr = year(Date), mon = month(Date)) %>% summarise(Northern = n())
park2 <- park %>% group_by(yr = year(Date), mon = month(Date)) %>% summarise(Park = n())
richmond2 <- richmond %>% group_by(yr = year(Date), mon = month(Date)) %>% summarise(Richmond = n())
southern2 <- southern %>% group_by(yr = year(Date), mon = month(Date)) %>% summarise(Southern = n())
taraval2 <- taraval %>% group_by(yr = year(Date), mon = month(Date)) %>% summarise(Taraval = n())
tenderloin2 <- tenderloin %>% group_by(yr = year(Date), mon = month(Date)) %>% summarise(Tenderloin = n())

master <- data.frame(bayview2, central2$Central, ingleside2$Ingleside, mission2$Mission, northern2$Northern, park2$Park, richmond2$Richmond, southern2$Southern, taraval2$Taraval, tenderloin2$Tenderloin)
master <- master[-(181:184),]
names(master) <- c("Year", "Month", "Bayview", "Central", "Ingleside", "Mission", "Northern", "Park", "Richmond", "Southern", "Taraval", "Tenderloin")
str(master)
attach(master)

bayview.ts <- ts(Bayview, frequency = 12, start = c(2003, 1))
central.ts <- ts(Central, frequency = 12, start = c(2003, 1))
ingleside.ts <- ts(Ingleside, frequency = 12, start = c(2003, 1))
mission.ts <- ts(Mission, frequency = 12, start = c(2003, 1))
northern.ts <- ts(Northern, frequency = 12, start = c(2003, 1))
park.ts <- ts(Park, frequency = 12, start = c(2003, 1))
richmond.ts <- ts(Richmond, frequency = 12, start = c(2003, 1))
southern.ts <- ts(Southern, frequency = 12, start = c(2003, 1))
taraval.ts <- ts(Taraval, frequency = 12, start = c(2003, 1))
tenderloin.ts <- ts(Tenderloin, frequency = 12, start = c(2003, 1))

# plotting data
plot(bayview.ts,
     col = "black",
     lwd = 2,
     ylim = c(0, 3000))
lines(central.ts,
      col = "red",
      lwd = 2)
lines(ingleside.ts,
      col = "purple",
      lwd = 2)
lines(mission.ts,
      col = "blue",
      lwd = 2)
lines(northern.ts,
      col = "green",
      lwd = 2)
lines(park.ts,
      col = "yellow",
      lwd = 2)
lines(richmond.ts,
      col = "orange",
      lwd = 2)
lines(southern.ts,
      col = "dark green",
      lwd = 2)
lines(taraval.ts,
      col = "gold",
      lwd = 2)
lines(tenderloin.ts,
      col = "brown",
      lwd = 2)
grid(col = "darkgrey") # adding grid

rect(2007+12/12,0, 2009+6/12, 3000, col=rgb(160,160,160, maxColorValue=255, alpha=100),
     border=F) # 2008 recession

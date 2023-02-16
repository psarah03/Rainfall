rm(list = ls(all = TRUE))
data.loc <- ("C:/Users/pazls/Desktop/Learning R/Rainfall/Data")
setwd(data.loc)
data.raw <- read.csv("EllicottCity.csv", stringsAsFactors = TRUE)
dim(data.raw)
library("ggplot2")

converteddates <- as.character(data.raw$DATE)
converteddates <- as.Date(data.raw$DATE,"%m/%d/%Y")
data.raw$DATE <- converteddates

png(file="full_EllicottCity_station.png")
ggplot(data.raw, aes(x=converteddates, y=STATION)) + geom_point() + labs(title = "Stations", x = "Dates", y = "Stations")
dev.off()

stations <- unique(data.raw$STATION)
fillstations <- rep(NA, length(stations))
stationstable <- data.frame(STATIONS = stations, DAYS = fillstations)
station.names <- c("US1MDHW0013", "US1MDHW0025", "US1MDHW0015", "USC00181862", "US1MDHW0020")
for(item in 1:length(stations)) {
  stationstable$DAYS[item] <- length(subset(data.raw$DATE, data.raw$STATION == station.names[item]))
}

for(s in 1:length(station.names)) {
  stationstable$Days[s] <- length(subset(data.raw$DATE, data.raw$STATION == station.names[s]))
  current_stationPRCP <- subset(data.raw$PRCP, data.raw$STATION == station.names[s])
  current_stationTemp <- subset(data.raw$TMAX, data.raw$STATION == station.names[s]) # put into Tavg
  stationstable$PRCPvar[s] <- 0
  stationstable$Tempvar[s] <- 0
  for(x in 1:length(current_stationPRCP)) {
    if(is.na(current_stationPRCP[x])) {
      stationstable$PRCPvar[s] <- stationstable$PRCPvar[s] + 1
    }
    if(is.na(current_stationTemp[x])) {
      stationstable$Tempvar[s] <- stationstable$Tempvar[s] + 1
    }
  }
}

# combine the data
head(subset(data.raw$DATE, data.raw$STATION == "USC00181862"), 1) # starts 1970-01-01
tail(subset(data.raw$DATE, data.raw$STATION == "USC00181862"), 1) # ends 2004-08-31

head(subset(data.raw$DATE, data.raw$STATION == "US1MDHW0025"), 1) # starts 2011-03-15
tail(subset(data.raw$DATE, data.raw$STATION == "US1MDHW0025"), 1) # ends 2013-08-27

head(subset(data.raw$DATE, data.raw$STATION == "US1MDHW0015"), 1) # starts 2006-11-16
tail(subset(data.raw$DATE, data.raw$STATION == "US1MDHW0015"), 1) # ends 2011-06-07

head(subset(data.raw$DATE, data.raw$STATION == "US1MDHW0015"), 1) # starts 2006-11-16
tail(subset(data.raw$DATE, data.raw$STATION == "US1MDHW0015"), 1) # ends 2011-06-07

head(subset(data.raw$DATE, data.raw$STATION == "US1MDHW0020"), 1) # starts 2008-08-19
tail(subset(data.raw$DATE, data.raw$STATION == "US1MDHW0020"), 1) # ends 2010-12-18

head(subset(data.raw$DATE, data.raw$STATION == "US1MDHW0013"), 1) # starts 2006-04-03
tail(subset(data.raw$DATE, data.raw$STATION == "US1MDHW0013"), 1) # ends 2022-10-27

part1 <- data.frame(date = subset(data.raw$DATE, data.raw$STATION == "USC00181862"), tmin = subset(data.raw$TMIN, data.raw$STATION == "USC00181862"), tmax = subset(data.raw$TMAX, data.raw$STATION == "USC00181862"), prcp = subset(data.raw$PRCP, data.raw$STATION == "USC00181862"))
# part2 <- data.frame(date = subset(data.raw$DATE, data.raw$STATION == "US1MDHW0013"), tmin = subset(data.raw$TMIN, data.raw$STATION == "US1MDHW0013"), tmax = subset(data.raw$TMAX, data.raw$STATION == "US1MDHW0013"), prcp = subset(data.raw$PRCP, data.raw$STATION == "US1MDHW0013"))
completed <- part1
# completed <- rbind.data.frame(part1, part2)
completed <- completed[order(completed$date), ]
completed$year <- format(completed$date, "%Y")
completed$tavg <- (completed$tmax + completed$tmin) / 2
fill <- rep(NA, length(unique(completed$year)))
NAtable <- data.frame(year = unique(completed$year), NA_tavg = fill, NA_prcp = fill, days = fill)
for(c in 1:(length(unique(completed$year)))) {
  currentyear <- unique(completed$year)[c]
  yeardata <- subset(completed, completed$year == currentyear)
  l <- is.na(yeardata$tavg)
  m <- is.na(yeardata$prcp)
  tavg_NA <- sum(l, na.rm = TRUE)
  prcp_NA <- sum(m, na.rm = TRUE)
  NAtable$NA_tavg[c] <- tavg_NA
  NAtable$NA_prcp[c] <- prcp_NA
  d <- length(yeardata$year)
  NAtable$days[c] <- d
}

# make two versions of the program

annual.summary <- data.frame(year = unique(completed$year), tavg = fill, totalprcp = fill, NA_tavg = NAtable$NA_tavg, NA_prcp = NAtable$NA_prcp, days = NAtable$days)
for(m in 1:(length(unique(completed$year)))) {
  cyear <- unique(completed$year)[m]
  ydata <- subset(completed, completed$year == cyear)
  annual.summary$totalprcp[m] <- sum(ydata$prcp, na.rm = TRUE)
  annual.summary$tavg[m] <- mean(ydata$tavg, na.rm = TRUE)
}


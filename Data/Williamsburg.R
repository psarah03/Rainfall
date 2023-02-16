rm(list = ls(all = TRUE))
data.loc <- ("C:/Users/pazls/Desktop/Learning R/Rainfall/Data")
setwd(data.loc)
data.raw1 <- read.csv("Williamsburg1.csv", stringsAsFactors = TRUE)
data.raw2 <- read.csv("Williamsburg2.csv", stringsAsFactors = TRUE)
library("RColorBrewer") 
library("ggplot2")

data1 <- data.frame(station = data.raw1$STATION, date = data.raw1$DATE, tmin = data.raw1$TMIN, tmax = data.raw1$TMAX, prcp = data.raw1$PRCP)
data2 <- data.frame(station = data.raw2$STATION, date = data.raw2$DATE, tmin = data.raw2$TMIN, tmax = data.raw2$TMAX, prcp = data.raw2$PRCP)
combined <- rbind.data.frame(data1, data2)
combined$date <- as.Date(as.character(combined$date), "%m/%d/%Y")

png(file="full_Williamsburg_station.png")
ggplot(combined, aes(x=date, y=station)) + geom_point() + labs(title = "Stations", x = "Dates", y = "Stations")
dev.off()

stations <- unique(combined$station)
fillstations <- rep(NA, length(stations))
stationstable <- data.frame(Station = stations, Days = fillstations)

stationnames <- c("USC00449151", "US1VAJC0061", "US1VAJC0040", "US1VAJC0062", "US1VAWLC006", "US1VAWLC004", "US1VAJC0022", "US1VAJC0044", "US1VAWLC005", "US1VAWLC002", "US1VAWLC003", "US1VAJC0004", "US1VAJC0026", "US1VAJC0028", "US1VAJC0007", "US1VAJC0029", "US1VAWLC011", "US1VAJC0050", "US1VAJC0051", "US1VAJC0031", "US1VAWLC015", "US1VAJC0055", "US1VAJC0012", "US1VAWLC013", "US1VAJC0036", "US1VAYR0010", "US1VAJC0038", "US1VAYR0012", "US1VAJC0018", "US1VAWLC001")
for(s in 1:length(stationnames)) {
  stationstable$Days[s] <- length(subset(combined$date, combined$station == stationnames[s]))
  current_stationPRCP <- subset(combined$prcp, combined$station == stationnames[s])
  current_stationTemp <- subset(combined$tmax, combined$station == stationnames[s]) # put into Tavg
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

station1 <- subset(combined, combined$station == "USC00449151")
station1 <- station1[order(station1$date), ] # potentially interesting to make an in_order function
station1$avgTemp <- (station1$tmin + station1$tmax) / 2
station1$year <- format(station1$date, "%Y")
years <- unique(station1$year)

fill <- rep(NA, length(years))
NAtable <- data.frame(year = years, NA_Tavg = fill, NA_prcp = fill, days = fill)

for(c in 1:(length(years))) {
  currentyear <- years[c]
  yeardata <- subset(station1, station1$year == currentyear)
  l <- is.na(yeardata$avgTemp)
  m <- is.na(yeardata$prcp)
  Tavg_na <- sum(l, na.rm = TRUE)
  prcp_na <- sum(m, na.rm = TRUE)
  NAtable$NA_Tavg[c] <- Tavg_na
  NAtable$NA_prcp[c] <- prcp_na
}

for(d in 1:(length(years))) {
  cyear <- years[d]
  y_data <- subset(station1, station1$year == cyear)
  NAtable$days[d] <- length(y_data$year)
}

fill <- rep(NA, length(years))
annual.summary <- data.frame(YEARS = years, AVGTEMP = fill, TOTALPRCP = fill, na_AVGTEMP = fill, na_TOTALPRCP = fill, days = fill)

for (c in 1:(length(years))) {
  current.year <- years[c] 
  yeardata <- subset(station1, station1$year == current.year)
  annual.summary$AVGTEMP[c] <- mean(yeardata$avgTemp, na.rm = TRUE) # should I make this True?
  annual.summary$TOTALPRCP[c] <- sum(yeardata$prcp, na.rm = TRUE)
}

for(d in 1:(length(years))) {
  cyear <- years[d]
  y_data <- subset(station1, station1$year == cyear)
  NAtable$days[d] <- length(y_data$year)
}

annual.summary$days <- NAtable$days


for(c in 1:length(years)) {
  cyear <- years[c]
  ydata <- subset(station1, station1$year == cyear)
  na_Tavg <- is.na(ydata$avgTemp)
  na_totalPRCP <- is.na(ydata$prcp)
  a <- sum(na_Tavg, na.rm = TRUE)
  b <- sum(na_totalPRCP, na.rm = TRUE)
  annual.summary$na_AVGTEMP[c] <- a
  annual.summary$na_TOTALPRCP[c] <- b
}


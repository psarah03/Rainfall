rm(list = ls(all = TRUE))
data.loc <- ("C:/Users/pazls/Desktop/Learning R/Rainfall/Data")
setwd(data.loc)
data.raw1 <- read.csv("Brevard1.csv", stringsAsFactors = TRUE)
data.raw2 <- read.csv("Brevard2.csv", stringsAsFactors = TRUE)
library("ggplot2")

data1 <- data.frame(Date = data.raw1$DATE, Station = data.raw1$STATION, Tmax = data.raw1$TMAX, Tmin = data.raw1$TMIN, PRCP = data.raw1$PRCP)
data2 <- data.frame(Date = data.raw2$DATE, Station = data.raw2$STATION, Tmax = data.raw2$TMAX, Tmin = data.raw2$TMIN, PRCP = data.raw2$PRCP)
data <- rbind(data1, data2)

png(file="full_Brevard_station.png")
converteddates <- as.character(data$Date)
converteddates <- as.Date(data$Date,"%m/%d/%Y")
data$Date <- converteddates
ggplot(data, aes(x=Date, y=Station)) + geom_point() + labs(title = "Stations", x = "Dates", y = "Stations") 
dev.off()

stations <- unique(data$Station)
fillstations <- rep(NA, length(stations))
stationstable <- data.frame(Station = stations, Days = fillstations)

stationnames <- c("USC00317486", "USC00316805", "USC00311055", "US1NCTR0025", "US1NCTR0002", "US1NCTR0024", "US1NCTR0035", "US1NCTR0001", "US1NCTR0012", "US1NCTR0034", "US1NCTR0011", "US1NCTR0022", "US1NCTR0021", "US1NCTR0032", "US1NCTR0020", "US1NCTR0031", "US1NCTR0030", "US1NCTR0009", "US1NCTR0008", "US1NCTR0019", "US1NCTR0016", "US1NCTR0026")
for(s in 1:length(stationnames)) {
  stationstable$Days[s] <- length(subset(data$Date, data$Station == stationnames[s]))
  current_stationPRCP <- subset(data$PRCP, data$Station == stationnames[s])
  current_stationTemp <- subset(data$Tmax, data$Station == stationnames[s]) # put into Tavg
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

data$Date <- as.Date(data$Date, "%m/%d/%Y")
data$Year <- format(data$Date, "%Y")
years <- unique(data$Year)
data <- data[order(data$Date), ]
data$Tavg <- (data$Tmax + data$Tmin) / 2

station2 <- subset(data, data$Station == "USC00316805")

fill <- rep(NA, length(years))
NAtable <- data.frame(year = years, NA_Tavg = fill, NA_prcp = fill, days = fill)

for(c in 1:(length(years))) {
  currentyear <- years[c]
  yeardata <- subset(station2, station2$Year == currentyear)
  l <- is.na(yeardata$Tavg)
  m <- is.na(yeardata$PRCP)
  Tavg_na <- sum(l, na.rm = TRUE)
  prcp_na <- sum(m, na.rm = TRUE)
  NAtable$NA_Tavg[c] <- Tavg_na
  NAtable$NA_prcp[c] <- prcp_na
}

for(d in 1:(length(years))) {
  cyear <- years[d]
  y_data <- subset(station2, station2$Year == cyear)
  NAtable$days[d] <- length(y_data$Year)
}

annual.summary <- data.frame(YEAR = years, AVGTEMP = fill, NA_AVGTEMP = NAtable$NA_Tavg, TOTALPRCP = fill, NA_PRCP = NAtable$NA_prcp)
for(f in 1:length(years)) {
  y <- years[f]
  ydata <- subset(station2, station2$Year == y) 
  annual.summary$AVGTEMP[f] <- mean(ydata$Tavg, na.rm = TRUE)
  annual.summary$TOTALPRCP[f] <- sum(ydata$PRCP, na.rm = TRUE)
}


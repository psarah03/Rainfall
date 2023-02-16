rm(list = ls(all = TRUE))
data.loc <- ("C:/Users/pazls/Desktop/Learning R/Rainfall/Data")
setwd(data.loc)
data.raw <- read.csv("Wise.csv", stringsAsFactors = TRUE)
library("ggplot2")

data <- data.frame(date = data.raw$DATE, station = data.raw$STATION, tmin = data.raw$TMIN, tmax = data.raw$TMAX, prcp = data.raw$PRCP)

png(file = "full_Wise_station.png")
converteddates <- as.Date(data.raw$DATE, "%m/%d/%Y")
data$date  <- converteddates
ggplot(data.raw, aes(x=converteddates, y=STATION)) + geom_point() + labs(title = "Stations", x = "Dates", y = "Stations")
dev.off()

data[order(data$date), ] # 
data$tavg <- (data$tmin + data$tmax)* (1/2)

fill <- rep(NA, length(unique(data$station)))
stationstable <- data.frame(station = fill, days = fill, PRCPvar = fill, Tempvar = fill)
stationstable$station[1] <- "USC00449215"
stationstable$days[1] <- length(subset(data.raw$DATE, data.raw$STATION == "USC00449215"))
stationstable$PRCPvar[1] <- 0
stationstable$Tempvar[1] <- 0
for (i in 1:length(data$prcp)) {
  if(is.na(data$prcp[i])) {
    stationstable$PRCPvar[1] = stationstable$PRCPvar[1] + 1 
  }
  if(is.na(data$tavg[i])) {
    stationstable$Tempvar[1] = stationstable$Tempvar[1] + 1
  }
}

data$year <- format(data$date, "%Y")
years <- unique(data$year)
station2 <- subset(data, data$station == "USC00449215")

fill <- rep(NA, length(years))
station2$tavg <- (station2$tmin + station2$tmax) / 2
NAtable <- data.frame(year = years, NA_Tavg = fill, NA_prcp = fill, days = fill)

for(c in 1:(length(years))) {
  currentyear <- years[c]
  yeardata <- subset(station2, station2$year == currentyear)
  l <- is.na(yeardata$tavg)
  m <- is.na(yeardata$prcp)
  Tavg_na <- sum(l, na.rm = TRUE)
  prcp_na <- sum(m, na.rm = TRUE)
  NAtable$NA_Tavg[c] <- Tavg_na
  NAtable$NA_prcp[c] <- prcp_na
}

for(d in 1:(length(years))) {
  cyear <- years[d]
  y_data <- subset(station2, station2$year == cyear)
  NAtable$days[d] <- length(y_data$year)
}

fill2 <- rep(NA, length(unique(data$year)))
annual.summary <- data.frame(year = unique(data$year), tavg = fill2, totalprcp = fill2, NA_tavg = NAtable$NA_Tavg, NA_prcp = NAtable$NA_prcp, days = NAtable$days)

for(m in 1:(length(years))) {
  cyear <- years[m]
  ydata <- subset(data ,data$year == cyear)
  annual.summary$totalprcp[m] <- sum(ydata$prcp, na.rm = TRUE)
  annual.summary$tavg[m] <- mean(ydata$tavg, na.rm = TRUE)
}


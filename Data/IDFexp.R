rm(list = ls(all = TRUE))
data.loc <- ("C:/Users/pazls/Desktop/Learning R/Rainfall/Data")
setwd(data.loc)
data.Baltimore_15min_Part1 <- read.csv("Baltimore_15min_Part1.csv", stringsAsFactors = TRUE)
data.Baltimore_15min_Part2 <- read.csv("Baltimore_15min_Part2.csv", stringsAsFactors = TRUE)
data.Baltimore_hourly <- read.csv("Baltimore_hourly.csv", stringsAsFactors = TRUE)
data.Baltimore_daily <- read.csv("Baltimore_daily.csv", stringsAsFactors = TRUE)
library(lubridate)
library(dplyr)
library("ggplot2")

## HOW GOOD IS THE DATA? ############################################################

## How to use lubridate?
example <- "19710911"
print(ymd(example))
example2 <- "19710911 04:15"
example2 <- ymd_hm(example2)
print(ymd_hm(example2))
# 19710911 04:15
example2 <- example2 + 3600
hour <- hour(example2)

## Step 1: Develop a coverage table
fill <- rep(NA, 6)
coverage <- data.frame(names = fill, prcp_start = fill, prcp_end = fill, prcp_coverage = fill)

B15 <- rbind.data.frame(data.Baltimore_15min_Part1, data.Baltimore_15min_Part2)
B15 <- data.frame(Station = B15$STATION, Station_name = B15$STATION_NAME, Date = B15$DATE, QPCP = B15$QPCP)
B15$Date <- ymd_hm(as.character(B15$Date))
B15missing <- 0
for(i in 1:(length(B15$QPCP))) {
  if(is.na(B15$QPCP[i])) {
    print(B15$QPCP[i])
  }
  if(B15$QPCP[i] < 0) {
    B15missing <- B15missing + 1
  }
} # 6262 missing values in B15
print(first_before <- head(B15$Date, 3)) # "1971-09-11 04:15:00 UTC" "1971-09-11 04:30:00 UTC" "1971-09-11 04:45:00 UTC"
print(prcp_before <- head(B15$QPCP, 3)) # 0.0 0.1 0.1
print(last_before <- tail(B15$Date, 3)) # "2013-12-29 12:45:00 UTC" "2013-12-29 13:45:00 UTC" "2014-01-01 00:00:00 UTC"
print(prcp_before2 <- tail(B15$QPCP, 3)) # 0.1 0.1 -9999.0
B15 <- B15 %>% arrange(Date)
print(first_after <- head(B15$Date, 3)) # "1971-05-07 21:15:00 UTC" "1971-05-07 21:30:00 UTC" "1971-05-08 01:15:00 UTC"
print(prcp_after <- head(B15$QPCP, 3)) # 0.0 0.2 0.1
print(last_after <- tail(B15$Date, 3)) # "2014-01-01 UTC" "2014-01-01 UTC" "2014-01-01 UTC" 
# 2014 is unusual
print(prcp_after2 <- tail(B15$QPCP, 3)) # -9999 -9999 -9999

coverage$names[1] <- "Baltimore 15"
coverage$prcp_start[1] <- as.character(first_after[1])
coverage$prcp_end[1] <- as.character(last_after[1])
B15duration <- 15581
coverage$prcp_coverage[1] <- 100 * (length(B15$Date) - B15missing) / (24 * 4 * B15duration)

Bhourly <-  data.frame(Station = data.Baltimore_hourly$STATION, Station_name = data.Baltimore_hourly$STATION_NAME, Date = data.Baltimore_hourly$DATE, QPCP = data.Baltimore_hourly$HPCP)

Bhourly$Date <- ymd_hm(as.character(Bhourly$Date))
Bhourlymissing <- 0
for(i in 1:(length(Bhourly$QPCP))) {
  if(is.na(Bhourly$QPCP[i])) {
    print(Bhourly$QPCP[i])
  }
  if(Bhourly$QPCP[i] == 999.99) { # sometimes NA values are represented by -999.0
    Bhourlymissing <- Bhourlymissing + 1
  }
} # 0 missing values in Bhourly
Bhourly <- Bhourly %>% arrange(Date)


coverage$names[2] <- "Baltimore hourly"
coverage$prcp_start[2] <- as.character(head(Bhourly$Date, 1)) # 1948-05-01 01:00:00
coverage$prcp_end[2] <- as.character(tail(Bhourly$Date, 1)) # 2013-12-29 15:00:00
Bhourlyduration <- 23984
coverage$prcp_coverage[2] <- 100 * (length(Bhourly$Date) - Bhourlymissing) / (24 * Bhourlyduration)

Bdaily <- data.frame(Station = data.Baltimore_daily$STATION, Station_name = data.Baltimore_daily$NAME, Date = data.Baltimore_daily$DATE, QPCP = data.Baltimore_daily$PRCP)
Bdaily$Date <- as.Date(Bdaily$Date, "%m/%d/%Y")
print(unique(format(Bdaily$Date, "%Y")))
Bdaily <- Bdaily[order(Bdaily$Date), ]
print(unique(format(Bdaily$Date, "%Y")))

station_list <- unique(data.Baltimore_daily$NAME) 
# LAUREL 3 W, MD US, BELTSVILLE, MD US, BRIGHTON DAM, MD US, ABERDEEN PHILLIPS FIELD, MD US

Aberdeen <- subset(data.Baltimore_daily, data.Baltimore_daily$NAME == "ABERDEEN PHILLIPS FIELD, MD US")
coverage$names[3] <- as.character(station_list[1])
coverage$prcp_start[3] <- as.character(head(Aberdeen$DATE, 1)) # "1940-01-01"
coverage$prcp_end[3] <- as.character(tail(Aberdeen$DATE, 1)) # "2010-01-01"
Bdailyduration <- 25569
Bdailymissing <- 0
for(i in 1:(length(Aberdeen$PRCP))) {
  if(is.na(Aberdeen$PRCP[i])) {
    Bdailymissing <- Bdailymissing + 1
  }
} 
coverage$prcp_coverage[3] <- 100 * (length(Aberdeen$DATE) - Bdailymissing) / (Bdailyduration)

Beltsville <- subset(data.Baltimore_daily, data.Baltimore_daily$NAME == "BELTSVILLE, MD US")
coverage$names[4] <- as.character(station_list[2])
coverage$prcp_start[4] <- as.character(head(Beltsville$DATE, 1)) # "1941-05-01"
coverage$prcp_end[4] <- as.character(tail(Beltsville$DATE, 1)) # "2010-01-01"
Bdailyduration <- 25569 # CHANGE!!
Bdailymissing <- 0
for(i in 1:(length(Beltsville$PRCP))) {
  if(is.na(Beltsville$PRCP[i])) {
    Bdailymissing <- Bdailymissing + 1
  }
} 
coverage$prcp_coverage[4] <- 100 * (length(Beltsville$DATE) - Bdailymissing) / (Bdailyduration)

Brighton <- subset(data.Baltimore_daily, data.Baltimore_daily$NAME == "BRIGHTON DAM, MD US")
coverage$names[5] <- as.character(station_list[3])
coverage$prcp_start[5] <- as.character(head(Brighton$DATE, 1)) # "1948-08-01"
coverage$prcp_end[5] <- as.character(tail(Brighton$DATE, 1)) # "2010-01-01"
Bdailyduration <- 25569 # CHANGE !!
Bdailymissing <- 0
for(i in 1:(length(Brighton$PRCP))) {
  if(is.na(Brighton$PRCP[i])) {
    Bdailymissing <- Bdailymissing + 1
  }
} 
coverage$prcp_coverage[5] <- 100 * (length(Brighton$DATE) - Bdailymissing) / (Bdailyduration)

Laurel <- subset(data.Baltimore_daily, data.Baltimore_daily$NAME == "LAUREL 3 W, MD US")
coverage$names[6] <- as.character(station_list[4])
coverage$prcp_start[6] <- as.character(head(Laurel$DATE, 1)) # "1948-08-01"
coverage$prcp_end[6] <- as.character(tail(Laurel$DATE, 1)) # "2010-01-01"
Bdailyduration <- 25569 # CHANGE !!
Bdailymissing <- 0
for(i in 1:(length(Laurel$PRCP))) {
  if(is.na(Laurel$PRCP[i])) {
    Bdailymissing <- Bdailymissing + 1
  }
} 
coverage$prcp_coverage[6] <- 100 * (length(Laurel$DATE) - Bdailymissing) / (Bdailyduration)
# Beltsville is a little closer

## HOURLY INTERVALS ############################################################################

# Experimenting with seq
d <- seq(as.Date("1910/1/1"), as.Date("1999/1/1"), "years")
max(d)

# 1 hour interval

dates <- seq(as.POSIXct("1948/05/01 00:00", tz = "UTC"), as.POSIXct("2013/12/29 24:00:00", tz = "UTC"), by="hour")
fill2 <- rep(0.00, length(dates))
newBhourly <- data.frame(dates = dates, QPCP = fill2)
availabledates <- unique(Bhourly$Date)

for(i in 1:length(dates)) { # 
  if(newBhourly$dates[i] %in% availabledates) {
    date <- newBhourly$dates[i]
    Bhourly_data <- subset(Bhourly, Bhourly$Date == date)
    maximum_list <- subset(Bhourly_data$QPCP, Bhourly_data$QPCP != 999.99)
    if(length(maximum_list) == 0) {
      newBhourly$QPCP[i] <- NA
    } else {
      newBhourly$QPCP[i] <- max(maximum_list)
    }
    
  }
}

# using in 
x <- seq(1:5)
4 %in% x
6 %in% x

# 2 hour interval

B2hour <- data.frame(date_2hour = newBhourly$dates, sum_2hour = fill2)
for(i in 1:length(newBhourly$dates)) {
  print(i)
  start <- subset(newBhourly, newBhourly$dates == newBhourly$dates[i])
  nexthour <- subset(newBhourly, newBhourly$dates == newBhourly$dates[i + 1])
  if(!is.na(start$QPCP) | !is.na(nexthour$QPCP))  {
    start_max <- max(start$QPCP)
    next_max <- max(nexthour$QPCP)
    B2hour$sum_2hour[i] <- sum(start_max + next_max)
  } else if(is.na(start$QPCP)) {
    B2hour$sum_2hour[i] <- NA
    i = i + 1
  } else if(is.na(nexthour$QPCP)) {
    B2hour$sum_2hour[i] <- NA
    i = i + 2 
  }
}

# saving my data

write.csv(newBhourly, "C:/Users/pazls/Desktop/Learning R/Rainfall/Data/newBhourly.csv", row.names=TRUE)
write.csv(B2hour, "C:/Users/pazls/Desktop/Learning R/Rainfall/Data/B2hour.csv", row.names=TRUE)

# 3 hour interval
B3hour <- data.frame(date_3hour = newBhourly$dates, sum_3hour = fill2)
for(i in 1:50) { # length(newBhourly$dates)
  print(i)
  start <- subset(newBhourly, newBhourly$dates == newBhourly$dates[i])
  nexthour <- subset(newBhourly, newBhourly$dates == newBhourly$dates[i + 1])
  lasthour <- subset(newBhourly, newBhourly$dates == newBhourly$dates[i + 2])
  if(!is.na(start$QPCP) | !is.na(nexthour$QPCP) | !is.na(lasthour$QPCP))  {
    start_max <- max(start$QPCP)
    next_max <- max(nexthour$QPCP)
    last_max <- max(lasthour$QPCP)
    B3hour$sum_3hour[i] <- sum(start_max + next_max + last_max)
  } else if(is.na(start$QPCP)) {
    B3hour$sum_3hour[i] <- NA
    i = i + 1
  } else if(is.na(nexthour$QPCP)) {
    B3hour$sum_3hour[i] <- NA
    i = i + 2 
  } else if(is.na(lasthour$QPCP)) {
    B3hour$sum_3hour[i] <- NA
    i = i + 3 
  }
}

print(head(B3hour, 50))

# 4 hour interval
B4hour <- data.frame(date_4hour = newBhourly$dates, sum_4hour = fill2)
for(i in 1:50) { # length(newBhourly$dates)
  print(i)
  start <- subset(newBhourly, newBhourly$dates == newBhourly$dates[i])
  nexthour <- subset(newBhourly, newBhourly$dates == newBhourly$dates[i + 1])
  nexthour2 <- subset(newBhourly, newBhourly$dates == newBhourly$dates[i + 2])
  lasthour <- subset(newBhourly, newBhourly$dates == newBhourly$dates[i + 3])
  if(!is.na(start$QPCP) | !is.na(nexthour$QPCP) | !is.na(nexthour2$QPCP) | !is.na(lasthour$QPCP))  {
    start_max <- max(start$QPCP)
    next_max <- max(nexthour$QPCP)
    next_max2 <- max(nexthour2$QPCP)
    last_max <- max(lasthour$QPCP)
    B4hour$sum_4hour[i] <- sum(start_max + next_max + next_max2 + last_max)
  } else if(is.na(start$QPCP)) {
    B4hour$sum_4hour[i] <- NA
    i = i + 1
  } else if(is.na(nexthour$QPCP)) {
    B4hour$sum_4hour[i] <- NA
    i = i + 2 
  } else if(is.na(nexthour2$QPCP)) {
    B4hour$sum_4hour[i] <- NA
    i = i + 3 
  } else if (is.na(lasthour$QPCP)) {
    B4hour$sum_4hour[i] <- NA
    i = i + 4 
  }
}

print(head(B4hour, 50))

## DAILY INTERVALS ##################################################################################

# need to pick the station with the most coverage: 

fill5 <- rep(NA, length(unique(Bdaily$Date)))
B1day <- data.frame(date_1day = fill5, sum_1day = fill5)

## DDF curves #######################################################################################

years <- unique(format(as.Date(newBhourly$dates), "%Y"))
fill4 <- rep(NA, length(years))
hour1ddf <- data.frame(year = years, annualprcp = fill4, m = fill4, probability = fill4, return_pd = fill4)
hour2ddf <- data.frame(year = years, annualprcp = fill4, m = fill4, probability = fill4, return_pd = fill4)

for(i in 1:length(years)) { # for newBhourly
  currentyear <- unique(format(as.Date(newBhourly$dates), "%Y"))
  year_data <- subset(newBhourly, currentyear == years[i])
  hour1ddf$annualprcp[i] <- sum(year_data$QPCP, na.rm = TRUE)
}

for(i in 1:length(years)) { # for B2hour
  currentyear <- unique(format(as.Date(B2hour$date_2hour), "%Y"))
  year_data <- subset(B2hour, currentyear == years[i])
  hour2ddf$annualprcp[i] <- sum(year_data$sum_2hour, na.rm = TRUE)
}

# assign rank to each annualprcp value
m <- c(14, 1, 2, 17)
print(rank(m))

hour1ddf$m <- rank(hour1ddf$annualprcp)
hour1ddf$probability <- hour1ddf$m / 67
hour1ddf$return_pd <- 1 / hour1ddf$probability

png(file="B1hourddf.png")
ggplot(hour1ddf, aes(x=return_pd, y=annualprcp)) + geom_point() + labs(title = "Depth-Duration-Frequency Curve", x = "Return period", y = "Depth(mm)") # + scale_x_continuous(trans='log10')
dev.off()

hour2ddf$m <- rank(hour2ddf$annualprcp)
hour2ddf$probability <- hour2ddf$m / 67
hour2ddf$return_pd <- 1 / hour2ddf$probability

png(file="B2hourddf.png")
ggplot(hour2ddf, aes(x=return_pd, y=annualprcp)) + geom_point() + labs(title = "Depth-Duration-Frequency Curve", x = "Return period", y = "Depth(mm)")
dev.off()

depth1hr <- subset(hour1ddf, hour1ddf$return_pd == 20)

# hypothetically, the closest to the 20 year storm is 

fill3 <- rep(NA, 7)
tenyearstorm <- data.frame(interval = fill3, annualprcp = fill3, return_pd = fill3)
tenyearstorm$interval <- c("1 hour", "2 hours", "3 hours", "4 hours", "1 day", "2 days", "3 days")
# should I include more intervals???
tenyearstorm$annualprcp[1] <- 97.36
tenyearstorm$return_pd[1] <- 22.333

ddf <- data.frame(time = fill3, depth = fill3, intensity = fill3)
ddf$time <- tenyearstorm$interval
ddf$depth <- tenyearstorm$annualprcp
ddf$intensity <- tenyearstorm$annualprcp / 60                                               


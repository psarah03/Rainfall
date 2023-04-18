rm(list = ls(all = TRUE))
data.loc <- ("C:/Users/pazls/Desktop/Learning R/Rainfall/Data")
setwd(data.loc)
data.Baltimore_15min_Part1 <- read.csv("Baltimore_15min_Part1.csv", stringsAsFactors = TRUE)
data.Baltimore_15min_Part2 <- read.csv("Baltimore_15min_Part2.csv", stringsAsFactors = TRUE)
data.Baltimore_hourly <- read.csv("Baltimore_hourly.csv", stringsAsFactors = TRUE)
library(lubridate)
library(dplyr)

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
fill <- rep(NA, 2)
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


## HOW TO USE THE DATA? ############################################################################

# Experimenting
d <- seq(as.Date("1910/1/1"), as.Date("1999/1/1"), "years")
max(d)

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

B1hour <- newBhourly

# using in 
x <- seq(1:5)
4 %in% x
6 %in% x

B2hour <- data.frame(date_2hour = newBhourly$dates, sum_2hour = fill2)
for(i in 1:length(newBhourly$dates)) {
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





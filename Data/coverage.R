rm(list = ls(all = TRUE))
data.loc <- ("C:/Users/pazls/Desktop/Learning R/Rainfall/Data")
setwd(data.loc)
data <- read.csv("3398701_CharlestonAirport.csv", stringsAsFactors = TRUE)

data.new <- data.frame(station = data$STATION, dates = data$DATE, prcp = data$PRCP, tmax = data$TMAX, tmin = data$TMIN, awnd = data$AWND)

fill <- rep(NA, 4)
coverage <- data.frame(var = fill, start_date = fill, end_date = fill, total_days = fill, dates_coverage1 = fill, dates_coverage2 = fill)
coverage$var <- c("prcp", "tmax", "tmin", "avg wind speed")

data.new$dates <- as.Date(data.new$dates, "%Y-%m-%d")
data.new <- data.new[order(data.new$dates), ]
start <- head(data.new$dates, 1)
end <- tail(data.new$dates, 1)
# days = 21915 days

prcp_na <- sum(is.na(data.new$prcp))
prcp_dates <- subset(data.new$dates, !is.na(data.new$prcp))
coverage$start_date[1] <- as.character(head(prcp_dates, 1))
coverage$end_date[1] <- as.character(tail(prcp_dates, 1))
coverage$total_days[1] <- length(prcp_dates)
coverage$dates_coverage1[1] <- 100 * (coverage$total_days[1] - prcp_na) / 21915
coverage$dates_coverage2[1] <- 100 * (coverage$total_days[1] - prcp_na) / length(prcp_dates)

tmax_na <- sum(is.na(data.new$tmax))
tmax_dates <- subset(data.new$dates, !is.na(data.new$tmax))
coverage$start_date[2] <- as.character(head(tmax_dates, 1))
coverage$end_date[2] <- as.character(tail(tmax_dates, 1))
coverage$total_days[2] <- length(tmax_dates)
coverage$dates_coverage1[2] <- 100 * (coverage$total_days[2] - tmax_na) / 21915
coverage$dates_coverage2[2] <- 100 * (coverage$total_days[2] - tmax_na) / length(tmax_dates)

tmin_na <- sum(is.na(data.new$tmin))
tmin_dates <- subset(data.new$dates, !is.na(data.new$tmin))
coverage$start_date[3] <- as.character(head(tmin_dates, 1))
coverage$end_date[3] <- as.character(tail(tmin_dates, 1))
coverage$total_days[3] <- length(tmin_dates)
coverage$dates_coverage1[3] <- 100 * (coverage$total_days[3] - tmin_na) / 21915
coverage$dates_coverage2[3] <- 100 * (coverage$total_days[3] - tmin_na) / length(tmin_dates)

awnd_na <- sum(is.na(data.new$awnd))
awnd_dates <- subset(data.new$dates, !is.na(data.new$awnd))
coverage$start_date[4] <- as.character(head(awnd_dates, 1))
coverage$end_date[4] <- as.character(tail(awnd_dates, 1))
coverage$total_days[4] <- length(awnd_dates)
coverage$dates_coverage1[4] <- 100 * (coverage$total_days[4] - awnd_na) / 21915
coverage$dates_coverage2[4] <- 100 * (coverage$total_days[4] - awnd_na) / length(awnd_dates)

data.new$tavg <- (data.new$tmax + data.new$tmin) / 2

# average high temp
years <- unique(format(data.new$dates, "%Y"))
fill <- rep(NA, length(years))
annualsummary <- data.frame(year = years, avghighesttemp = fill, total_prcp = fill)
# for(i in 1:length(years)) {
#  currentyear <- subset(data.new, format(data.new$dates, "%Y") == years[i])
#  annualsummary$avghighesttemp[i] <- max(currentyear$tavg)
#}
# total precipitation
# for(i in 1:length(years)) {
#  currentyear <- subset(data.new, format(data.new$dates, "%Y") == years[i])
#  annualsummary$total_prcp[i] <- sum(currentyear$prcp)
#}

avghighesttemp <- function(table) {
  for(i in 1:length(years)) {
    currentyear <- subset(table, format(table$dates, "%Y") == years[i])
    annualsummary$avghighesttemp[i] <- max(currentyear$tavg)
  }
  return(annualsummary$avghighesttemp)
}

totalprcp <- function(table) {
  for(i in 1:length(years)) {
    currentyear <- subset(table, format(table$dates, "%Y") == years[i])
    annualsummary$total_prcp[i] <- sum(currentyear$prcp)
  }
  return(annualsummary$total_prcp)
}

annualsummary$avghighesttemp <- avghighesttemp(data.new)
annualsummary$total_prcp <- totalprcp(data.new)


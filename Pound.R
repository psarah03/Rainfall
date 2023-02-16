rm(list = ls(all = TRUE))
data.loc <- ("C:/Users/pazls/Desktop/Learning R/Rainfall/Data")
setwd(data.loc)
data.raw <- read.csv("Pound.csv", stringsAsFactors = TRUE)
dim(data.raw) # 38 variables --> compare to other csv files
# head(data.raw, 6)

# Problem: are there repeated dates due to multiple stations?
# Solution: no repeated values
print(unique(data.raw$STATION)) # 1

# Problem: lots of missing values?
# Solution: shorten data frame to focus on dates with fewer NA values
# go through each year to determine number of NA values for Tmin, Tmax, PRCP

class(data.raw$DATE) # numeric
data.raw$DATE <- as.character(data.raw$DATE)
data.raw$DATE <- as.Date(data.raw$DATE, "%m/%d/%Y")
year <- format(data.raw$DATE, "%Y")

# made a new data frame, so I wouldn't mess up the original data set
data_new <- data.frame(year = year, Tmin = data.raw$TMIN, Tmax = data.raw$TMAX, PRCP = data.raw$PRCP)

fill <- rep(NA, length(unique(year)))
NAtable <- data.frame(year = unique(year), NA_Tmin = fill, NA_Tmax = fill, NA_PRCP = fill, days = fill)

numofyears <- unique(year)

# go through each year to determine number of non-NA values
for(c in 1:(length(numofyears))) {
  print(c)
  currentyear <- unique(year)[c]
  year_data <- subset(data_new, data_new$year == currentyear)
  l <- is.na(year_data$Tmin)
  m <- is.na(year_data$Tmax)
  n <- is.na(year_data$PRCP)
  Tmin_na <- sum(l, na.rm = TRUE)
  Tmax_na <- sum(m, na.rm = TRUE)
  PRCP_na <- sum(n, na.rm = TRUE)
  NAtable$NA_Tmin[c] <- Tmin_na
  NAtable$NA_Tmax[c] <- Tmax_na
  NAtable$NA_PRCP[c] <- PRCP_na
}

# Problem: are there missing dates?
# Solution: ??
# days between 1970-02-91 (y-m-d) to 2022-10-31 (y-m-d) = 19,266
# go through each year to see how many days are missing
print(length(data.raw$DATE)) # 17208
# see how many days per year are recorded

for(d in 1:(length(numofyears))) {
  cyear <- unique(year)[d]
  print(cyear)
  y_data <- subset(data_new$year, data_new$year == cyear)
  NAtable$days[d] <- length(y_data)
}

# can make a data table for precipitation from 1970 to 1991
# can make a temperature table for 2011 to 2022

data1 <- data.frame(DATE = data.raw$DATE, TMIN = data.raw$TMIN, TMAX = data.raw$TMAX,PRCP = data.raw$PRCP)
data1$DATE <- as.Date(data1$DATE, format = "%m/%d/%Y")

# create a Tavg
Tavg <- (data1$TMIN + data1$TMAX) / 2
data1$TAVG <- Tavg

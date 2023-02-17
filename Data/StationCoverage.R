rm(list = ls(all = TRUE))
data.loc <- ("C:/Users/pazls/Desktop/Learning R/Rainfall/Data")
setwd(data.loc)
data.EllicottCity <- read.csv("EllicottCity.csv", stringsAsFactors = TRUE)
data.Williamsburg1 <- read.csv("Williamsburg1.csv", stringsAsFactors = TRUE)
data.Williamsburg2 <- read.csv("Williamsburg2.csv", stringsAsFactors = TRUE)
data.Brevard1 <- read.csv("Brevard1.csv", stringsAsFactors = TRUE)
data.Brevard2 <- read.csv("Brevard2.csv", stringsAsFactors = TRUE)
data.Wise <- read.csv("Wise.csv", stringsAsFactors = TRUE)
dim(data.raw)
library("ggplot2")

print(unique(data.EllicottCity$STATION))
EllicottCity <- c("US1MDHW0013")
# 5 levels --> only 1 is relevant: US1MDHW0013

print(unique(data.Williamsburg1$STATION))
print(unique(data.Williamsburg2$STATION)) # USC00449151
Williamsburg <- c("USC00449151")

print(unique(data.Brevard1$STATION))
print(unique(data.Brevard2$STATION))
# USC00317486, USC00316805, USC00311055
Brevard <- c("USC00317486", "USC00316805", "USC00311055")

print(unique(data.Wise$STATION)) # USC00448215
Wise <- c("USC00448215")

station_list <- c(EllicottCity, Williamsburg, Brevard, Wise)
fill <- rep(NA, 6)
stationcoverage <- data.frame(names = fill, station = station_list, temp_start = fill, temp_end = fill, temp_coverage = fill, precip_start = fill, precip_end = fill, precip_coverage = fill)

## Ellicott City #############################################################

data.EllicottCity$TAVG <- (data.EllicottCity$TMIN + data.EllicottCity$TMAX) *(1/2)
png(file = "EllicottCity_temp.png")
d = c()
e = c()

for(i in 1:(length(data.EllicottCity$DATE))) {
  if(!is.na(data.EllicottCity$TAVG[i])) {
    d <- append(d, data.EllicottCity$DATE[i], after = length(d))
    e <- append(e, 1, after = length(e))
  }
}
temp_dataEC <- data.frame(dates = d, height = e)
temp_dataEC$dates <- as.Date(temp_dataEC$dates, "%m/%d/%Y")


data.EllicottCity$DATE <- as.Date(data.EllicottCity$DATE, "%m/%d/%Y")
data.EllicottCity[order(data.EllicottCity$DATE), ]

# a gap of a year probably requires splitting the date into two parts

# haven't figured out how to split data into two parts

# longest = c()
# longest_start = c()
# longest_placeholder <- 0
# for(i in 1:(length(data.EllicottCity$DATE))) {
#  if (is.na(data.EllicottCity$TAVG[i])) {
#    longest_start <-  append(longest_start, data.EllicottCity$DATE[i], after = length(longest_start))
#    longest_placeholder <- longest_placeholder + 1
#  } 
#  if (!is.na(data.EllicottCity$TAVG[i])) {
#    longest <- append(longest, longest_placeholder, after = length(longest))
#  }
# }

ggplot(temp_dataEC, aes(x=dates, y=height)) + geom_point() + labs(title = "Temperature Coverage", x = "Dates", y = "")

# why does xlim(1970, 2023) produce the following error message?
# Warning message: Removed 11101 rows containing missing values

dev.off()

head(data.EllicottCity$DATE, 1)

####### Williamsburg ##############################################################

length(data.Williamsburg1)
length(data.Williamsburg2)
data.Williamsburg1 <- data.frame(STATION = data.Williamsburg1$STATION, DATE = data.Williamsburg1$DATE, TMAX = data.Williamsburg1$TMAX, TMIN = data.Williamsburg1$TMIN, PRCP = data.Williamsburg1$PRCP)
data.Williamsburg2 <- data.frame(STATION = data.Williamsburg2$STATION, DATE = data.Williamsburg2$DATE, TMAX = data.Williamsburg2$TMAX, TMIN = data.Williamsburg2$TMIN, PRCP = data.Williamsburg2$PRCP)

data.Williamsburg <- rbind.data.frame(data.Williamsburg1, data.Williamsburg2)

data.Williamsburg$TAVG <- (data.Williamsburg$TMIN + data.Williamsburg$TMAX) *(1/2)
png(file = "Williamsburg_temp.png")

f = c()
g = c()

for(j in 1:(length(data.Williamsburg$DATE))) {
  if(!is.na(data.Williamsburg$TAVG[j])) {
    f <- append(f, data.Williamsburg$DATE[j], after = length(f))
    g <- append(g, 1, after = length(g))
  }
}
temp_dataW <- data.frame(dates = f, height = g)
temp_dataW$dates <- as.Date(temp_dataW$dates, "%m/%d/%Y")


data.Williamsburg$DATE <- as.Date(data.Williamsburg$DATE, "%m/%d/%Y")
data.Williamsburg[order(data.Williamsburg$DATE), ]

ggplot(temp_dataW, aes(x=dates, y=height)) + geom_point() + labs(title = "Temperature Coverage", x = "Dates", y = "")

dev.off()

###### Brevard ####################################################

length(data.Brevard1)
length(data.Brevard2) # same size
data.Brevard1 <- data.frame(STATION = data.Brevard1$STATION, DATE = data.Brevard1$DATE, TMAX = data.Brevard1$TMAX, TMIN = data.Brevard1$TMIN, PRCP = data.Brevard1$PRCP)
data.Brevard2 <- data.frame(STATION = data.Brevard2$STATION, DATE = data.Brevard2$DATE, TMAX = data.Brevard2$TMAX, TMIN = data.Brevard2$TMIN, PRCP = data.Brevard2$PRCP)
data.Brevard <- rbind.data.frame(data.Brevard1, data.Brevard2)

data.Brevard$TAVG <- (data.Brevard$TMIN + data.Brevard$TMAX) *(1/2)
unique(data.Brevard$STATION) # 3 

data.BrevardS1 <- subset(data.Brevard, data.Brevard$STATION == "USC00317486")
data.BrevardS2 <- subset(data.Brevard, data.Brevard$STATION == "USC00316805")
data.BrevardS3 <- subset(data.Brevard, data.Brevard$STATION == "USC00311055")
png(file = "Brevard_temp.png")

l = c()
m = c()

for(k in 1:(length(data.BrevardS1$DATE))) {
  if(!is.na(data.BrevardS1$TAVG[k])) {
    l <- append(l, data.BrevardS1$DATE[k], after = length(l))
    m <- append(m, 1, after = length(m))
  }
}

for(k in 1:(length(data.BrevardS2$DATE))) {
  if(!is.na(data.BrevardS2$TAVG[k])) {
    l <- append(l, data.BrevardS2$DATE[k], after = length(l))
    m <- append(m, 2, after = length(m))
  }
}

for(k in 1:(length(data.BrevardS3$DATE))) {
  if(!is.na(data.BrevardS3$TAVG[k])) {
    l <- append(l, data.BrevardS3$DATE[k], after = length(l))
    m <- append(m, 3, after = length(m))
  }
}

class(data.BrevardS1$DATE[1])
class(data.BrevardS2$DATE[1])
class(data.BrevardS3$DATE[1])

temp_dataB <- data.frame(dates = l, height = m)
temp_dataB$dates <- as.Date(temp_dataB$dates, "%m/%d/%Y")

data.BrevardS1$DATE <- as.Date(data.BrevardS1$DATE, "%m/%d/%Y")
data.BrevardS1[order(data.BrevardS1$DATE), ]

data.BrevardS2$DATE <- as.Date(data.BrevardS2$DATE, "%m/%d/%Y")
data.BrevardS2[order(data.BrevardS2$DATE), ]

data.BrevardS3$DATE <- as.Date(data.BrevardS3$DATE, "%m/%d/%Y")
data.BrevardS3[order(data.BrevardS3$DATE), ]

ggplot(temp_dataB, aes(x=dates, y=height)) + geom_point() + labs(title = "Temperature Coverage", x = "Dates", y = "")

dev.off()

head(data.BrevardS1$DATE, 1)
tail(data.BrevardS1$DATE, 1)

head(data.BrevardS2$DATE, 1) # 1970-01-01
tail(data.BrevardS2$DATE, 1)

head(data.BrevardS3$DATE, 1)
tail(data.BrevardS3$DATE, 1)

############ Wise ######################################################



################## how to append vectors ###############################
v <- c("a", "b")
length(v)
v <- append(v, "c", after = length(v))



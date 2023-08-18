rm(list = ls(all = TRUE))
data.loc <- ("C:/Users/pazls/Documents/Appalachian/Data/")
setwd(data.loc)
data <- read.csv("3398701_CharlestonAirport.csv", stringsAsFactors = TRUE)
library("ggplot2")

data.new <- data.frame(station = data$STATION, dates = data$DATE, prcp = data$PRCP, tmax = data$TMAX, tmin = data$TMIN, awnd = data$AWND)

fill <- rep(NA, 4)
coverage <- data.frame(var = fill, start_date = fill, end_date = fill, total_days = fill, dates_coverage = fill)
coverage$var <- c("prcp", "tmax", "tmin", "avg wind speed")

data.new$dates <- as.Date(data.new$dates, format = "%m/%d/%Y") 
data.new <- data.new[order(data.new$dates), ]
start <- head(data.new$dates, 1)
end <- tail(data.new$dates, 1)
# days = 21915 days

prcp_na <- sum(is.na(data.new$prcp))
prcp_dates <- subset(data.new$dates, !is.na(data.new$prcp))
coverage$start_date[1] <- as.character(head(prcp_dates, 1))
coverage$end_date[1] <- as.character(tail(prcp_dates, 1))
coverage$total_days[1] <- length(prcp_dates)
coverage$dates_coverage[1] <- 100 * (coverage$total_days[1]) / 21915

tmax_na <- sum(is.na(data.new$tmax))
tmax_dates <- subset(data.new$dates, !is.na(data.new$tmax))
coverage$start_date[2] <- as.character(head(tmax_dates, 1))
coverage$end_date[2] <- as.character(tail(tmax_dates, 1))
coverage$total_days[2] <- length(tmax_dates)
coverage$dates_coverage[2] <- 100 * (coverage$total_days[2]) / 21915

tmin_na <- sum(is.na(data.new$tmin))
tmin_dates <- subset(data.new$dates, !is.na(data.new$tmin))
coverage$start_date[3] <- as.character(head(tmin_dates, 1))
coverage$end_date[3] <- as.character(tail(tmin_dates, 1))
coverage$total_days[3] <- length(tmin_dates)
coverage$dates_coverage[3] <- 100 * (coverage$total_days[3]) / 21915

awnd_na <- sum(is.na(data.new$awnd))
awnd_dates <- subset(data.new$dates, !is.na(data.new$awnd))
coverage$start_date[4] <- as.character(head(awnd_dates, 1))
coverage$end_date[4] <- as.character(tail(awnd_dates, 1))
coverage$total_days[4] <- length(awnd_dates)
coverage$dates_coverage[4] <- 100 * (coverage$total_days[4]) / 21915

data.new$tavg <- (data.new$tmax + data.new$tmin) / 2

# average high temp
years <- unique(format(data.new$dates, "%Y"))
fill <- rep(NA, length(years))
annualsummary <- data.frame(year = years, avghighesttemp = fill, total_prcp = fill, daysover90degrees = fill, max5dayrainfall = fill)

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

# days over 90 degrees F

daysover90degrees <- function(table) {
  for(i in 1:length(years)) {
    currentyear <- subset(table, format(table$dates, "%Y") == years[i])
    hotdays <- subset(currentyear, currentyear$tmax >= 90) # greater than equal
    annualsummary$daysover90degrees[i] <- length(hotdays$tmax)
  }
  return(annualsummary$daysover90degrees)
}

# maximum 5 day rainfall
# don't forget about leap years
max5dayrainfall <- function(table) {
  for(i in 1:1) { #length(years)
    currentyear <- subset(table, format(table$dates, "%Y") == years[i])
    yearlength <- 0
    if (length(currentyear$dates) == 365) {
      yearlength <- 361
    } else if (length(currentyear$dates == 366)){
      yearlength <- 362
    }
    list <- rep(0, 365)
    for (j in 1:yearlength) {
      dayspan <- c(currentyear$prcp[j], currentyear$prcp[j+1], currentyear$prcp[j+2], currentyear$prcp[j+3], currentyear$prcp[j+4])
      list[j] <- sum(dayspan)
    }
    annualsummary$max5dayrainfall[i] <- max(list)
  }
  return(annualsummary$max5dayrainfall)
}

annualsummary$avghighesttemp <- avghighesttemp(data.new)
annualsummary$total_prcp <- totalprcp(data.new)
annualsummary$daysover90degrees <- daysover90degrees(data.new)
annualsummary$max5dayrainfall <- max5dayrainfall(data.new)

data.loc <- ("C:/Users/pazls/Documents/Appalachian/Images/")
setwd(data.loc)
# plot coverage and variables of annual summary
png(file="App_avghighesttemp.png")
ggplot(annualsummary, aes(x=year, y=avghighesttemp)) + geom_point() + labs(title = "Average Highest Temperature", x = "Year", y = "Temperature (F)") + scale_x_discrete(breaks=c(1960, 1970, 1980, 1990, 2000, 2010, 2020))
dev.off()

png(file="App_totalprcp.png")
ggplot(annualsummary, aes(x=year, y=total_prcp)) + geom_point() + labs(title = "Average Total Precipitation", x = "Year", y = "Precipitation (in)")  + scale_x_discrete(breaks=c(1960, 1970, 1980, 1990, 2000, 2010, 2020))
dev.off()





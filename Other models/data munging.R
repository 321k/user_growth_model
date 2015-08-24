setwd('/Users/transferwise/user_growth_model/Other models')

db <- read.csv('weekly user growth total.csv', sep=',', na.strings='NA')
tmp <- read.csv('weekly user growth GBPINR.csv', sep=',', na.strings='NA')

db <- merge(tmp, db)
db$start_date <- as.Date(db$date)
db$end_date <- db$start_date+6
db$yearmon <- as.yearmon(db$start_date)

GBPINR <- as.data.frame(getFX('GBP/INR', from = '2012-01-01', to = Sys.Date(), auto.assign=F))
GBPINR$date <- as.Date(rownames(GBPINR))

db$avg_rate <- NA
for(i in 1:nrow(db)){
  weekspan <- GBPINR[which(GBPINR$date>=db$start_date[i] & GBPINR$date<=db$end_date[i]),]
  db$avg_rate[i] <- mean(weekspan$'GBP.INR')
}

db$end_of_month <- 0
db$end_of_month[which(db$yearmon[-n] != db$yearmon[-1])] <- 1

n <- nrow(db)
db$d_avg_rate <- c(NA, db$avg_rate[-1]/db$avg_rate[-n]-1)
db$d_WNU_GBPINR <- c(NA, db$WNU_GBPINR[-1]/db$WNU_GBPINR[-n]-1)
db$d_WNU <- c(NA, db$WNU[-1]/db$WNU[-n]-1)
db$lag1_d_WNU_GBPINR <- c(NA, db$d_WNU_GBPINR[-n])
db$lag2_d_WNU_GBPINR <- c(NA, db$lag1_d_WNU_GBPINR[-n])
db$lag1_d_WNU <- c(NA, db$d_WNU[-n])
db$lag2_d_WNU <- c(NA, db$lag1_d_WNU[-n])


write.csv(db, "ready_data.csv")

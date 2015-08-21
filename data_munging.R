print('Data munging')
setwd('/Users/transferwise/Google\ Drive/Research/User\ growth\ model')

# Add libraries
print('Load libraries')
library(tseries)
library(e1071)
library(reshape)
library(quantmod)
library(Kmisc)
library(plm)
library(ggplot2)
library(plyr)
library(lubridate)
source('multiplot.R')

# List of currencies
from_ccy <- c('GBP', 'EUR', 'GBP', 'EUR', 'GBP', 'GBP', 'EUR')
to_ccy <- c('EUR', 'GBP', 'INR', 'INR', 'HUF', 'USD', 'USD')
#from_ccy <- c('GBP', 'EUR', 'GBP', 'EUR', 'GBP')
#to_ccy <- c('EUR', 'GBP', 'INR', 'INR', 'HUF')

currencies <- data.frame(from_ccy, to_ccy)
currencies$db_pair <- paste(from_ccy, to_ccy, sep=' > ')
currencies$qm_pair <- paste(from_ccy, to_ccy, sep='/')
db_pair <- paste("'", currencies$db_pair, "'", sep="", collapse = ',')

print('Read data')
db <- read.csv('weekly user growth by corridor.csv', sep='\t', na.strings='NULL')

#db$first_ccy_pair <- as.character(db$first_ccy_pair)
db$start_date <- as.Date(as.character(db$date), '%Y-%m-%d')
db$date <- db$start_date + 5
db$new_users <- as.character(db$new_users)

db$month <- as.numeric(substr(as.character(db$date), 6, 7)) #   Add month
db$yearmonth <- as.yearmon(db$date) #   Add year and month
db$year <- as.numeric(substr(as.character(db$date), 1, 4)) #   Add year
db_pair <- unique(db$first_ccy_pair)

# Calculate % change in user numbers
db <- ddply(db, "first_ccy_pair", transform,  DeltaCol = Delt(new_users, type='arithmetic'))
names(db)[ncol(db)] <- 'd_new_users'

db <- ddply(db, "first_ccy_pair", transform,  DeltaCol = Delt(new_users, type='log'))
names(db)[ncol(db)] <- 'ln_new_users'

# Calculate % change in nps score
db <- ddply(db, "first_ccy_pair", transform,  DeltaCol = Delt(nps_score, type='log'))
names(db)[ncol(db)] <- 'ln_nps_score'
db$ln_nps_score[which(!is.finite(db$ln_nps_score))] <- 0

# Get exchange rates
rates <- new.env()
getFX(currencies$qm_pair, from = '2012-01-01', to = Sys.Date(), env = rates)

# Add exchange rates to table
db$xrate <- NA
for(j in 1:length(currencies$db_pair)){  
  corridor_rows <- which(db$first_ccy_pair == currencies$db_pair[j])
  xrate = eval(parse(text=paste('rates$', currencies$from_ccy[j], currencies$to_ccy[j], sep="")))
  xrate <- as.data.frame(xrate)
  names(xrate) <- 'rate'
  xrate$date=as.Date(rownames(xrate), '%Y-%m-%d')
  xrate <- merge(db[corridor_rows,], xrate, all.x=T, all.y=F, by='date')
  db$xrate[corridor_rows] <- xrate$rate
}

# Calculate other exchange rate measures
db <- ddply(db, "first_ccy_pair", transform,  DeltaCol = Delt(xrate, type='arithmetic'))
names(db)[ncol(db)] <- 'd_xrate'

db <- ddply(db, "first_ccy_pair", transform,  DeltaCol = Delt(xrate, type='log'))
names(db)[ncol(db)] <- 'ln_xrate'

db$high_xrate <- NA

# Flag high exchange rate
for(j in 1:length(currencies$db_pair)){  
  corridor_rows <- which(db$first_ccy_pair == currencies$db_pair[j])
  for(i in 2:length(corridor_rows)){
    xrate_mean <- mean(db$xrate[corridor_rows][max(1, (i-6)):i], na.rm=T) #  Mean past x days
    xrate_sd <- sd(db$xrate[corridor_rows][max(1, (i-6)):i], na.rm=T) #  Standard deviation past x days
    if(db$xrate[corridor_rows][i] > (xrate_mean + 0.2 * xrate_sd) & !is.na(db$xrate[corridor_rows][i]) & !is.na(xrate_mean) & !is.na(xrate_sd)){
      db$high_xrate[corridor_rows][i] = 1
    } else {
      db$high_xrate[corridor_rows][i] = 0
    }
  }
}

# Add end of month dummy
db$end_of_month <- NA

for(j in 1:length(currencies$db_pair)){  
  corridor_rows <- which(db$first_ccy_pair == currencies$db_pair[j])
  for(i in 1:(length(corridor_rows))){
    this_month <- db$yearmonth[corridor_rows][i]
    next_month <- as.yearmon(db$date[corridor_rows][i]+7)
    if(is.na(next_month) | is.na(this_month)){
      next()
    }
    if(this_month < next_month){
      db$end_of_month[corridor_rows][i] = 'end of month'
    } else {
      db$end_of_month[corridor_rows][i] = 'not end'
    } 
  }
}

# Add marketing spend
marketing_spend <- read.csv('marketing spend.csv')
marketing_spend$date <- as.Date(as.character(marketing_spend$date), '%d/%m/%Y')
marketing_spend$month <- substr(as.character(marketing_spend$date), 6, 7)
marketing_spend$year <- substr(as.character(marketing_spend$date), 1, 4)
marketing_spend <- marketing_spend[-1]
db <- merge(db, marketing_spend, by = c('month', 'year'), all.x=T)
db <- db[order(db$date),]

# Calculate abnormal new users
window <- 5
db$ab_new_users <- NA
for(j in 1:length(currencies$db_pair)){  
  corridor_rows <- which(db$first_ccy_pair == currencies$db_pair[j])
  
  for(i in window:length(db$new_users[corridor_rows])){
    expected_incremental <- mean(db$new_users[corridor_rows][(i-window):i-1])
    actual_incremental <- db$new_users[corridor_rows][i]
    db$ab_new_users[corridor_rows][i] <- actual_incremental - expected_incremental
  }
}

# Calcualte expected and abnormal growth
window <- 15
db$exp_d_new_users <- NA
db$ab_d_new_users <- NA
for(j in 1:length(currencies$db_pair)){  
  corridor_rows <- which(db$first_ccy_pair == currencies$db_pair[j])
  for(i in window:length(db$new_users[corridor_rows])){
    expected_growth <- mean(db$d_new_users[corridor_rows][(i-window):(i-1)])
    actual_growth <- db$d_new_users[corridor_rows][i]
    db$exp_d_new_users[corridor_rows][i] <- expected_growth
    db$ab_d_new_users[corridor_rows][i] <- actual_growth - expected_growth
  }
}

# Calcualte lagged values
db$d_new_users_1 <- NA
db$d_new_users_2 <- NA
db$ln_nps_score_1 <- NA
db$d_xrate_1 <- NA
db$d_xrate_p1 <- NA

for(j in 1:length(currencies$db_pair)){  
  corridor_rows <- which(db$first_ccy_pair == currencies$db_pair[j])
  series <- db$d_new_users[corridor_rows]
  db$d_new_users_1[corridor_rows] <- c(0,series[-length(series)])
  db$d_new_users_2[corridor_rows] <- c(0,0,series[-((length(series)-1):length(series))])
  series <- db$ln_nps_score[corridor_rows]
  db$ln_nps_score_1[corridor_rows] <- c(0,series[-length(series)])
  series <- db$d_xrate[corridor_rows]
  db$d_xrate_1[corridor_rows] <- c(0,series[-length(series)])
  db$d_xrate_p1[corridor_rows] <- c(series[-1], 0)
}

db[(nrow(db)-20):nrow(db),]
# Save results
db_fin <- db[which(!is.na(db$xrate)),]

print('Write data')
write.csv(db_fin, 'Tables/ready_data.csv', row.names=F)

rates_table<- data.frame()

for(j in 1:length(currencies$db_pair)){  
  xrate = eval(parse(text=paste('rates$', currencies$from_ccy[j], currencies$to_ccy[j], sep="")))
  rates_table <- cbind(rates_table, xrate)
  xrate <- as.data.frame(xrate)
  xrate$date=as.Date(rownames(xrate), '%Y-%m-%d')
  start_date <- as.Date(Sys.time())-60
  xrate <- xrate[which(xrate$date>=start_date),]
  ggplot(xrate, aes(date, xrate[,1]))+geom_line()+ggtitle(names(xrate)[1])+ylab(names(xrate)[1])
  ggsave(filename=paste('plots/', names(xrate)[1], '.png', sep=''), width=5, height=2.5)
}

write.csv(rates_table, 'Tables/rates_table.csv')
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(db[which(db$start_date>=start_date),], aes(first_ccy_pair, new_users, group=week, fill=week))+geom_bar(stat='identity', position='dodge')+
  scale_fill_brewer(palette="Spectral")
ggsave(filename=paste('plots/new_users.png', sep=''), width=10, height=5)

names(db)

?geom_bar

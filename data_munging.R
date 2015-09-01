setwd('/Users/transferwise/user_growth_model')

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
from_ccy <- c('GBP', 'EUR', 'GBP', 'EUR', 'GBP', 'GBP', 'EUR', 'USD')
to_ccy <- c('EUR', 'GBP', 'INR', 'INR', 'HUF', 'USD', 'USD', 'INR')
currencies <- data.frame(from_ccy, to_ccy)
currencies$db_pair <- paste(from_ccy, to_ccy, sep=' > ')
currencies$qm_pair <- paste(from_ccy, to_ccy, sep='/')
db_pair <- paste("'", currencies$db_pair, "'", sep="", collapse = ',')
write.csv(currencies, 'currencies.csv')

print('Read data')
db <- read.csv('weekly user growth by corridor.csv', sep='\t', na.strings='NULL')

db$date <- as.Date(as.character(db$date), '%Y-%m-%d')
db$start_date <- as.Date(as.character(db$date), '%Y-%m-%d')
db$end_date <- db$start_date + 6
db$new_users <- as.numeric(as.character(db$new_users))

db$month <- as.numeric(substr(as.character(db$date), 6, 7)) #   Add month
db$yearmonth <- as.yearmon(db$date) #   Add year and month
db$year <- as.numeric(substr(as.character(db$date), 1, 4)) #   Add year
db_pair <- unique(db$first_ccy_pair)

# Calculate % change in user numbers
db <- ddply(db, "first_ccy_pair", transform,  DeltaCol = Delt(new_users, type='arithmetic'))
names(db)[ncol(db)] <- 'd_new_users'

db <- ddply(db, "first_ccy_pair", transform,  DeltaCol = Delt(new_users, type='log'))
names(db)[ncol(db)] <- 'ln_new_users'

# Get exchange rates
rates <- new.env()
getFX(currencies$qm_pair, from = '2012-01-01', to = Sys.Date(), env = rates)

# Add exchange rates to table
db$xrate <- NA
db$max_rate <- NA
db$min_rate <- NA
#for(j in 1:length(currencies$db_pair)){  
#  r <- which(db$first_ccy_pair == currencies$db_pair[j])
#  xrate = eval(parse(text=paste('rates$', currencies$from_ccy[j], currencies$to_ccy[j], sep="")))
#  xrate <- as.data.frame(xrate)
#  names(xrate) <- 'rate'
#  xrate$date=as.Date(rownames(xrate), '%Y-%m-%d')
#  xrate <- merge(db[r,], xrate, all.x=T, all.y=F, by='date')
#  db$xrate[r] <- xrate$rate
#}

for(j in 1:length(currencies$db_pair)){
  r <- which(db$first_ccy_pair == currencies$db_pair[j])
  xrate = as.data.frame(
    eval(parse(text=paste('rates$', currencies$from_ccy[j], currencies$to_ccy[j], sep="")))
  )
  names(xrate) <- 'rate'
  xrate$date=as.Date(rownames(xrate), '%Y-%m-%d')
  
  for(i in r){
    weekspan <- xrate[which(xrate$date>=db$start_date[i] & xrate$date<=db$end_date[i]),]
    db$xrate[i] <- mean(weekspan$rate)
    db$max_rate[i] <- max(weekspan$rate)
    db$min_rate[i] <- min(weekspan$rate)
  }
}

# Calculate other exchange rate measures
db <- ddply(db, "first_ccy_pair", transform,  DeltaCol = Delt(xrate, type='arithmetic'))
names(db)[ncol(db)] <- 'd_xrate'

db <- ddply(db, "first_ccy_pair", transform,  DeltaCol = Delt(xrate, type='log'))
names(db)[ncol(db)] <- 'ln_xrate'


# Calculate the maximum change between two weeks
db$max_d_xrate <- NA
for(j in 1:length(currencies$db_pair)){  
  r <- which(db$first_ccy_pair == currencies$db_pair[j])
  n <- length(r)
  db$max_d_xrate[r] <- db$max_rate[r] / c(0,db$min_rate[r][-n]) - 1
}


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
db$end_of_month <- 'not end'
for(j in 1:length(currencies$db_pair)){  
  r <- which(db$first_ccy_pair == currencies$db_pair[j])
  n <- length(r)
  db$end_of_month[r][which(db$yearmon[r][-n] != db$yearmon[r][-1])] <- 'end of month'
}

# Add marketing spend
marketing_spend <- read.csv('marketing spend.csv')
marketing_spend$date <- as.Date(as.character(marketing_spend$date), '%d/%m/%Y')
marketing_spend$month <- substr(as.character(marketing_spend$date), 6, 7)
marketing_spend$year <- substr(as.character(marketing_spend$date), 1, 4)
marketing_spend <- marketing_spend[-1]
db <- merge(db, marketing_spend, by = c('month', 'year'), all.x=T)
db <- db[order(db$date),]

# Calcualte lagged values
db$new_users_1 <- NA
db$d_new_users_1 <- NA
db$d_new_users_2 <- NA
db$d_xrate_1 <- NA

for(j in 1:length(currencies$db_pair)){  
  r <- which(db$first_ccy_pair == currencies$db_pair[j])
  n <- length(db$d_new_users[r])
  db$new_users_1[r] <- c(0,db$new_users[r][-n])
  db$d_new_users_1[r] <- c(0,db$d_new_users[r][-n])
  db$d_new_users_2[r] <- c(0,db$d_new_users_1[r][-n])
  db$d_xrate_1[r] <- c(0,db$d_xrate[r][-n])
}

# Remove growth for small corridors
#db[which(db$first_ccy_pair=='USD > INR' & db$start_date <= '2015-01-01'),c('d_new_users', 'ln_new_users', 'd_xrate')] <- NA



# Save results
db_fin <- db[which(!is.na(db$xrate)),]

print('Write data')
write.csv(db_fin, 'Tables/ready_data.csv', row.names=F)

rates_table<- data.frame()
png(file='xrate.png',width=1500,height=800)
par(mfrow=c(2,4))
for(j in 1:length(currencies$db_pair)){  
  xrate = eval(parse(text=paste('rates$', currencies$from_ccy[j], currencies$to_ccy[j], sep="")))
  rates_table <- cbind(rates_table, xrate)
  xrate <- as.data.frame(xrate)
  xrate$date=as.Date(rownames(xrate), '%Y-%m-%d')
  start_date <- as.Date(Sys.time())-60
  xrate <- xrate[which(xrate$date>=start_date),]
  ?plot
  #ggplot(xrate, aes(date, xrate[,1]))+geom_line()+ggtitle(names(xrate)[1])+ylab(names(xrate)[1])
  plot(xrate[,2], xrate[,1], type='l', xlab='Date', ylab=names(xrate)[1], main=names(xrate)[1])
  #ggsave(filename=paste('plots/', names(xrate)[1], '.png', sep=''), width=5, height=2.5)
}
dev.off()

write.csv(rates_table, 'Tables/rates_table.csv')
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(db[which(db$start_date>=start_date),], aes(first_ccy_pair, new_users, group=week, fill=week))+geom_bar(stat='identity', position='dodge')+
  scale_fill_brewer(palette="Spectral")
ggsave(filename=paste('plots/new_users.png', sep=''), width=10, height=5)


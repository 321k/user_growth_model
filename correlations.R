
# Set-up ------------------------------------------------------------------


library(Kmisc)
library(RMySQL)
library(data.table)
library(quantmod)
library(plyr)
library(dplyr)
library(ggplot2)
library(RCurl)
library(reshape2)
library(directlabels)


f <- getURL('https://raw.githubusercontent.com/321k/R-Helper-Functions/master/general%20helper%20functions')
eval(parse(text=f))
# ssh stats@178.162.202.36 -L 3307:172.18.1.43:3306

con <- dbConnect(MySQL(),
                 user = 'stats',
                 password = 'Kalamaja123',
                 host = '127.0.0.1',
                 port = 3307,
                 dbname='obfuscated')

query_users <- "
select 
  date_format(first_payment_attempt, '%x-%v')  as week
  ,date_format(first_payment_attempt, '%Y-%m-%d') as date
  ,case when 
    concat(ccy_send, ' > ', ccy_target) in ('GBP > EUR','EUR > GBP','GBP > INR','EUR > INR','GBP > HUF','GBP > USD','EUR > USD', 'USD > INR', 'GBP > AUD')
  then concat(ccy_send, ' > ', ccy_target)
  else 'other'
  end as ccy_pair
  ,sum(scaled_user_count) as growth
from report_attribution_metadata uc
group by 1,3
"

db_users <- dbGetQuery(con, query_users)

query_users_daily <- "
select 
  date_format(first_payment_attempt, '%Y-%m-%d') as date
  ,case when 
    concat(ccy_send, ' > ', ccy_target) in ('GBP > EUR','EUR > GBP','GBP > INR','EUR > INR','GBP > HUF','GBP > USD','EUR > USD', 'USD > INR', 'GBP > AUD')
  then concat(ccy_send, ' > ', ccy_target)
  else 'other'
  end as ccy_pair
  ,sum(scaled_user_count) as growth
from report_attribution_metadata uc
group by 1,2
"

db_users_daily <- dbGetQuery(con, query_users_daily)


query_transfers <- "
select
date_request_submitted_week as week
,date_format(date_request_submitted, '%Y-%m-%d') as date
,case when 
    concat(ccy_send, ' > ', ccy_target) in ('GBP > EUR','EUR > GBP','GBP > INR','EUR > INR','GBP > HUF','GBP > USD','EUR > USD', 'USD > INR', 'GBP > AUD')
then concat(ccy_send, ' > ', ccy_target)
end as ccy_pair
,sum(flag_transferred) as growth
from report_req_metadata
group by 1,3
"
db_transfers <- dbGetQuery(con, query_transfers)

query_transfers_daily <- "
select
date_format(date_request_submitted, '%Y-%m-%d') as date
,case when 
concat(ccy_send, ' > ', ccy_target) in ('GBP > EUR','EUR > GBP','GBP > INR','EUR > INR','GBP > HUF','GBP > USD','EUR > USD', 'USD > INR', 'GBP > AUD')
then concat(ccy_send, ' > ', ccy_target)
end as ccy_pair
,sum(flag_transferred) as growth
from report_req_metadata
group by 1,2
"
db_transfers_daily <- dbGetQuery(con, query_transfers_daily)


# List of currencies
from_ccy <- c('GBP', 'EUR', 'GBP', 'EUR', 'GBP', 'GBP', 'EUR', 'USD', 'GBP')
to_ccy <- c('EUR', 'GBP', 'INR', 'INR', 'HUF', 'USD', 'USD', 'INR', 'AUD')
currencies <- data.frame(from_ccy, to_ccy)
currencies$db_pair <- paste(from_ccy, to_ccy, sep=' > ')
currencies$qm_pair <- paste(from_ccy, to_ccy, sep='/')
db_pair <- paste("'", currencies$db_pair, "'", sep="", collapse = ',')

# Get exchange rates
rates <- new.env()
getFX(currencies$qm_pair, from = '2011-04-01', to = Sys.Date(), env = rates)

# Run correlation test ----------------------------------------------------


db <- db_users
#db <- db_transfers
db$date <- as.Date(as.character(db$date), '%Y-%m-%d')
db$start_date <- as.Date(as.character(db$date), '%Y-%m-%d')
db$end_date <- db$start_date + 6
db$growth <- as.numeric(as.character(db$growth))
db_pair <- unique(db$ccy_pair)

# Calculate % change in user numbers
db <- ddply(db, "ccy_pair", transform,  DeltaCol = Delt(growth, type='arithmetic'))
names(db)[ncol(db)] <- 'd_growth'


# Add exchange rates to table
db$xrate <- NA
db$max_rate <- NA
db$min_rate <- NA

for(j in 1:length(currencies$db_pair)){
  r <- which(db$ccy_pair == currencies$db_pair[j])
  xrate = as.data.frame(
    eval(parse(text=paste('rates$', currencies$from_ccy[j], currencies$to_ccy[j], sep="")))
  )
  names(xrate) <- 'rate'
  xrate$date=as.Date(rownames(xrate), '%Y-%m-%d')
  
  for(i in r){
    weekspan <- xrate[which(xrate$date>=db$start_date[i] & xrate$date<=db$end_date[i]),]
    if(nrow(weekspan)==0) next()
    db$xrate[i] <- mean(weekspan$rate)
    db$max_rate[i] <- max(weekspan$rate)
    db$min_rate[i] <- min(weekspan$rate)
  }
}

# Calculate other exchange rate measures
db <- ddply(db, "ccy_pair", transform,  DeltaCol = Delt(xrate, type='arithmetic'))
names(db)[ncol(db)] <- 'd_xrate'

# Calcualte lagged values
db$growth_lag1 <- NA
db$d_growth_lag1 <- NA
db$d_xrate_lag1 <- NA

for(j in 1:length(currencies$db_pair)){  
  r <- which(db$ccy_pair == currencies$db_pair[j])
  n <- length(db$d_growth[r])
  db$growth_lag1[r] <- c(0,db$growth[r][-n])
  db$d_growth_lag1[r] <- c(0,db$d_growth[r][-n])
  db$d_xrate_lag1[r] <- c(0,db$d_xrate[r][-n])
}


db$estimate <- NA
db$se <- NA
db$tvalue <- NA
db$pvalue <- NA
db$fitted <- NA
db$theorethical_growth <- NA
for(j in 1:length(currencies$db_pair)){
  cor_period = 20
  r <- which(db$ccy_pair == currencies$db_pair[j])
  db_subset <- db[r,]
  for(i in cor_period:nrow(db_subset)){
    if(length(which(is.na(db_subset$d_xrate[(i-cor_period):i])))>0) next()
    if(length(which(is.infinite(db_subset$d_growth[(i-cor_period):i])))>0) next()
    fit <- lm(d_growth~d_xrate, data=db_subset[(i-cor_period):i,])
    if( is.null(fit$coefficients) | is.na(fit$coefficients[2])) next()
    db_subset$estimate[i] <- summary(fit)[[4]][2,1]
    db_subset$se[i] <- summary(fit)[[4]][2,2]
    db_subset$tvalue[i] <- summary(fit)[[4]][2,3]
    db_subset$pvalue[i] <- summary(fit)[[4]][2,4]
    db_subset$fitted[i] <- fit %>% fitted %>% last
  }
  db[r,] <- db_subset
}

db$theorethical_growth <- db$fitted * db$growth_lag1
r <- which(db$date>='2015-01-01' & db$ccy_pair != 'other' & db$date<'2016-01-04')
db$alpha <- 1-db$pvalue
db$alpha[which(db$alpha<0.80)] <- 0.6

ggplot(db[r,], aes(date, estimate, color=ccy_pair, alpha=alpha)) + geom_line() +ggtitle(paste("Correlation for the past", cor_period, "periods")) + ylab('correlation')
ggplot(db[r,], aes(date, theorethical_growth, color=ccy_pair, alpha=alpha)) + geom_line() +ggtitle(paste("Correlation for the past", cor_period, "periods"))
ggplot(db[r,], aes(date, xrate, color=ccy_pair))+geom_line()

tmp <- db %>% 
  filter(date >= '2015-01-01',
         ccy_pair != 'other') %>% 
  mutate(month = as.numeric(format(date, "%m"))) %>% 
  select(month, ccy_pair, theorethical_growth) %>% 
  group_by(month, ccy_pair) %>%
  summarise(sum(theorethical_growth))

sqldf("select month, ccy_pair, sum(theorethical_growth) as sum from tmp group by 1, 2")

r <- which(db$ccy_pair == currencies$db_pair[4])
p1 <- ggplot(db[r,], aes(date, theorethical_growth, color=ccy_pair)) + geom_line() +ggtitle("Theorethical growth due to exchange rate")
p2 <- ggplot(db[r,], aes(date, estimate, color=pvalue)) + geom_line() +ggtitle("Correlation over past 20 weeks. \n Darker color=more significant correlation.")
multiplot(p1,p2)

p <- list()
for(i in 1:nrow(currencies)){
  r <- which(db$ccy_pair == currencies$db_pair[i] & db$date>='2015-01-01')
  p[[i]] <- ggplot(db[r, c('date', 'xrate')], aes(date, xrate)) + geom_line() +ylab(currencies$db_pair[i])
}
multiplot(p[[1]], p[[2]], p[[3]], p[[4]])
multiplot(p[[5]], p[[6]], p[[7]], p[[8]])


p <- list()
for(i in 1:nrow(currencies)){
  r <- which(db$ccy_pair == currencies$db_pair[i] & db$date>='2015-01-01')
  
  p[[i]] <- ggplot(db[r, c('date', 'xrate')], aes(date, xrate)) + geom_line() +ylab(currencies$db_pair[i])
}
multiplot(p[[1]], p[[2]], p[[3]], p[[4]])
multiplot(p[[5]], p[[6]], p[[7]], p[[8]])

tmp_corridor_growth_and_exchange_rates <- db[c('week', 'date', 'ccy_pair', 'growth', 'd_growth', 'xrate', 'd_xrate', 'estimate', 'se', 'tvalue', 'pvalue')]
dbWriteTable(con, name='tmp_corridor_growth_and_exchange_rates', value = tmp_corridor_growth_and_exchange_rates, row.names=F, overwrite=T)

# Daily correlation -------------------------------------------------------
c('db_transfers_dialy', 'db_users_daily')
db <- db_users_daily
db$date <- as.Date(db$date, '%Y-%m-%d')
xrates <-as.data.frame(matrix(NA, 0, 3)) %>% setnames(names(.), c('rate', 'date', 'ccy_pair'))
# Add exchange rates to table

for(j in 1:length(currencies$db_pair)){
  r <- which(db$ccy_pair == currencies$db_pair[j])
  xrate = as.data.frame(
    eval(parse(text=paste('rates$', currencies$from_ccy[j], currencies$to_ccy[j], sep="")))
  )
  names(xrate) <- 'rate'
  xrate$date=as.Date(rownames(xrate), '%Y-%m-%d')
  xrate$ccy_pair <- currencies$db_pair[j]
  xrates <- rbind(xrates, xrate)
}

db <- db %>% merge(xrates, by=c('date', 'ccy_pair')) 

db$weekdays <- as.factor(weekdays(db$date))
db <- db %>% filter(weekdays != 'Sunday' & weekdays !='Saturday')

db <- ddply(db, "ccy_pair", transform,  DeltaCol = Delt(rate, type='arithmetic'))
names(db)[ncol(db)] <- 'd_rate'

db <- ddply(db, "ccy_pair", transform,  DeltaCol = Delt(growth, type='arithmetic'))
names(db)[ncol(db)] <- 'd_growth'


# Remove weekday effect (all time) and store residuals
prepare_for_lm <- function(y,x){
  r <- which(is.finite(y))
  fit <- lm(y[r]~x[r])
  
  res <- rep(NA, length(y))
  res[r] <- fit$residuals
  return(res)
}
fit <- lm(d_growth_resid~weekdays, data=db[r,])
summary(fit)
db <- ddply(db, "ccy_pair", transform, d_growth_resid = prepare_for_lm(d_growth, weekdays))

# Calcualte lagged values
lagged <- function(x){
  n <- length(x)
  x <- c(NA,x[-n])
  return(x)
}

db <- ddply(db, "ccy_pair", transform, d_growth_resid_lag_1 = lagged(d_growth_resid))
db <- ddply(db, "ccy_pair", transform, d_growth_resid_lag_2 = lagged(d_growth_resid_lag_1))
db <- ddply(db, "ccy_pair", transform, d_rate_lag_1 = lagged(d_rate))
db <- ddply(db, "ccy_pair", transform, d_rate_lag_2 = lagged(d_rate_lag_1))
db <- ddply(db, "ccy_pair", transform, growth_lag_1 = lagged(growth))

# Plot a given corridor
r <- which(db$ccy_pair=='GBP > INR' & !is.na(db$rate) & db$date > '2014-01-01')
plot(d_growth_resid~weekdays, data=db[r,])
lm(d_growth_resid~d_rate + d_rate_lag_1, data=db[r,]) %>% summary
plot(d_growth_resid~d_rate + d_rate_lag_1, data=db[r,])

fit <- lm(d_growth_resid~d_rate, data=db[r,])
abline(fit, col='red')


# Rolling regression
db$estimate <- NA
db$se <- NA
db$tvalue <- NA
db$pvalue <- NA
db$fitted <- NA
db$theorethical_growth <- NA
for(j in 1:length(currencies$db_pair)){
  cor_period = 60
  r <- which(db$ccy_pair == currencies$db_pair[j])
  db_subset <- db[r,]
  for(i in cor_period:nrow(db_subset)){
    if(length(which(is.na(db_subset$d_rate[(i-cor_period):i])))>0) next()
    if(length(which(is.infinite(db_subset$d_growth[(i-cor_period):i])))>0) next()
    fit <- lm(d_growth_resid~d_rate+d_rate_lag_1, data=db_subset[(i-cor_period):i,])
    if( is.null(fit$coefficients) | is.na(fit$coefficients[2])) next()
    db_subset$estimate[i] <- summary(fit)[[4]][2,1]
    db_subset$se[i] <- summary(fit)[[4]][2,2]
    db_subset$tvalue[i] <- summary(fit)[[4]][2,3]
    db_subset$pvalue[i] <- summary(fit)[[4]][2,4]
    db_subset$fitted[i] <- fit %>% fitted %>% last
  }
  db[r,] <- db_subset
}

# Add month, theorethical new users or transfers and transparency value for plot
db$yearmon <- as.yearmon(db$date)
db$theorethical_growth <- db$fitted * db$growth_lag_1
db$alpha <- 1-db$pvalue
db$alpha[which(db$alpha<0.80)] <- 0.6

r <- which(db$date>='2015-01-01' & db$ccy_pair != 'other' & db$date<'2016-01-04')

# Plot strength of correlation
ggplot(db[r,], aes(date, estimate, color=ccy_pair, alpha=alpha)) + geom_line() +ggtitle(paste("Correlation for the past", cor_period, "days (weekends removed)")) + ylab('Daily correlation (growth and rate)')

# Plot theorethical new users
ggplot(db[r,], aes(date, theorethical_growth, color=ccy_pair, alpha=alpha)) + geom_line() +ggtitle(paste("Correlation for the past", cor_period, "periods"))

db %>% 
  filter(yearmon >='2015-10') %>% 
  group_by(yearmon, ccy_pair) %>% 
  summarise(sum(theorethical_growth)) %>% 
  dcast(ccy_pair~yearmon)

db %>% 
  filter(yearmon >='2015-10') %>% 
  group_by(yearmon, ccy_pair) %>% 
  summarise((prod(1+fitted))-1) %>% 
  dcast(ccy_pair~yearmon)

db %>% 
  filter(yearmon >='2015-10') %>% 
  group_by(yearmon, ccy_pair) %>% 
  summarise((prod(1+d_growth))-1) %>% 
  dcast(ccy_pair~yearmon)

db %>% 
  select(yearmon, ccy_pair, rate) %>% 
  filter(yearmon>='2015-11-01', yearmon<'2016-01-01') %>% 
  group_by(yearmon, ccy_pair) %>% 
  summarise(rate = mean(rate)) %>% 
  dcast(ccy_pair~yearmon) %>%
  setnames(names(.), c('ccy_pair', 'Nov_2015', 'Dec_2015')) %>% 
  mutate(MoM_change = paste(round(100*(Dec_2015/Nov_2015-1),2), '%', sep='')) %>% 
  write.cb



# Using random effects model
# http://www.r-bloggers.com/random-regression-coefficients-using-lme4/
# http://www.bodowinter.com/tutorial/bw_LME_tutorial.pdf
setwd('/Users/transferwise/user_growth_model')

library(lme4)
library(nlme)
library(lmerTest)
library(lubridate)
library(ISOweek)
library(forecast)
source('firstDayOfWeek.R')

currencies <- read.csv('currencies.csv')

db <- read.csv('Tables/ready_data.csv')
db$date <- as.Date(db$date, '%Y-%m-%d')
db$start_date <- as.Date(db$start_date)
db$first_ccy_pair <- as.character(db$first_ccy_pair)
last_week_start <- firstDayOfWeek()
db <- db[which(db$date >= '2014-06-01' & db$date < last_week_start),]

#fit <- lmer(d_new_users~d_new_users_1+d_new_users_2+end_of_month+(1+d_xrate|first_ccy_pair), data=db)
fit <- lmer(d_new_users~d_new_users_1+d_new_users_2+end_of_month+(1+d_xrate+d_xrate_1|first_ccy_pair), data=db) # Best fit
#fit <- lmer(d_new_users~end_of_month+(1+d_xrate+d_xrate_1|first_ccy_pair), data=db)
#fit <- lmer(d_new_users~d_new_users_1+end_of_month+(1+d_xrate+d_xrate_1|first_ccy_pair), data=db)
#fit <- lmer(d_new_users~d_new_users_1+end_of_month+(1+max_d_xrate|first_ccy_pair), data=db)
#fit <- lmer(d_new_users~d_new_users_1+end_of_month+(1+max_d_xrate+d_xrate_1|first_ccy_pair), data=db)
#fit <- lmer(d_new_users~d_new_users_1+d_xrate+end_of_month+(1+max_d_xrate+d_xrate_1|first_ccy_pair), data=db)
#fit <- lmer(d_new_users~d_new_users_1+d_xrate+end_of_month+(1+d_xrate|first_ccy_pair), data=db)
#fit <- lmer(arima_resid~ln_xrate+end_of_month+(1|first_ccy_pair), data=db) # Best fit
db$lme4_residuals <- residuals(fit)
db$lme4_fitted <- fitted(fit)

db$'Expected new users' <- round(db$'new_users_1' * (1+db$'lme4_fitted'), 0)
db$'Over/under' <- db$'new_users' / db$'Expected new users' - 1

#############
# OLS model on ARIMA residuals
#############

# Add arima residuals
ext_reg <- c('d_xrate_1')
db$d_new_users[which(is.na(db$d_new_users))] <- 0
db$arima_resid <- NA
db$arima_fitted <- NA
for(i in 1:length(currencies$db_pair)){
  r <- which(db$first_ccy_pair == currencies$db_pair[i])
  n <- length(db$d_new_users[r])
  fit <- Arima(db$d_new_users[r], c(3,1,0), xreg= db[r,ext_reg])
  #fit <- Arima(db$d_new_users[r], c(0,0,1))
  db$arima_resid[r] <- fit$residuals
  db$arima_fitted[r] <- fitted(fit)
}
plot(forecast(fit))
plot(fit)
xreg=db[r,'ext_reg']

db$'Expected new users' <- round(db$'new_users_1' * (1+db$'arima_fitted'), 0)
db$'Over/under' <- db$'new_users' / db$'Expected new users' - 1


write.csv(db, 'Tables/random effects model.csv')

##################
# Table with results
##################

include <- c('start_date','first_ccy_pair', 'd_xrate', 'd_new_users', 'lme4_fitted', 'new_users_1', 'new_users')
date <- unique(db$start_date)
date <- date[order(date)]
n <- length(date)
week_steps=0
this_week = date[n-week_steps]
previous_week = date[n-1-week_steps]

res_table <- db[include]
res_table <- res_table[which(res_table$start_date == this_week),]
names(res_table) <- c('Date', 'Corridor', 'Exchange rate', 'Realized growth', 'Expected growth', 'Realized WNU last week', 'Realized WNU this week')
res_table$'Expected new users' <- round(res_table$'Realized WNU last week' * (1+res_table$'Expected growth'), 0)
res_table$'Over/under' <- res_table$'Realized WNU this week' / res_table$'Expected new users' - 1
res_table
write.csv(res_table, paste('Tables/results ', this_week, '.csv', sep=""), row.names=F)

##################
# Plot
##################
corridor_rows <- which(db$first_ccy_pair == 'USD > INR')
plot_data <- db[corridor_rows,]
plot(plot_data$d_xrate, plot_data$d_new_users)
fit <- lm(d_new_users~d_xrate+d_new_users_1+end_of_month, data=plot_data)
length(fit$residuals)

#summary(fit)
#plot(fit$fitted, plot_data$d_new_users)
par(mfrow=c(2,1))
plot(d_xrate~date, data=plot_data, type='l')
plot(d_new_users~date, data=plot_data, type='l')
par(mfrow=c(1,1))

fit <- lm(arima_fitted~d_xrate_1,data=db[r,])

plot(db[r,c('d_new_users','arima_fitted')])
plot(db[r,'d_new_users'], type='l')
lines(db[r,'arima_fitted'], type='l', col='red')

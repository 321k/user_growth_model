# test model, VAR
library(vars)
library(quantmod)
library(plyr)
library(lubridate)

setwd('/Users/transferwise/user_growth_model/Other models')

db <-  read.csv('ready_data.csv')
# OLS regression
db <- db[which(db$start_date >= '2014-01-01'),]
fit <- lm(d_WNU_GBPINR ~ d_WNU+d_avg_rate+lag1_d_WNU_GBPINR+lag1_d_WNU+end_of_month, data=db)
summary(fit)
db$fitted <- fit$fitted
plot(db$d_WNU_GBPINR, db$d_avg_rate)
hist(fit$residuals)
jarque.bera.test(fit$residual)
# VAR model
variables <- c('d_WNU', 'd_WNU_GBPINR', 'd_avg_rate')
fit1 <- VAR(db[variables], p=1)
fit2 <- VAR(db[variables], p=2)

summary(fit1)
db$end_of_month


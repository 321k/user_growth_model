library(forecast)

setwd('/Users/transferwise/user_growth_model/Other models')
db <-  read.csv('ready_data.csv')
names(db)
ext_reg <- c('d_WNU', 'lag1_d_WNU', 'd_avg_rate')

fit <- arima(db$d_WNU_GBPINR, c(1,0,1), xreg = db[ext_reg])
db$residuals <- fit$residuals
db$fitted <- fitted(fit)

hist(fit$residuals)


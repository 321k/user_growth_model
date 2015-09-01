library(forecast)

setwd('/Users/transferwise/user_growth_model/Other models')
db <-  read.csv('ready_data.csv')
db <- db[-c(1:3),]
db$start_date <- as.Date(as.character(db$start_date))
include <- c('start_date', 'd_WNU_GBPINR')

acf(db['d_WNU_GBPINR'])
pacf(db['d_WNU_GBPINR'])

ext_reg <- c('d_WNU', 'lag1_d_WNU', 'd_avg_rate')

fit <- Arima(db$d_WNU_GBPINR, c(3,1,0))
db$residuals <- as.numeric(fit$residuals)
jarque.bera.test(fit$residuals)
hist(fit$residuals)
plot(db$d_avg_rate, db$residuals)
plot(db$start_date, db$residuals, type='l')
plot(db$avg_rate)
fit <- lm(db$residuals~db$d_avg_rate)
abline(fit)
summary(fit)



setwd('/Users/transferwise/user_growth_model/')
db <- read.csv('Tables/ready_data.csv')
db$date <- as.Date(db$date, '%Y-%m-%d')
db$start_date <- as.Date(db$start_date)
db$first_ccy_pair <- as.character(db$first_ccy_pair)
db <- db[which(db$date >= '2014-01-01'),]
i=1
r <- which(db$first_ccy_pair == currencies$db_pair[i])
r <- r[-length(r)]
db <- db[r,]

d_new_users <- ts(db$d_new_users)
d_xrate <- ts(db$d_xrate)
d_xrate_1 <- ts(db$d_xrate_1)
test_start <- 30
test_end <- length(d_xrate)
training <- window(d_new_users, end=(test_start-1))
test <- window(d_new_users, start=test_start)
fit <- Arima(training, c(3,1,0), xreg = d_xrate[1:(test_start-1)])
fc <- forecast(fit, h=(test_end-test_start+1), xreg=d_xrate[test_start:test_end])
plot(fc)
lines(test, col='red')

n <- length(test)
#fc <- ts(numeric(n), start=test_start)
for(i in 1:n){
  h=1
  x <- window(d_new_users, end=(test_start-1+i))
  refit <- Arima(x, model=fit, xreg = d_xrate[1:(test_start-1+i)])
  refc <- forecast(refit, h=h,xreg=d_xrate[(test_start+i):test_end])
  fc$mean[i] <- refc$mean[h]
  #fc$lower[i,] <- refc$lower[h,]
  #fc$upper[i,] <- refc$upper[h,]
}
i=1
refc$upper[1,]
fc$upper[,]
plot(fc)
lines(test, col='red')

names(fc)

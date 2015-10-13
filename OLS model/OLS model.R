# OLS model

setwd('/Users/transferwise/user_growth_model')

library(lubridate)
library(ISOweek)
library(forecast)
library(sqldf)
source('firstDayOfWeek.R')
library(tseries)
library(lmSupport)

jbtest <- function(fit, name='Histogram'){
  data <- residuals(fit)[which(!is.na(residuals(fit)))]
  res <- jarque.bera.test(data)
  hist(data, main = name)
  return(res)
}

currencies <- read.csv('currencies.csv')

db <- read.csv('Tables/ready_data.csv')
db$date <- as.Date(db$date, '%Y-%m-%d')
db$start_date <- as.Date(db$start_date)
db$first_ccy_pair <- as.character(db$first_ccy_pair)
last_week_start <- firstDayOfWeek()
db <- db[which(db$date >= '2015-01-01' & db$date < last_week_start),]

fit1 <- fit <- fit_reduced <- fit1_reduced <- list()
db$fitted <- NA
for(i in 1:nrow(currencies)){
  r <- which(db$first_ccy_pair == currencies$db_pair[i])
  fit[[i]] <- lm(d_new_users ~ d_xrate + d_new_users_1 + d_new_users_2 +d_total_new_users_excl_own, data=db[r,])
  fit1[[i]] <- lm(d_new_users ~ d_xrate + d_new_users_1 + d_new_users_2 + d_total_new_users_1, data=db[r,])
  fit_reduced[[i]] <- lm(d_new_users ~ d_new_users_1 + d_new_users_2 +d_total_new_users_excl_own, data=db[r,])
  fit1_reduced[[i]] <- lm(d_new_users ~ d_new_users_1 + d_new_users_2 + d_total_new_users_1, data=db[r,])
  db$fitted[r] <- fit[[i]]$residuals
}

for(i in 1:length(fit)){
  print(as.character(currencies$db_pair[i]))
  print(modelCompare(fit_reduced[[i]], fit[[i]])$DeltaR2)
  print("------")
}
?modelCompare

# Tests
i=0
i=i+1
print(as.character(currencies$db_pair[i]))
vif(fit[[i]]) # Variance inflation factor under 10, multicollinearity not a problem
qqPlot(fit[[i]], main="QQ Plot") # Normal distribution



jbtest(fit[[i]])
jbtest(fit_reduced[[i]])



for(i in 1:length(fit)){
  print(as.character(currencies$db_pair[i]))
  #print(currencies$db_pair[i]
  #print(coefficients(fit[[i]]))
  print(summary(fit[[i]]))
  #print(anova(fit1[[i]], fit1_reduced[[i]]))
  #outlierTest(fit[[i]])
  print("------")
}

modelCompare(fit_reduced[[i]], fit[[i]])$DeltaR2

include <- c('start_date','first_ccy_pair', 'd_xrate', 'd_new_users', 'fitted', 'new_users_1', 'new_users')
db <- db[include]
db$'Expected new users' <- round(db$'new_users_1' * (1+db$'fitted'), 0)
r <- which(db$first_ccy_pair=='other')
db$'Expected new users'[r] <- db$new_users[r]
db$'Over/under' <- db$'new_users' / db$'Expected new users' - 1

write.csv(db, "OLS model/OLS_model.csv")

par(mfrow=c(2,1))
i=0
i=i+1
r <- which(db$first_ccy_pair == currencies$db_pair[i])
x <- db[r,]
summary(fit[[i]])
plot(x$d_new_users, type='l', main=currencies$db_pair[i])
lines(x$fitted, col='red')
#lines(x$d_total_new_users_excl_own, col='blue')
plot(x$d_xrate, type='l')
#plot(x$d_xrate, x$d_new_users)
#abline(lm(x$d_new_users~x$d_xrate))
if(i==nrow(currencies))
  i=0


# Using random effects model
# http://www.r-bloggers.com/random-regression-coefficients-using-lme4/
# http://www.bodowinter.com/tutorial/bw_LME_tutorial.pdf

library(lme4)
library(nlme)
setwd('/Users/transferwise/Google\ Drive/Research/User\ growth\ model')
db <- read.csv('Tables/ready_data.csv')
db$date <- as.Date(db$date, '%Y-%m-%d')
db <- db[-c(1:3)]
db <- db[which(db$date >= '2014-01-01'),]
db$first_ccy_pair <- as.character(db$first_ccy_pair)

fit <- lmer(d_new_users~d_new_users_1+d_new_users_2+end_of_month+(1+d_xrate|first_ccy_pair), data=db)

db$lme4_residuals <- residuals(fit)
db$lme4_fitted <- fitted(fit)

write.csv(db, 'Tables/random effects model.csv')

date <- unique(db$start_date)
date <- date[order(date)]
n <- length(date)
names(db)
res_table <- db[which(db$start_date == date[n]),c(1,2,3,7,11,26)]
res_table <- cbind(res_table, db[which(db$start_date == date[n-1]),3])
names(res_table) <- c('Date', 'Corridor', 'Realized WNU this week', 'Realized growth', 'Exchange rate', 'Expected growth', 'Realized WNU last week')
res_table <- res_table[c(1,2,5,6,7,3)]
res_table$'Expected new users' <- round(res_table$'Realized WNU last week' * (1+res_table$'Expected growth'), 0)
res_table$'Over/under' <- round(res_table$'Realized WNU this week' - res_table$'Expected new users', 0)

#write.csv(res_table, 'Tables/results.csv', row.names=F)


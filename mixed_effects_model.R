# Using random effects model
# http://www.r-bloggers.com/random-regression-coefficients-using-lme4/
# http://www.bodowinter.com/tutorial/bw_LME_tutorial.pdf

library(lme4)
library(nlme)
library(lmerTest)

setwd('/Users/transferwise/user_growth_model')
db <- read.csv('Tables/ready_data.csv')
db$date <- as.Date(db$date, '%Y-%m-%d')
db$start_date <- as.Date(db$start_date)

# Remove USD > INR
#db <- db[-which(db$first_ccy_pair == 'USD > INR'),]

db <- db[which(db$date >= '2015-01-01'),]
db$first_ccy_pair <- as.character(db$first_ccy_pair)

fit <- lmer(d_new_users~d_new_users_1+d_new_users_2+end_of_month+(1+d_xrate|first_ccy_pair), data=db)

db$lme4_residuals <- residuals(fit)
db$lme4_fitted <- fitted(fit)

write.csv(db, 'Tables/random effects model.csv')

include <- c('start_date', 'first_ccy_pair', 'new_users', 'd_new_users', 'd_xrate', 'lme4_fitted')
date <- unique(db$start_date)
date <- date[order(date)]
n <- length(date)
week_steps=3
this_week = date[n-week_steps]
previous_week = date[n-1-week_steps]

res_table <- db[include]
res_table <- res_table[which(res_table$start_date == this_week),]
prev_WNU <- db[which(db$start_date == previous_week),c('first_ccy_pair', 'new_users')]
names(prev_WNU) <- c('first_ccy_pair', 'Realized WNU last week')
res_table <- merge(res_table, prev_WNU)
names(res_table) <- c('Corridor', 'Date', 'Realized WNU this week', 'Realized growth', 'Exchange rate', 'Expected growth', 'Realized WNU last week')
final_order <- c('Date', 'Corridor', 'Exchange rate', 'Realized growth', 'Expected growth', 'Realized WNU last week', 'Realized WNU this week')
res_table <- res_table[final_order]
res_table$'Expected new users' <- round(res_table$'Realized WNU last week' * (1+res_table$'Expected growth'), 0)
res_table$'Over/under' <- round(res_table$'Realized WNU this week' - res_table$'Expected new users', 0)

write.csv(res_table, paste('Tables/results ', this_week, '.csv', sep=""), row.names=F)

###############
# Plot
corridor_rows <- which(db$first_ccy_pair == 'USD > INR')
plot_data <- db[corridor_rows,]
plot(plot_data$d_xrate, plot_data$d_new_users)
fit <- lm(d_new_users~d_xrate+d_new_users_1+end_of_month, data=plot_data)
length(fit$residuals)

summary(fit)
par(mfrow=c(1,1))
plot(fit$fitted, plot_data$d_new_users)
plot(d_xrate~date, data=plot_data, type='l')
plot(d_new_users~date, data=plot_data, type='l')

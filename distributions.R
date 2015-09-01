x<-db[which(db$first_ccy_pair == 'USD > INR'),]
y<-db[-which(db$first_ccy_pair %in% c('USD > INR','EUR > INR', 'GBP > INR')),]
z<-db[which(db$first_ccy_pair %in% c('USD > INR','EUR > INR', 'GBP > INR')),]

ggplot(x, aes(lme4_residuals))+geom_density(alpha=0.2)
ggplot(db, aes(lme4_residuals, fill=first_ccy_pair))+geom_density(alpha=0.2)
ggplot(db, aes(lme4_residuals, fill=first_ccy_pair))+geom_density(alpha=0.2)
ggplot(y, aes(lme4_residuals, fill=first_ccy_pair))+geom_density(alpha=0.2)
ggplot(z, aes(lme4_residuals, fill=first_ccy_pair))+geom_density(alpha=0.2)

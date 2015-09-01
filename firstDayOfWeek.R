library(ISOweek)
firstDayOfWeek <- function(date=Sys.time()){
  x <- as.Date(date, '%Y-%m-%d', tz='UTC')
  x <- date2ISOweek(x)
  x <- paste(substr(x, 1, nchar(x)-1), '1', sep='')
  x <- ISOweek2date(x)
  return(x)
}

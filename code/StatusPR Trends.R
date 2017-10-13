rm(list=ls())

library(data.table)
library(dplyr)
library(lubridate)
library(stringr)

status = fread('data/StatusPR.csv') %>%
  mutate(
    yr = '2017',
    date=ymd(paste(yr, Month, Day, sep='-'))) %>%
  arrange(date)
dim(status)

### plot function
plot.trend = function(dt, col) {
  polygon(c(dt[[1]][1], dt[[1]], dt[[1]][length(dt[[1]])]),
    c(0, dt[[2]], 0), border=NA, col=adjustcolor(col, 0.7))
  lines(dt[[1]], dt[[2]], col=col)
}

### people in shelters
shelter = filter(status, Resource == 'People in shelters')

x.dates = seq(as.Date('2017-09-21'), max(status$date)+3, by='weeks')

plot.shelters = function() {
png('figs/In_Shelters.png', 5, 3, units='in', res=360)
par(mar=c(2, 3.5, 0.5, 0.5), cex=0.8)
plot(x.dates, rep(1, length(x.dates)),
  ylim=c(0, 12000),
  type='n', 
  axes=F, xlab='', ylab='')
abline(h=seq(0, 12000, by=2000), col=adjustcolor('darkgrey', 0.3))
abline(v=x.dates, col=adjustcolor('darkgrey', 0.3))
plot.trend(select(shelter, date, Value), col='darkblue')
lines(rep(ymd('2017-09-21'), 2), c(0, 8000), lwd=3, col='darkred')
text(ymd('2017-09-21'), 8000, 'Maria Landfall', cex=0.75, pos=4, col='darkred')
axis.Date(1, at=x.dates, format="%b %d", col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex=0.8)
axis(2, at=seq(0, 12000, by=2000), las=1, col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex=0.8)
mtext('Number of people in shelters', 2, line=2.5, col='grey40', cex=0.8)
dev.off()
}
plot.shelters()

  

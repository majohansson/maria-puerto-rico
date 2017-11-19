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

x.dates = seq(as.Date('2017-09-21'), max(status$date)+5, by='weeks')

### plot function
plot.trend = function(dt, col) {
  polygon(c(dt[[1]][1], dt[[1]], dt[[1]][length(dt[[1]])]),
    c(0, dt[[2]], 0), border=NA, col=adjustcolor(col, 0.7))
  lines(dt[[1]], dt[[2]], col=col)
}

### people in shelters
plot.shelters = function() {
png('figs/In_Shelters.png', 5, 3, units='in', res=360)
par(mar=c(2, 3.5, 0.5, 0.5), cex=0.8)
plot(x.dates, rep(1, length(x.dates)),
  ylim=c(0, 12000),
  type='n', 
  axes=F, xlab='', ylab='')
abline(h=seq(0, 12000, by=2000), col=adjustcolor('darkgrey', 0.3))
abline(v=x.dates, col=adjustcolor('darkgrey', 0.3))
plot.trend(filter(status, Resource == 'People in shelters') %>%
    select(date, Value), col='darkblue')
arrows(ymd('2017-09-21'), 8000, ymd('2017-09-21'), 0, length=0.05)
text(ymd('2017-09-25'), 8300, 'Landfall - Maria', cex=0.75)
axis.Date(1, at=x.dates, format="%b %d", col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex=0.8)
axis(2, at=seq(0, 12000, by=2000), las=1, col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex=0.8)
mtext('Number of people in shelters', 2, line=2.5, col='grey40', cex=0.8)
dev.off()
}
plot.shelters()

### hospitals with electricity
plot.hosp = function() {
png('figs/Hospitals.png', 5, 3, units='in', res=360)
par(mar=c(2, 3.5, 0.5, 0.5), cex=0.8)
plot(x.dates, rep(1, length(x.dates)),
  ylim=c(0, 75),
  type='n', 
  axes=F, xlab='', ylab='')
abline(h=seq(0, 70, by=10), col=adjustcolor('darkgrey', 0.3))
abline(v=x.dates, col=adjustcolor('darkgrey', 0.3))
lines(filter(status, Resource == 'Hospitals with electricity') %>%
    select(date, Value), col='darkred', lwd=2)
lines(filter(status, Resource == 'Hospitals') %>%
    select(date, Value), col='darkblue', lwd=2)
arrows(ymd('2017-09-21'), 23, ymd('2017-09-21'), 0, length=0.05)
text(ymd('2017-09-25'), 25, 'Landfall - Maria', cex=0.75)
axis.Date(1, at=x.dates, format="%b %d", col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex=0.8)
axis(2, at=seq(0, 70, by=10), las=1, col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex=0.8)
mtext('Number of hospitals', 2, line=2.5, col='grey40', cex=0.8)
legend('bottomright', bty='n',
  legend=c('Assisted', 'Connected to electric grid'),
  lty=c(1, 1), col=c('darkblue', 'darkred'), cex=0.7)
dev.off()
}
plot.hosp()

### Water
agua.m = summary(lm(Value ~ date, data=filter(status, 
  Resource == 'Water', Location == 'Puerto Rico')))
agua.m$coefficients['date', 'Estimate'] - 
  qnorm(0.975) * agua.m$coefficients['date', 'Std. Error']
agua.m$coefficients['date', 'Estimate'] + 
  qnorm(0.975) * agua.m$coefficients['date', 'Std. Error']
water.colors = c("#5467d0", "#637b32", "#475a8d", "#bf562c", "#4e5d50", "#9a4447")
water.alpha = c(rep(1, 0.7), 1)
water.lwd = c(rep(1, 5), 3)
water.locs = c('Metro', 'Norte', 'Oeste', 'Sur', 'Este', 'Puerto Rico')
plot.water = function() {
png('figs/Water.png', 5, 3, units='in', res=360)
par(mar=c(2, 3.5, 0.5, 0.5), cex=0.8)
plot(x.dates, rep(1, length(x.dates)),
  ylim=c(0, 100),
  type='n', 
  axes=F, xlab='', ylab='')
abline(h=seq(0, 100, by=20), col=adjustcolor('darkgrey', 0.3))
abline(v=x.dates, col=adjustcolor('darkgrey', 0.3))
for (i in 1:6 ) {
  this.water = filter(status, Resource == 'Water', Location == water.locs[i])
  lines(this.water$date, this.water$Value, 
    col=adjustcolor(water.colors[i], water.alpha), lwd=water.lwd[i])
  if (i == 6) {
    text(this.water$date[1]-1, this.water$Value[1]-1,
      labels=water.locs[i], col=water.colors[i], pos=3, cex=0.9, font=4)
  } else {
    text(this.water$date[1], this.water$Value[1],
      labels=water.locs[i], col=water.colors[i], pos=2, cex=0.9, font=3)
  }
}
arrows(ymd('2017-09-21'), 10, ymd('2017-09-21'), 0, length=0.05)
text(ymd('2017-09-25'), 14, 'Landfall - Maria', cex=0.75)
axis.Date(1, at=x.dates, format="%b %d", col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex=0.8)
axis(2, at=seq(0, 100, by=20), las=1, col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex=0.8)
mtext('Percent with running water', 2, line=2.5, col='grey40', cex=0.8)
dev.off()
}
plot.water()

### Telecommunications
cell.color = c('darkblue', 'darkgreen')
plot.telecom = function() {
png('figs/Telecom.png', 10, 3, units='in', res=360)
par(mar=c(2, 3.5, 0.5, 0.5), cex=0.8, mfrow=c(1, 2))

# overall
plot(x.dates, rep(1, length(x.dates)),
  ylim=c(0, 100),
  type='n', 
  axes=F, xlab='', ylab='')
abline(h=seq(0, 100, by=20), col=adjustcolor('darkgrey', 0.3))
abline(v=x.dates, col=adjustcolor('darkgrey', 0.3))
plot.trend(filter(status, Resource == 'Telecomunications') %>%
    select(date, Value), col='darkblue')
arrows(ymd('2017-09-21'), 50, ymd('2017-09-21'), 0, length=0.05)
text(ymd('2017-09-21'), 54, 'Landfall - Maria', cex=0.75, adj=0.1)
axis.Date(1, at=x.dates, format="%b %d", col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex=0.8)
axis(2, at=seq(0, 100, by=20), las=1, col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex=0.8)
mtext('Percent with telecomunications', 2, line=2.5, col='grey40', cex=0.8)

# cell connection
plot(x.dates, rep(1, length(x.dates)),
  ylim=c(0, 2800),
  type='n', 
  axes=F, xlab='', ylab='')
abline(h=seq(0, 2800, by=500), col=adjustcolor('darkgrey', 0.3))
abline(v=x.dates, col=adjustcolor('darkgrey', 0.3))
#lines(c(x.dates[1], x.dates[length(x.dates)]), c(2712, 2712), 
#  col=cell.color[1], lty=3)
#text(x.dates[1], 2712+100, 
#  labels='2,712 total cell antennas in Puerto Rico (source: FCC)', 
#  col=cell.color[1], cex=0.8, pos=4, font=3)
lines(c(x.dates[1], x.dates[length(x.dates)]), c(2671, 2671), 
  col=cell.color[2], lty=3)
text(x.dates[1], 2671+100, labels='2,671 total cell antennas in Puerto Rico (source: JRTC)', 
  col=cell.color[2], cex=0.8, pos=4, font=3)
lines(filter(status, Resource == 'Cell antennas') %>%
    select(date, Value), col=adjustcolor(cell.color[2], 0.7), lwd=2)
#lines(filter(status, Resource == 'Cell towers') %>%
#    select(date, Value), col=adjustcolor(cell.color[1], 0.7), lwd=2)
arrows(ymd('2017-09-21'), 1200, ymd('2017-09-21'), 0, length=0.05)
text(ymd('2017-09-21'), 1300, 'Landfall - Maria', cex=0.75, adj=0.1)
axis.Date(1, at=x.dates, format="%b %d", col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex=0.8)
axis(2, at=seq(0, 2800, by=500), las=1, col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex=0.8)
mtext('Number functioning', 2, line=2.5, col='grey40', cex=0.8)

dev.off()
}
plot.telecom()





rm(list=ls())

library(data.table)
library(dplyr)
library(lubridate)
library(stringr)

### dates for plot
x.dates = seq(ymd('2017-09-21'), ymd('2017-12-31'), by='weeks')

### reported data
elec = fread('data/StatusPR.csv') %>%
  mutate(
    yr = '2017',
    date = ymd(paste(yr, Month, Day, sep='-'))) %>%
  arrange(date) %>%
  filter(Resource == 'Electricity')

### targets from Oct 14
targets = data.table(
  date = ymd(c('2017-10-30', '2017-11-15', '2017-12-01', '2017-12-15')),
  target = c(30, 50, 80, 95))

### model expectation on Oct 14
initial.model <- lm(Value ~ date, filter(elec, date <= ymd('2017-10-14')))
summary(initial.model)
initial.preds = predict.lm(initial.model, 
  newdata=data.table(date=c(min(elec$date), ymd('2017-12-31'))),
  se.fit=T, interval='prediction')$fit %>%
  as.data.frame() %>%
  mutate(date = c(min(elec$date), ymd('2017-12-31')))

### plot
plot.elec = function() {
png('figs/Electricity.png', 5, 3, units='in', res=360)
par(mar=c(2, 2.5, 0.5, 0.5), cex=0.8)
plot(x.dates, rep(1, length(x.dates)),
  ylim=c(0, 101),
  type='n', 
  axes=F, xlab='', ylab='')
abline(h=seq(0, 100, by=20), col=adjustcolor('darkgrey', 0.3))
for (x.date in x.dates) {
  lines(rep(x.date, 2), c(-4, 100), col=adjustcolor('darkgrey', 0.3))
}
lines(filter(elec, date <= ymd('2017-10-14')) %>% select(date, Value), 
  col='darkblue')
points(filter(elec, date <= ymd('2017-10-14')) %>% select(date, Value), 
  col='darkblue', pch=19, cex=0.8)
points(filter(elec, ymd('2017-10-14') < date) %>% select(date, Value), 
  col='darkblue', pch=1, cex=0.8)

# prediction
polygon(c(initial.preds$date, rev(initial.preds$date)),
  c(initial.preds$upr, rev(initial.preds$lwr)), 
  border=F, col=adjustcolor('darkblue', 0.5))
lines(select(initial.preds, date, fit), col='darkblue', lty=3)
lines(select(initial.preds, date, lwr), col='darkblue', lty=1, lwd=0.5)
lines(select(initial.preds, date, upr), col='darkblue', lty=1, lwd=0.5)

# targets
points(targets, col='darkred', pch=19)
text(targets$date, targets$target+8, labels=format(targets$date, '%b %d'), 
  col='darkred', pch=19, cex=0.7)
text(targets$date, targets$target+4, labels=paste0(targets$target, '%'), 
  col='darkred', pch=19, cex=0.7)

# landfall
arrows(ymd('2017-09-21'), 50, ymd('2017-09-21'), 0, length=0.05)
#lines(rep(ymd('2017-09-21'), 2), c(0, 50), lwd=1)
text(ymd('2017-09-26'), 53, 'Landfall - Maria', cex=0.75)

# dressing
axis.Date(1, at=x.dates, format="%b %d", col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex.axis=0.8)
axis(2, at=seq(0, 100, by=20), las=1, col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex.axis=0.8)
mtext('Percent of PR with electricity (status.pr)', 2, line=1.5, col='grey40', cex=0.7)
dev.off()
}
plot.elec()



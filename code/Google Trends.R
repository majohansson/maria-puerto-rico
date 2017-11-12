rm(list=ls())

library(data.table)
library(tidyr)
library(dplyr)
library(stringr)

end.date = as.Date('2017-11-11')

# query
# https://trends.google.com/trends/explore?date=2017-07-01%202017-10-22&geo=US&q=Hurricane%20Harvey,Hurricane%20Irma,Hurricane%20Maria
# https://trends.google.com/trends/explore?date=2017-07-01%202017-10-22&geo=US&q=Texas,Florida,Puerto%20Rico

# data
hurr = fread('data/multiTimeline.csv') %>%
  gather(term, gt, -Day) %>%
  mutate(
    term = str_replace(term, '\\:\\s\\(United States\\)', ''),
    date = as.Date(Day)
  ) %>%
  arrange(date)

states = fread('data/multiTimelineStates.csv') %>%
  gather(term, gt, -Day) %>%
  mutate(
    term = str_replace(term, '\\:\\s\\(United States\\)', ''),
    date = as.Date(Day)
  ) %>%
  arrange(date)

states.bkg = states %>%
  filter(as.Date('2017-07-01') < date & date <= as.Date('2017-07-31')) %>%
  group_by(term) %>%
  summarize(july = mean(gt))

# normalize states
states = left_join(states, select(states.bkg, term, july)) %>%
  mutate(add.gt = gt - july)


# filter date
hurr = filter(hurr, as.Date('2017-08-20') < date & date <= end.date)
states = filter(states, as.Date('2017-08-20') < date & date <= end.date)

### figure
plot.trend = function(dt, col) {
  polygon(c(dt[[1]][1], dt[[1]], dt[[1]][length(dt[[1]])]),
    c(0, dt[[2]], 0), border=NA, col=adjustcolor(col, 0.7))
  lines(dt[[1]], dt[[2]], col=col)
}

harvey.landfall = as.Date('2017-08-25')
harvey.col = 'darkorange'
irma.landfall = as.Date('2017-09-10')
irma.col = 'deeppink3'
maria.landfall = as.Date('2017-09-20')
maria.col = 'darkblue'
nate.landfall = as.Date('2017-10-07')
nate.col = 'darkred'

x.dates = seq(as.Date('2017-08-20'), max(hurr$date)+3, by='weeks')

plot.gt = function() {
png('figs/GoogleTrends.png', 7, 3, units='in', res=360)
par(mfrow=c(1, 2), mar=c(2, 2, 0.5, 0.5), oma=c(0, 1.5, 0, 0), cex=0.7)

# hurricanes
plot(filter(hurr, term == 'Hurricane Harvey') %>% select(date, gt),
  ylim=c(-1, 100), type='n', 
  axes=F, xlab='', ylab='')
abline(h=c(0, 25, 50, 75, 100), col=adjustcolor('darkgrey', 0.3))
abline(v=x.dates, col=adjustcolor('darkgrey', 0.3))
# Harvey
plot.trend(filter(hurr, term == 'Hurricane Harvey') %>% select(date, gt), 
  col=harvey.col)
lines(rep(harvey.landfall, 2), c(0, 25))
text(harvey.landfall, 27, 'Landfall - Texas', cex=0.75, adj=0.4)
text(as.Date('2017-08-26'), 46, col=harvey.col, 
  '"Hurricane Harvey"', cex=0.75)
# Irma
plot.trend(filter(hurr, term == 'Hurricane Irma') %>% select(date, gt), 
  col=irma.col)
lines(rep(irma.landfall, 2), c(0, 30))
text(irma.landfall, 32, 'Landfall - Florida', cex=0.75)
text(as.Date('2017-09-19'), 70, col=irma.col, 
  '"Hurricane Irma"', cex=0.75)
# Maria
plot.trend(filter(hurr, term == 'Hurricane Maria') %>% select(date, gt), 
  col=maria.col)
lines(rep(maria.landfall, 2), c(0, 25))
text(maria.landfall, 27, 'Landfall - Puerto Rico', cex=0.75)
text(as.Date('2017-09-29'), 15, col=maria.col, 
  '"Hurricane Maria"', cex=0.75)
axis.Date(1, at=x.dates, format="%b %d", col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex=0.5)
axis(2, at=c(0, 100), labels=c('0', 'Max'), las=1, col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex=0.8)
mtext('Relative Google search volume', 2, line=2.5, col='grey50', cex=0.7)
mtext('Hurricanes', 3, -2, font=2)

# states
plot(filter(states, term == 'Texas') %>% select(date, gt),
  ylim=c(-1, 100), type='n', 
  axes=F, xlab='', ylab='')
abline(h=c(0, 25, 50, 75, 100), col=adjustcolor('darkgrey', 0.3))
abline(v=x.dates, col=adjustcolor('darkgrey', 0.3))
# Harvey
lines(filter(states, term == 'Texas') %>% select(date, gt), 
  col=adjustcolor(harvey.col, 0.5), lwd=0.5)
plot.trend(filter(states, term == 'Texas') %>% select(date, add.gt), 
  col=harvey.col)
lines(rep(harvey.landfall, 2), c(0, 25))
text(harvey.landfall, 27, 'Landfall - Texas', cex=0.75, adj=0.4)
text(as.Date('2017-08-28'), 40, col=harvey.col, 
  '"Texas"', cex=0.75)
# Irma
lines(filter(states, term == 'Florida') %>% select(date, gt), 
  col=adjustcolor(irma.col, 0.5), lwd=0.5)
plot.trend(filter(states, term == 'Florida') %>% select(date, add.gt), 
  col=irma.col)
lines(rep(irma.landfall, 2), c(0, 30))
text(irma.landfall, 32, 'Landfall - Florida', cex=0.75)
text(as.Date('2017-09-15'), 55, col=irma.col, 
  '"Florida"', cex=0.75)
# Maria
lines(filter(states, term == 'Puerto Rico') %>% select(date, gt), 
  col=adjustcolor(maria.col, 0.5), lwd=0.5)
plot.trend(filter(states, term == 'Puerto Rico') %>% select(date, add.gt), 
  col=maria.col)
lines(rep(maria.landfall, 2), c(0, 25))
text(maria.landfall, 27, 'Landfall - Puerto Rico', cex=0.75)
text(as.Date('2017-9-27'), 18, col=maria.col, 
  '"Puerto Rico"', cex=0.75)

axis.Date(1, at=x.dates, format="%b %d", col='white', 
  mgp=c(3, 0, 0), col.axis='grey50', cex=0.8)
mtext('States/Territory', 3, -2, font=2)
legend(as.Date('2017-09-20'), 90, bty='n',
  legend=c('Relative volume', 'Increase compared to July'),
  lty=c(1, 0), lwd=c(0.5, 0), pch=c(NA, 22), col='black',
  pt.bg=c(NA, 'grey40'), pt.cex=1.5, cex=0.7)

dev.off()
}
plot.gt()


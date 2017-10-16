rm(list=ls())

library(data.table)
library(dplyr)
library(stringr)

end.date = as.Date('2017-10-15')

# query
# https://trends.google.com/trends/explore?date=2017-08-01%202017-10-16&geo=US&q=%2Fm%2F0h553ms,Hurricane%20Harvey,Hurricane%20Irma

hurr = fread('data/multiTimeline.csv')
names(hurr) = str_replace(names(hurr), '\\:\\s\\(United States\\)', '')

# manipulate & filter date
hurr = mutate(hurr, date = as.Date(Day)) %>%
  filter(as.Date('2017-08-20') < date & date <= end.date) %>%
  arrange(date)




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
png('figs/Google_Trends.png', 5, 3, units='in', res=360)
par(mar=c(2, 3, 0.5, 0.5), cex=0.8)
plot(hurr$date, rep(1, dim(hurr)[1]),
  ylim=c(0, 100), type='n', 
  axes=F, xlab='', ylab='')
abline(h=c(0, 25, 50, 75, 100), col=adjustcolor('darkgrey', 0.3))
abline(v=x.dates, col=adjustcolor('darkgrey', 0.3))
# Harvey
plot.trend(select(hurr, date, `Hurricane Harvey`), col=harvey.col)
lines(rep(harvey.landfall, 2), c(0, 25))
text(harvey.landfall, 30, 'Landfall - Texas', cex=0.75, adj=0.4)
text(as.Date('2017-08-26'), 48, col=harvey.col, 
  '"Hurricane Harvey"', cex=0.75)
# Irma
plot.trend(select(hurr, date, `Hurricane Irma`), col=irma.col)
lines(rep(irma.landfall, 2), c(0, 55))
text(irma.landfall, 60, 'Landfall - Florida', cex=0.75)
text(as.Date('2017-08-31'), 80, col=irma.col, 
  '"Hurricane Irma"', cex=0.75)
# Maria
plot.trend(select(hurr, date, `Hurricane Maria`), col=maria.col)
lines(rep(maria.landfall, 2), c(0, 22))
text(maria.landfall, 27, 'Landfall - Puerto Rico', cex=0.75)
text(as.Date('2017-09-20'), 35, col=maria.col, 
  '"Hurricane Maria"', cex=0.75)
# Nate

axis.Date(1, at=x.dates, format="%b %d", col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex=0.8)
axis(2, at=c(0, 100), labels=c('0', 'Max'), las=1, col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex=0.8)
mtext('Relative Google search volume', 2, line=1.5, col='grey40', cex=0.8)
dev.off()
}
plot.gt()


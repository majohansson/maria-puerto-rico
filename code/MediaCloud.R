rm(list=ls())

library(data.table)
library(dplyr)
library(stringr)

end.date = as.Date('2017-10-03')

### queries
# hurricanes
#https://dashboard.mediacloud.org/#query/["\"Hurricane Harvey\"","\"Hurricane Irma\"","\"Hurricane Maria\"","\"Hurricane Nate\""]/[{"sets":[9139487]},{"sets":[9139487]},{"sets":[9139487]},{"sets":[9139487]}]/["2017-8-01","2017-8-01","2017-8-01","2017-8-01"]/["2017-10-7","2017-10-7","2017-10-7","2017-10-7"]/[{"uid":7,"name":"Harvey","color":"e14c11"},{"uid":8,"name":"Irma","color":"20b1b8"},{"uid":9,"name":"Maria","color":"5f165f"},{"uid":10,"name":"Nate","color":"9e7b06"}]
# states
#https://dashboard.mediacloud.org/#query/["Texas","Florida","\"Puerto Rico\"","Mississippi"]/[{"sets":[9139487]},{"sets":[9139487]},{"sets":[9139487]},{"sets":[9139487]}]/["2017-8-01","2017-8-01","2017-8-01","2017-8-01"]/["2017-10-7","2017-10-7","2017-10-7","2017-10-7"]/[{"uid":3,"name":"Texas","color":"e14c11"},{"uid":4,"name":"Florida","color":"20b1b8"},{"uid":5,"name":"Puerto Rico","color":"5f165f"},{"uid":6,"name":"Mississippi","color":"9e7b06"}]

# load hurricane data
hurr.names = c('Harvey', 'Irma', 'Maria', 'Nate')
hurr.files = list.files('data/mediacloud-hurricanes/') %>%
  sort()
hurr = data.table()
for (i in 1:4) {
  hurr = fread(paste0('data/mediacloud-hurricanes/', hurr.files[i])) %>%
    mutate(hurricane = hurr.names[i]) %>%
    bind_rows(hurr)
}

# load state data
state.names = c('Texas', 'Florida', 'Puerto Rico', 'Mississipi')
state.files = list.files('data/mediacloud-states/') %>%
  sort()
states = data.table()
for (i in 1:4) {
  states = fread(paste0('data/mediacloud-states/', state.files[i])) %>%
    mutate(state = state.names[i]) %>%
    bind_rows(states)
}

# manipulate & filter date
hurr = mutate(hurr, date = as.Date(date)) %>%
  filter(as.Date('2017-08-20') < date & date <= end.date) %>%
  arrange(date)
states = mutate(states, date = as.Date(date)) %>%
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

plot.mediacloud = function() {
png('figs/MediaCloud.png', 7, 3, units='in', res=360)
par(mfrow=c(1, 2), mar=c(2, 2, 0.5, 0.5), oma=c(0, 1.5, 0, 0), cex=0.7)

# hurricanes
plot(filter(hurr, hurricane == 'Harvey') %>% select(date, sentences),
  ylim=c(0, 4500), type='n', 
  axes=F, xlab='', ylab='')
abline(h=seq(0, 4000, by=1000), col=adjustcolor('darkgrey', 0.3))
abline(v=x.dates, col=adjustcolor('darkgrey', 0.3))
# Harvey
plot.trend(filter(hurr, hurricane == 'Harvey') %>% select(date, sentences), 
  col=harvey.col)
lines(rep(harvey.landfall, 2), c(0, 1100))
text(harvey.landfall, 1200, 'Landfall - Texas', cex=0.75, adj=0.4)
text(as.Date('2017-08-28'), 2400, col=harvey.col, 
  '"Hurricane Harvey"', cex=0.75)
# Irma
plot.trend(filter(hurr, hurricane == 'Irma') %>% select(date, sentences), 
  col=irma.col)
lines(rep(irma.landfall, 2), c(0, 1400))
text(irma.landfall, 1500, 'Landfall - Florida', cex=0.75)
text(as.Date('2017-09-10'), 2800, col=irma.col, 
  '"Hurricane Irma"', cex=0.75)
# Maria
plot.trend(filter(hurr, hurricane == 'Maria') %>% select(date, sentences), 
  col=maria.col)
lines(rep(maria.landfall, 2), c(0, 1100))
text(maria.landfall, 1200, 'Landfall - Puerto Rico', cex=0.75)
text(as.Date('2017-09-27'), 800, col=maria.col, 
  '"Hurricane Maria"', cex=0.75)
# Nate

axis.Date(1, at=x.dates, format="%b %d", col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex=0.5)
axis(2, at=seq(0, 4000, by=1000), las=1, col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex=0.5)
mtext('Sentences in MediaCloud US Top Online News', 2, line=2.5, col='grey50', cex=0.7)
mtext('Hurricanes', 3, -2, font=2)

# states
# hurricanes
plot(filter(states, state == 'Texas') %>% select(date, sentences),
  ylim=c(0, 4500), type='n', 
  axes=F, xlab='', ylab='')
abline(h=seq(0, 4000, by=1000), col=adjustcolor('darkgrey', 0.3))
abline(v=x.dates, col=adjustcolor('darkgrey', 0.3))
# Harvey
plot.trend(filter(states, state == 'Texas') %>% select(date, sentences), 
  col=harvey.col)
lines(rep(harvey.landfall, 2), c(0, 1100))
text(harvey.landfall, 1200, 'Landfall - Texas', cex=0.75, adj=0.4)
text(as.Date('2017-08-24'), 2500, col=harvey.col, 
  '"Texas"', cex=0.75)
# Irma
plot.trend(filter(states, state == 'Florida') %>% select(date, sentences), 
  col=irma.col)
lines(rep(irma.landfall, 2), c(0, 1600))
text(irma.landfall, 1700, 'Landfall - Florida', cex=0.75)
text(as.Date('2017-09-06'), 3200, col=irma.col, 
  '"Florida"', cex=0.75)
# Maria
plot.trend(filter(states, state == 'Puerto Rico') %>% select(date, sentences), 
  col=maria.col)
lines(rep(maria.landfall, 2), c(0, 1400))
text(maria.landfall, 1500, 'Landfall - Puerto Rico', cex=0.75)
text(as.Date('2017-09-27'), 2200, col=maria.col, 
  '"Puerto Rico"', cex=0.75)
# Nate

axis.Date(1, at=x.dates, format="%b %d", col='white', 
  mgp=c(3, 0, 0), col.axis='grey50', cex=0.8)
mtext('States/Territory', 3, -2, font=2)

dev.off()
}
plot.mediacloud()


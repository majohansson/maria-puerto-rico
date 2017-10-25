rm(list=ls())

library(data.table)
library(dplyr)
library(stringr)

end.date = as.Date('2017-10-21')

### queries
# hurricanes
#https://dashboard.mediacloud.org/#query/["\"Hurricane Harvey\"","\"Hurricane Irma\"","\"Hurricane Maria\""]/[{"sets":[9139487]},{"sets":[9139487]},{"sets":[9139487]}]/["2017-8-01","2017-8-01","2017-8-01"]/["2017-10-16","2017-10-16","2017-10-16"]/[{"uid":3,"name":"Harvey","color":"e14c11"},{"uid":4,"name":"Irma","color":"20b1b8"},{"uid":5,"name":"Maria","color":"5f165f"}]

# load hurricane data
hurr.names = c('Harvey', 'Irma', 'Maria', 'Nate')
hurr.files = list.files('data/mediacloud-hurricanes/') %>%
  sort()
hurr = data.table()
for (i in 1:length(hurr.files)) {
  hurr = fread(paste0('data/mediacloud-hurricanes/', hurr.files[i])) %>%
    mutate(hurricane = hurr.names[i]) %>%
    bind_rows(hurr)
}

# load state data
state.names = c('Texas', 'Florida', 'Puerto Rico', 'Mississipi')
state.files = list.files('data/mediacloud-states/') %>%
  sort()
states = data.table()
for (i in 1:length(state.files)) {
  states = fread(paste0('data/mediacloud-states/', state.files[i])) %>%
    mutate(state = state.names[i]) %>%
    bind_rows(states)
}

states.bkg = states %>%
  filter(as.Date('2017-07-01') < date & date <= as.Date('2017-07-31')) %>%
  group_by(state) %>%
  summarize(july = mean(sentences))

# normalize states
states = left_join(states, select(states.bkg, state, july)) %>%
  mutate(add.sent = sentences - july)

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
y.scale = 12000/5000

plot.mediacloud = function() {
png('figs/MediaCloud.png', 7, 3, units='in', res=360)
par(mfrow=c(1, 2), mar=c(2, 2, 0.5, 0.5), oma=c(0, 1.5, 0, 0), cex=0.7)

# hurricanes
plot(filter(hurr, hurricane == 'Harvey') %>% select(date, sentences),
  ylim=c(0, 12000), type='n', 
  axes=F, xlab='', ylab='')
abline(h=seq(0, 12000, by=2000), col=adjustcolor('darkgrey', 0.3))
abline(v=x.dates, col=adjustcolor('darkgrey', 0.3))
# Harvey
plot.trend(filter(hurr, hurricane == 'Harvey') %>% select(date, sentences), 
  col=harvey.col)
lines(rep(harvey.landfall, 2), c(0, 1100*y.scale))
text(harvey.landfall, 1200*y.scale, 'Landfall - Texas', cex=0.75, adj=0.4)
text(as.Date('2017-08-28'), 2700*y.scale, col=harvey.col, 
  '"Hurricane Harvey"', cex=0.75)
# Irma
plot.trend(filter(hurr, hurricane == 'Irma') %>% select(date, sentences), 
  col=irma.col)
lines(rep(irma.landfall, 2), c(0, 1400*y.scale))
text(irma.landfall, 1500*y.scale, 'Landfall - Florida', cex=0.75)
text(as.Date('2017-09-10'), 3300*y.scale, col=irma.col, 
  '"Hurricane Irma"', cex=0.75)
# Maria
plot.trend(filter(hurr, hurricane == 'Maria') %>% select(date, sentences), 
  col=maria.col)
lines(rep(maria.landfall, 2), c(0, 1100*y.scale))
text(maria.landfall, 1200*y.scale, 'Landfall - Puerto Rico', cex=0.75)
text(as.Date('2017-10-01'), 900*y.scale, col=maria.col, 
  '"Hurricane Maria"', cex=0.75)
# Nate

axis.Date(1, at=x.dates, format="%b %d", col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex=0.5)
axis(2, at=seq(0, 12000, by=2000), las=1, col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex=0.5)
mtext('Sentences in MediaCloud US Top Online News', 2, line=2.5, col='grey50', cex=0.7)
mtext('Hurricanes', 3, -2, font=2)

# states
plot(filter(states, state == 'Texas') %>% select(date, sentences),
  ylim=c(0, 12000), type='n', 
  axes=F, xlab='', ylab='')
abline(h=seq(0, 12000, by=2000), col=adjustcolor('darkgrey', 0.3))
abline(v=x.dates, col=adjustcolor('darkgrey', 0.3))
# Harvey
lines(filter(states, state == 'Texas') %>% select(date, sentences), 
  col=adjustcolor(harvey.col, 0.5), lwd=0.5)
plot.trend(filter(states, state == 'Texas') %>% select(date, add.sent), 
  col=harvey.col)
lines(rep(harvey.landfall, 2), c(0, 1100*y.scale))
text(harvey.landfall, 1200*y.scale, 'Landfall - Texas', cex=0.75, adj=0.4)
text(as.Date('2017-08-22'), 3600*y.scale, col=harvey.col, 
  '"Texas"', cex=0.75)
# Irma
lines(filter(states, state == 'Florida') %>% select(date, sentences), 
  col=adjustcolor(irma.col, 0.5), lwd=0.5)
plot.trend(filter(states, state == 'Florida') %>% select(date, add.sent), 
  col=irma.col)
lines(rep(irma.landfall, 2), c(0, 1600*y.scale))
text(irma.landfall, 1700*y.scale, 'Landfall - Florida', cex=0.75)
text(as.Date('2017-09-16'), 3500*y.scale, col=irma.col, 
  '"Florida"', cex=0.75)
# Maria
lines(filter(states, state == 'Puerto Rico') %>% select(date, sentences), 
  col=adjustcolor(maria.col, 0.5), lwd=0.5)
plot.trend(filter(states, state == 'Puerto Rico') %>% select(date, add.sent), 
  col=maria.col)
lines(rep(maria.landfall, 2), c(0, 1900*y.scale))
text(maria.landfall, 2000*y.scale, 'Landfall - Puerto Rico', cex=0.75)
text(as.Date('2017-10-04'), 2700*y.scale, col=maria.col, 
  '"Puerto Rico"', cex=0.75)
# Nate

axis.Date(1, at=x.dates, format="%b %d", col='white', 
  mgp=c(3, 0, 0), col.axis='grey50', cex=0.8)
mtext('States/Territory', 3, -2, font=2)
legend(as.Date('2017-09-25'), 9800, bty='n',
  legend=c('Relative volume', 'Increase compared to July'),
  lty=c(1, 0), lwd=c(0.5, 0), pch=c(NA, 22), col='black',
  pt.bg=c(NA, 'grey40'), pt.cex=1.5, cex=0.7)

dev.off()
}
plot.mediacloud()


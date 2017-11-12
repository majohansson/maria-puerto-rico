rm(list=ls())

library(data.table)
library(dplyr)
library(stringr)

end.date = as.Date('2017-11-11')

### query
#https://television.gdeltproject.org/cgi-bin/iatv_ftxtsearch/iatv_ftxtsearch?primary_keyword=%22Hurricane+Maria%22&context_keywords=&filter_network=ALL&filter_timespan=CUSTOM&filter_timespan_custom_start=08%2F01%2F2017&filter_timespan_custom_end=&filter_displayas=PERCENT&filter_combineseparate=COMBINE&filter_outputtype=DISPLAY#searchbox

# load tv coverage data
tv.files = list.files('data/television-explorer/')
tv = data.table()
for (i in 1:length(tv.files)) {
  tv = fread(paste0('data/television-explorer/', tv.files[i])) %>%
    mutate(
      date = as.Date(str_sub(as.character(Date), 1, 8), '%Y%m%d'),
      query = str_extract(tv.files[i], '\\-.*'),
      query = str_replace(query, '.csv', ''),
      query = str_replace(query, '^-', '')
      ) %>%
    select(-Date) %>%
    bind_rows(tv)
}

# july background
tv.bkg = tv %>%
  filter(as.Date('2017-07-01') < date & date <= as.Date('2017-07-31')) %>%
  group_by(query) %>%
  summarize(july = mean(Combined))

# normalize
tv = left_join(tv, select(tv.bkg, query, july)) %>%
  mutate(add.Combined = Combined - july)

# filter date
tv = filter(tv, as.Date('2017-08-20') < date & date <= end.date) %>%
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

x.dates = seq(as.Date('2017-08-20'), max(tv$date)+3, by='weeks')

plot.tv = function() {
png('figs/TelevisionExplorer.png', 7, 3, units='in', res=360)
par(mfrow=c(1, 2), mar=c(2, 2, 0.5, 0.5), oma=c(0, 1.5, 0, 0), cex=0.7)

# hurricanes
plot(filter(tv, query == 'Harvey') %>% select(date, Combined),
  ylim=c(0, 3.1), type='n', 
  axes=F, xlab='', ylab='')
abline(h=seq(0, 3, by=0.5), col=adjustcolor('darkgrey', 0.3))
abline(v=x.dates, col=adjustcolor('darkgrey', 0.3))
# Harvey
plot.trend(filter(tv, query == 'Harvey') %>% select(date, Combined), 
  col=harvey.col)
lines(rep(harvey.landfall, 2), c(0, 1.1))
text(harvey.landfall, 1.2, 'Landfall - Texas', cex=0.75, adj=0.4)
text(as.Date('2017-09-02'), 1.05, col=harvey.col, 
  '"Hurricane Harvey"', cex=0.75)
# Irma
plot.trend(filter(tv, query == 'Irma') %>% select(date, Combined), 
  col=irma.col)
lines(rep(irma.landfall, 2), c(0, 1.2))
text(irma.landfall, 1.3, 'Landfall - Florida', cex=0.75)
text(as.Date('2017-09-18'), 0.95, col=irma.col, 
  '"Hurricane Irma"', cex=0.75)
# Maria
plot.trend(filter(tv, query == 'Maria') %>% select(date, Combined), 
  col=maria.col)
lines(rep(maria.landfall, 2), c(0, 0.65))
text(maria.landfall, 0.75, 'Landfall - Puerto Rico', cex=0.75)
text(as.Date('2017-10-01'), 0.4, col=maria.col, 
  '"Hurricane Maria"', cex=0.75)
axis.Date(1, at=x.dates, format="%b %d", col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex=0.5)
axis(2, at=seq(0, 3, by=0.5), las=1, col='white', 
  mgp=c(3, 0, 0), col.axis='grey40', cex=0.5)
mtext('Percent of sentences across all networks', 2, line=2.5, col='grey50', cex=0.7)
mtext('Hurricanes', 3, -2, font=2)

# states
plot(filter(tv, query == 'Texas') %>% select(date, Combined),
  ylim=c(0, 3.1), type='n', 
  axes=F, xlab='', ylab='')
abline(h=seq(0, 3, by=0.5), col=adjustcolor('darkgrey', 0.3))
abline(v=x.dates, col=adjustcolor('darkgrey', 0.3))
# Harvey
lines(filter(tv, query == 'Texas') %>% select(date, Combined), 
  col=adjustcolor(harvey.col, 0.5), lwd=0.5)
plot.trend(filter(tv, query == 'Texas') %>% select(date, add.Combined), 
  col=harvey.col)
lines(rep(harvey.landfall, 2), c(0, 0.7))
text(harvey.landfall, 0.8, 'Landfall - Texas', cex=0.75, adj=0.4)
text(as.Date('2017-08-28'), 2.3, col=harvey.col, 
  '"Texas"', cex=0.75)
# Irma
lines(filter(tv, query == 'Florida') %>% select(date, Combined), 
  col=adjustcolor(irma.col, 0.5), lwd=0.5)
plot.trend(filter(tv, query == 'Florida') %>% select(date, add.Combined), 
  col=irma.col)
lines(rep(irma.landfall, 2), c(0, 1.2))
text(irma.landfall, 1.3, 'Landfall - Florida', cex=0.75)
text(as.Date('2017-09-17'), 2.1, col=irma.col, 
  '"Florida"', cex=0.75)
# Maria
lines(filter(tv, query == 'Puerto_Rico') %>% select(date, Combined), 
  col=adjustcolor(maria.col, 0.5), lwd=0.5)
plot.trend(filter(tv, query == 'Puerto_Rico') %>% select(date, add.Combined), 
  col=maria.col)
lines(rep(maria.landfall, 2), c(0, 1.45))
text(maria.landfall, 1.55, 'Landfall - Puerto Rico', cex=0.75)
text(as.Date('2017-10-06'), 1.4, col=maria.col, 
  '"Puerto Rico"', cex=0.75)

axis.Date(1, at=x.dates, format="%b %d", col='white', 
  mgp=c(3, 0, 0), col.axis='grey50', cex=0.8)
mtext('States/Territory', 3, -2, font=2)
legend(as.Date('2017-10-10'), 2.5, bty='n',
  legend=c('Total percent', 'Increase compared to July'),
  lty=c(1, 0), lwd=c(0.5, 0), pch=c(NA, 22), col='black',
  pt.bg=c(NA, 'grey40'), pt.cex=1.5, cex=0.7)

dev.off()
}
plot.tv()



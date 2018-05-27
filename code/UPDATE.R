rm(list=ls())
source('code/Electricity.R')
source('code/StatusPR Trends.R')

Sys.Date() - as.Date('2017-09-20')

aee_clients = 1413000
clients <- filter(status, Resource == 'Clients with electricity') %>%
  arrange(date)
aee_clients * (1 - clients$Value[nrow(clients)] / 100)

acueductos_clients = 1220996 / 0.9932
pr_water_prop = status %>%
  filter(Resource == 'Water', Location == 'Puerto Rico') %>%
  arrange(desc(date))
acueductos_clients * (1 - pr_water_prop$Value[1]/100)

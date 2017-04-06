require(dplyr)

assessment <- read.csv('assessment.csv', stringsAsFactors = FALSE) %>%
  select(-(assesssource:mostrecent)) %>%
  mutate(daterecorded = as.Date(daterecorded, format = '%m/%d/%Y')) %>% 
  mutate(dateloaded = as.Date(dateloaded, format = '%m/%d/%Y'))
  

stock <- read.csv('stock.csv', stringsAsFactors = FALSE)

timeseries <- read.csv('timeseries.csv', stringsAsFactors = FALSE)

save(assessment, stock, timeseries, file = 'ram-legacy.RData')

require(knitr)

purl('data_manip_R_lect.Rnw')
r.script <- readLines('data_manip_R_lect.R')
is.heading <- grepl('## --', r.script)
script.reduced <- r.script[!is.heading]
solns.start <- grep('## Solutions', script.reduced, ignore.case = TRUE)
writeLines(text = script.reduced[1:(solns.start-1)], con = 'data_manip_R_lect.R')
writeLines(text = script[-(1:(solns.start-1))], con = 'practice_solns.R')


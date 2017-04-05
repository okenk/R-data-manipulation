require(dplyr)

assessment <- read.csv('assessment.csv', stringsAsFactors = FALSE) %>%
  select(-(assesssource:mostrecent)) %>%
  mutate(daterecorded = as.Date(daterecorded, format = '%m/%d/%Y')) %>% 
  mutate(dateloaded = as.Date(dateloaded, format = '%m/%d/%Y'))
  

stock <- read.csv('stock.csv', stringsAsFactors = FALSE)

timeseries <- read.csv('timeseries.csv', stringsAsFactors = FALSE)

save(assessment, stock, timeseries, file = 'ram-legacy.RData')

require(knitr)
drop.lines <- function(scriptname, what.to.drop){
  script <- readLines(scriptname)
  is.heading <- grepl('## --', script)
  script.reduced <- script[!is.heading]
  writeLines(text = script.reduced, con= scriptname)
}

purl('data_manip_R_lect.Rnw')
drop.lines('data_manip_R_lect.R', '## --')


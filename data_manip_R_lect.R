## Introduction to dplyr and other data manipulation techniques
## Kiva Oken
options(width = 48)
load('ram-legacy.RData')

## install.packages('tidyverse')

require(tidyverse)

olaf.assessments <- filter(assessment, 
                             recorder == 'JENSEN')

cod <- filter(stock, grepl('Gadus', scientificname))

slice(assessment, 5:8)

arrange(olaf.assessments, daterecorded)
arrange(olaf.assessments, desc(daterecorded))

select(olaf.assessments, stockid)
select(olaf.assessments, stock.id = stockid)
rename(olaf.assessments, stock.id = stockid)
select(olaf.assessments, assessid:stockid)
select(olaf.assessments, -recorder)

olaf.delay <- mutate(olaf.assessments, 
                     delay = dateloaded - 
                       daterecorded)
select(olaf.delay, delay)

transmute(olaf.assessments, 
                     delay = dateloaded - 
                       daterecorded)

toothfish.ssb <- filter(timeseries, assessid ==
                          'CCAMLR-ATOOTHFISHRS-1995-2007-JENSEN',
                        tsid == 'SSB-MT')
mutate(toothfish.ssb, 
       zscore = (tsvalue - mean(tsvalue)) / 
         sd(tsvalue))


summarize(toothfish.ssb, mean(tsvalue, na.rm = TRUE))
summarize(toothfish.ssb, n_distinct(tsvalue), n())

do_something <- function(vec) {
  sum(vec, na.rm = TRUE)/5
}
summarize(toothfish.ssb, do_something(tsvalue))
summarise(toothfish.ssb, do_something(tsvalue))

  assessors <- do(olaf.assessments, 
                  assessor = unique(.$assessorid))
  assessors
  assessors$assessor
  class(assessors)
  
  distinct(olaf.assessments, assessorid)

## 1. Create a data frame in R that contains the time series data of Atlantic Amberjack
## using filter(). Hint: this stock is in olaf.assessments.
## 
## 2. Using filter() and one other dplyr function, determine in which year Amberjack had
## the highest recruitment (R-E00).

x <- rnorm(100)
x.mat <- matrix(x, nrow = 10)
x.mns <- apply(x.mat, 1, mean)

x.mns <- apply(matrix(rnorm(100), nrow = 10),
               1, mean)

x.mns <- rnorm(100) %>%
  matrix(nrow = 10) %>%
  apply(1, mean)

filter(timeseries,
       stockid == 'GRAMBERSATLC',
       tsid == 'SSB-MT') %>%
  with(plot(tsyear, tsvalue, type = 'l',
            xlab = 'Year', ylab = 'SSB (mt)',
            main = stocklong[1]))

lm(tsvalue ~ tsyear, data = toothfish.ssb) %>%
  with(plot(fitted.values, residuals))

toothfish <- filter(timeseries, assessid ==
    'CCAMLR-ATOOTHFISHRS-1995-2007-JENSEN') %>%
  select(tsid:tsvalue) %>%
  group_by(tsid)

summarize(toothfish, mn = mean(tsvalue, na.rm = TRUE),
          stdev = sd(tsvalue, na.rm = TRUE))
slice(toothfish, 1) 
mutate(toothfish, z.score = 
         (tsvalue - mean(tsvalue, na.rm=TRUE)) / 
         sd(tsvalue, na.rm=TRUE)) %>%
  View()

select(stock, stockid, scientificname, commonname) %>%
  inner_join(assessment) %>%
  View()

wide.toothfish <- ungroup(toothfish) %>%
  mutate(tsid = gsub('-', '_', tsid)) %>%
  spread(key = 'tsid', value = 'tsvalue')
long.toothfish <- gather(wide.toothfish,
                         key = tsid, value = tsvalue, 
                         BdivBmgttouse_dimensionless:
                           Utouse_index)

## 1. Create a data frame in R of data for Pacific herring (Clupea pallasii) that is
## grouped by area and population metric (SSB, recruitment, etc.). You will need to
## join information from all four of the data tables in the RAM database to do this.
## 
## 2. Using your data frame, calculate the mean and standard deviation of each
## population metric (SSB, recruitment, etc.) for each area. Note that the database
## contains NAs.
## 
## 3. Plot the time series of spawning stock biomass of Pacific herring to compare
## across regions using either do() with base graphics or ggplot().
## 
## 4. Bonus: Color the lines produced above by exploitation rate (ER-ratio). You
## may want to use tidyr.


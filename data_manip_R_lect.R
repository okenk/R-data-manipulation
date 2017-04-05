## ----packages, echo=FALSE, results='hide'--------------------------------
## Introduction to dplyr and other data manipulation techniques
## Kiva Oken
options(width = 48)
load('ram-legacy.RData')

## ----install, eval=FALSE-------------------------------------------------
## install.packages('tidyverse')

## ----load, message=FALSE, warning=FALSE----------------------------------
require(tidyverse)

## ----filter--------------------------------------------------------------
olaf.assessments <- filter(assessment, 
                             recorder == 'JENSEN')

## ----filter2-------------------------------------------------------------
cod <- filter(stock, grepl('Gadus', scientificname))

## ----filter3, results='hide'---------------------------------------------
slice(assessment, 5:8)

## ----arrange, results='hide', tidy=FALSE---------------------------------
arrange(olaf.assessments, daterecorded)
arrange(olaf.assessments, desc(daterecorded))

## ----select, results='hide', tidy=FALSE----------------------------------
select(olaf.assessments, stockid)
select(olaf.assessments, stock.id = stockid)
rename(olaf.assessments, stock.id = stockid)
select(olaf.assessments, assessid:stockid)
select(olaf.assessments, -recorder)

## ----mutate, results='hide', tidy=FALSE----------------------------------
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


## ----summarize, tidy=FALSE-----------------------------------------------
summarize(toothfish.ssb, mean(tsvalue, na.rm = TRUE))
summarize(toothfish.ssb, n_distinct(tsvalue), n())

## ----summarize2, tidy=FALSE, results='hide'------------------------------
do_something <- function(vec) {
  sum(vec, na.rm = TRUE)/5
}
summarize(toothfish.ssb, do_something(tsvalue))
summarise(toothfish.ssb, do_something(tsvalue))

## ----do, results='hide', tidy=FALSE--------------------------------------
  assessors <- do(olaf.assessments, 
                  assessor = unique(.$assessorid))
  assessors
  assessors$assessor
  class(assessors)
  
  distinct(olaf.assessments, assessorid)

## ----exercise_pt1, echo=FALSE, eval=FALSE--------------------------------
## 1. Create a data frame in R that contains the time series data of Atlantic Amberjack
## using filter(). Hint: this stock is in olaf.assessments.
## 
## 2. Using filter() and one other dplyr function, determine in which year Amberjack had
## the highest recruitment (R-E00).

## ----pipe1---------------------------------------------------------------
x <- rnorm(100)
x.mat <- matrix(x, nrow = 10)
x.mns <- apply(x.mat, 1, mean)

## ----pipe2---------------------------------------------------------------
x.mns <- apply(matrix(rnorm(100), nrow = 10),
               1, mean)

## ----pipe3---------------------------------------------------------------
x.mns <- rnorm(100) %>%
  matrix(nrow = 10) %>%
  apply(1, mean)

## ----groupby, tidy=FALSE, results='hide'---------------------------------
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

## ----join, tidy=FALSE----------------------------------------------------
select(stock, stockid, scientificname, commonname) %>%
  inner_join(assessment) %>%
  View()

## ----tidyr, tidy=FALSE---------------------------------------------------
wide.toothfish <- ungroup(toothfish) %>%
  mutate(tsid = gsub('-', '_', tsid)) %>%
  spread(key = 'tsid', value = 'tsvalue')
long.toothfish <- gather(wide.toothfish,
                         key = tsid, value = tsvalue, 
                         BdivBmgttouse_dimensionless:
                           Utouse_index)

## ----exercise_pt2, echo=FALSE, eval=FALSE--------------------------------
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
## 4. Bonus: Color the lines produces above by exploitation rate (ER-ratio). You will
## probably need to use tidyr.

## ----solutions, echo=FALSE, results='hide', fig.show='hide', error=FALSE, message=FALSE----
## Solutions
## Part 1
## 1
aj.assess <- filter(olaf.assessments, grepl('amberjack', stocklong, ignore.case = TRUE))
aj.assess
amberjack <- filter(timeseries, stockid == aj.assess$stockid[2])

## 2
aj.recruit <- filter(amberjack, tsid == 'R-E00')
summarize(aj.recruit, tsyear[which.max(tsvalue)])
arrange(aj.recruit, desc(tsvalue))
filter(aj.recruit, rank(-tsvalue) == 1)

## 3
ggplot(data = filter(amberjack, tsid == 'SSB-MT', tsyear >= 1965, tsyear <= 2005),
       aes(x = tsyear, y = tsvalue)) + 
  geom_line() + geom_point(size = 2) +
  xlab('Year') + ylab('SSB (mt)')
plot(tsvalue ~ tsyear, type = 'l', xlab = 'Year', ylab = 'SSB (mt)', las = 1,
     data = filter(amberjack, tsid == 'SSB-MT', tsyear >= 1965, tsyear <= 2005))


## Part 2
## 1
pac.herring <- filter(stock, scientificname=='Clupea pallasii') %>%
  inner_join(assessment) %>% 
  inner_join(timeseries) %>%
  select(stocklong, tsid, tsyear, tsvalue) %>% 
  group_by(stocklong, tsid)

## 2
summarize(pac.herring, mn = mean(tsvalue, na.rm=TRUE), 
          stdev = sd(tsvalue, na.rm=TRUE)) %>%
  View()

## 3
filter(pac.herring, tsid == 'SSB-MT') %>%
  ggplot() + aes(x=tsyear, y=tsvalue) + geom_line() + 
  facet_wrap(~ gsub('Pacific herring', '', stocklong)) +
  xlab('Year') + ylab('SSB (mt)')

par(mfrow = c(3,3), mar = c(2,2,2,2)) 
filter(pac.herring, tsid == 'SSB-MT') %>%
  do(temp = plot(.$tsyear, .$tsvalue, type = 'l', 
                 xlab = '', ylab = '', 
                 main = gsub('Pacific herring', '', 
                             .$stocklong[1])))

## Bonus
ungroup(pac.herring) %>%
  mutate(tsid = gsub('-', '_', tsid)) %>%
  spread(key = tsid, value = tsvalue) %>%
  filter(!is.na(SSB_MT)) %>%
  ggplot() + aes(x = tsyear, y = SSB_MT) + 
  geom_point(aes(col=ER_ratio)) +
  geom_line() + xlab('Year') + ylab('SSB (mt)') +
  facet_wrap(~ gsub('Pacific herring', '', stocklong))


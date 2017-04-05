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


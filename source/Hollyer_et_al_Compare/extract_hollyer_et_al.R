################################################################################
# Extract mean Hollyer et al. Transparency Scores
# Christopher Gandrud
# MIT License
# Original estimates downloaded from: http://hrvtransparency.org/
################################################################################

library(dplyr)
library(rjags)
library(tidyr)
library(countrycode)
library(DataCombine)
library(foreign)
library(rio)

# Set working directory. Change as needed.
setwd('~/Desktop/')

# Saving directory. Change as needed/
sd <- '/git_repositories/FRTIndex/'

# Load jags result
### Data not stored in git repository due to large file size ####
load('TransparencyIndex2013.RData')

# Extract mean transparency
means <- results$BUGSoutput$mean$transparency %>% as.data.frame()

# Add years
names(means) <- paste0('x', 1980:2010)

# Add country names
means$country <- unique(WDIdata$countryname)

# Reshape the data
gathered <- means %>% gather(year, hrv_mean, 1:31)

gathered$year <- gathered$year %>% gsub('x', '', .) %>% as.numeric()

# Give iso2c names
gathered$iso2c <- countrycode(gathered$country, origin = 'country.name',
                             destination = 'iso2c')
gathered <- gathered %>% select(-country) %>% arrange(iso2c, year)
gathered <- slide(gathered, Var = 'hrv_mean', GroupVar = 'iso2c', 
                  TimeVar = 'year', NewVar = 'lhrv_mean')
gathered <- PercChange(gathered, Var = 'hrv_mean', GroupVar = 'iso2c', 
                       NewVar = 'dhrv_mean', type = 'proportion',
                       TimeVar = 'year')

# Save basic Hollyer et al. medians
simple <- gathered %>% select(iso2c, year, hrv_mean)
export(simple, file = paste0(sd, 'source/Hollyer_et_al_Compare/hrv_means.csv'), 
       row.names = F)

#### Merge with bond spread data set ####
frt <- import(sprintf('%s/paper/analysis/frt0526.csv', sd))
frt$iso2c <- countrycode(frt$wbcode, origin = 'wb',
                         destination = 'iso2c') 
frt <- frt %>% select(-country)
frt$country <- countrycode(frt$iso2c, origin = 'iso2c', 
                           destination = 'country.name')

comb <- merge(frt, gathered, all.x = T)
comb <- MoveFront(comb, c('iso2c', 'ccode1', 'country', 'year', 'frt', 'dfrt',
                          'lfrt', 'hrv_mean', 'dhrv_mean', 'lhrv_mean')) %>%
            arrange(country, year) %>%
            select(-sname, -wbcode)

# Save
export(comb, file = paste0(sd, 'source/Hollyer_et_al_Compare/frt_hrv_bond.csv'),
       na = '')


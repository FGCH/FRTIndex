################################################################################
# Extract mean Hollyer et al. Transparency Scores
# Christopher Gandrud
# 28 January 2015
# MIT License
# Original estimates downloaded from: http://hrvtransparency.org/
################################################################################

library(dplyr)
library(rjags)
library(tidyr)
library(countrycode)
library(DataCombine)
library(ggplot2)

# Set working directory. Change as needed.
setwd('~/Desktop/')

# Load jags result
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

#### Merge with bond spread data set ####
frt <- read.csv('frt_bondspread_28Jan.csv', stringsAsFactors = F)
frt$iso2c <- countrycode(frt$wbcode, origin = 'wb',
                         destination = 'iso2c') 
frt <- frt %>% select(-country)
frt$country <- countrycode(frt$iso2c, origin = 'iso2c', 
                           destination = 'country.name')

comb <- merge(frt, gathered, all.x = T)
comb <- MoveFront(comb, c('iso2c', 'ccode1', 'country', 'year', 'frt', 'dfrt',
                          'lfrt', 'hrv_mean', 'dhrv_mean', 'lhrv_mean')) %>%
            arrange(country, year) %>%
            select(-X_merge, -sname, -wbcode)

# Save
write.dta(comb, file = 'frt_hrv_bond.dta')

#### Plot comparison to FRT ####
comb_noca <- comb %>% filter(country != 'Canada')

ggplot(comb, aes(log(frt), mean_hrv)) +
    geom_point() +
    stat_smooth() +
    theme_bw()


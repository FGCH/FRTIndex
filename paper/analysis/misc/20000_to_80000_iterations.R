# ---------------------------------------------------------------------------- #
# Replace FRT Index run with 20,000 iterations with 80,000 iterations, 
# high income countries
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load packages
library(rio)
library(dplyr)
library(DataCombine)

# Import Index run with 20,000 iterations
frt_old <- import('/git_repositories/FRTIndex/paper/analysis/frt_hrv_obi_bond.dta')
frt_old <- frt_old %>% select(-frt, -dfrt, -lfrt)

# Import Index run with 80,000 iterations
URL <- 'https://raw.githubusercontent.com/FGCH/FRTIndex/b186e949d3c4ca4f69d7adc3b4d76aee9b18fa4b/IndexData/FRTIndex.csv'
frt_new <- import(URL)

frt_new <- frt_new %>% select(iso2c, year, median) %>% rename(frt = median)

## Move Index up by 0.00001 to avoid undefined delta
frt_new$frt <- frt_new$frt + 0.001

frt_new <- slide(frt_new, Var = 'frt', TimeVar = 'year', GroupVar = 'iso2c',
                   NewVar = 'lfrt')
frt_new$dfrt <- frt_new$frt - frt_new$lfrt

# Merge
frt <- merge(frt_new, frt_old, by = c('iso2c', 'year'))
frt <- MoveFront(frt, c('country', 'ccode1'))

foreign::write.dta(frt, 
       file = '/git_repositories/FRTIndex/paper/analysis/frt_hrv_obi_bond_80000.dta')

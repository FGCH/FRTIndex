################################################################################
# Add in OBI fiscal transparency indicators
# Christopher Gandrud
# MIT License
################################################################################

# Load packages
library(dplyr)
library(countrycode)
library(foreign)
library(DataCombine)
library(repmis)
library(WDI)
library(MASS)
library(rio)

# Set working directory. Change as needed
setwd('/git_repositories/FRTIndex/')

# Load Alt et al. Fiscal Transparency Data
fiscal <- 'source/FiscalTranparency/Alt_et_al_aggregated_fiscal_transparency.csv' %>%
        read.csv(stringsAsFactors = F)

fiscal$iso2c <- countrycode(fiscal$worldbank_code, origin = 'wb',
                            destination = 'iso2c')
fiscal <- fiscal %>% dplyr::select(iso2c, year, obi3)

# Create lags/percent change
fiscal <- slide(fiscal, Var = 'obi3', TimeVar = 'year', GroupVar = 'iso2c',
                NewVar = 'lobi3')
fiscal <- PercChange(fiscal, Var = 'obi3', TimeVar = 'year', GroupVar = 'iso2c',
                     NewVar = 'dobi3', type = 'proportion')

# Raw OBI index
obi <- read.csv('source/FiscalTranparency/ibp_data_summary.csv',
                stringsAsFactors = F) %>%
                dplyr::select(-RANK)
names(obi) <- c('iso2c', 'year', 'obi_raw')


# Basic comparison
URL <- 'https://raw.githubusercontent.com/FGCH/FRTIndex/master/IndexData/FRTIndex.csv'
frt_index <- rio::import(URL)
raw_obi_frt <- merge(frt_index, obi, by = c('iso2c', 'year'))

length(unique(raw_obi_frt$country))
cor.test(raw_obi_frt$median, raw_obi_frt$obi_raw)

# Expand over time
obi <- TimeExpand(obi, GroupVar = 'iso2c', TimeVar = 'year')
obi <- obi %>% group_by(iso2c) %>%
            mutate(obi_filled = FillDown(Var = obi_raw)) %>%
            dplyr::select (-obi_raw)
obi <- subset(obi, !is.na(iso2c))
class(obi) <- 'data.frame'

# Create lags/percent change
obi <- slide(obi, Var = 'obi_filled', TimeVar = 'year', GroupVar = 'iso2c',
                NewVar = 'lobi_filled')
obi <- PercChange(obi, Var = 'obi_filled', TimeVar = 'year', GroupVar = 'iso2c',
                 NewVar = 'dobi_filled', type = 'proportion')

#### GDP Growth Data
growth <- WDI(indicator = 'NY.GDP.MKTP.KD.ZG', start = 1985, end = 2012) %>%
            rename(country_growth = NY.GDP.MKTP.KD.ZG) %>%
            dplyr::select(-country)

# Create lags/percent change
growth <- slide(growth, Var = 'country_growth', TimeVar = 'year', GroupVar = 'iso2c',
             NewVar = 'lcountry_growth')
growth <- PercChange(growth, Var = 'country_growth', TimeVar = 'year', GroupVar = 'iso2c',
                  NewVar = 'dcountry_growth', type = 'proportion')

#### Load main data
main <- read.dta('source/Hollyer_et_al_Compare/frt_hrv_bond.dta')

# Merge in fiscal transparency data
comb <- merge(main, fiscal, all.x = T)
comb <- merge(comb, obi, all.x = T)
comb <- merge(comb, growth, all.x = T)

comb <- comb %>% MoveFront(c('iso2c', 'year',  'ccode1', 'country', 'frt',
                             'dfrt', 'lfrt', 'hrv_mean', 'dhrv_mean',
                             'lhrv_mean', 'obi3', 'lobi3', 'dobi3',
                             'obi_filled', 'lobi_filled', 'dobi_filled'))

#### Add Euro membership
euro <- 'http://bit.ly/1yRvycq' %>% source_data()
euro$eurozone <- 1

# Merge
comb <- merge(comb, euro, all.x = T)
comb$eurozone[is.na(comb$eurozone)] <- 0

# Save
export(comb, file = 'paper/analysis/frt_hrv_obi_bond.csv')

#### Correlation with other transparency measures
cor.test(comb$frt, comb$obi_filled)

cor.test(comb$hrv, comb$obi_filled)

#### Create parellel coordinates plots to highlight differences ####
# Used in presentation

sub_2010 <- comb %>% filter(year == 2010) %>%
                dplyr::select(frt, hrv_mean, obi_filled)

names(sub_2010) <- c('Financial (FRT)', 'General (HRV)', 'Fiscal (OBI)')

pdf(file = 'paper/paper_plots/frt_hrv_obi_compare.pdf', width = 12, height =  8)
    parcoord(sub_2010, var.label = T)
dev.off()

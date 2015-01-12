############################
# Extract FRT Point Estimates and uncertainty
# Christopher Gandrud
# 12 January 2015
# MIT License
############################

# Load packages
if (!('StanCat' %in% installed.packages()[, 1])) devtools::install_github('christophergandrud/StanCat')
library(StanCat)
if (!('dpmr' %in% installed.packages()[, 1])) devtools::install_github('christophergandrud/dpmr')
library(dpmr)
library(repmis)
library(tidyr)
library(dplyr)
library(DataCombine)
library(countrycode)

# Set working directory to save index data to. Change as needed
setwd('/git_repositories/FRTIndex')

# Load function to subset the data frame to countries that report
# at least 1 item.
source('source/miscFunctions/report_min_once.R')

# Load data
BaseSub <-
    'https://raw.githubusercontent.com/FGCH/FRTIndex/master/source/RawData/wdi_fred_combined.csv' %>%
    source_data(stringsAsFactors = FALSE)
#### Keep only countries that report at least 1 item for the entire period  ####
BaseSub <- report_min_once(BaseSub)

# Country list from FRT_Stan_v_02_1990.R
countries <- unique(BaseSub$country)

# Load simulations
load('/Volumes/GANDRUD32/FRT/fit_2014-12-18.RData')

# Years
years <- 1990:2011

# Convert simulations to data.frame
fit_df <- as.data.frame(fit)
rm(fit)

# Keep only FRT scores (alpha)
fit_df_sub <- fit_df[, grep(pattern = 'alpha', x = names(fit_df))]

# Melt into long format and group
gathered <- gather(fit_df_sub, variable, value)
gathered <- group_by(gathered, variable)

median <- summarize(gathered, median = median(value))
lower_95 <- summarize(gathered, lower_95 = StanCat:::HPD(value, prob = 0.95,
                                                        side = 'lower'))
lower_90 <- summarize(gathered, lower_90 = StanCat:::HPD(value, prob = 0.9,
                                                        side = 'lower'))
upper_90 <- summarize(gathered, upper_90 = StanCat:::HPD(value, prob = 0.9,
                                                        side = 'upper'))
upper_95 <- summarize(gathered, upper_95 = StanCat:::HPD(value, prob = 0.95,
                                                        side = 'upper'))

comb <- merge(lower_95, lower_90) %>%
            merge(., median) %>%
            merge(., upper_90) %>%
            merge(., upper_95)

for (i in 2:ncol(comb)) comb[, i] <- round(comb[, i], digits = 3)

# Clean up identifiers
fr_country <- data.frame(from = paste0('alpha\\[', 1:length(countries), ',.*'),
                 to = countries)
fr_year <- data.frame(from = paste0('alpha\\[.*,', 1:length(years), '\\]'),
                      to = years)
comb$country <- comb$variable

comb <- FindReplace(comb, Var = 'country', replaceData = fr_country,
                    exact = FALSE)
comb <- FindReplace(comb, Var = 'variable', replaceData = fr_year,
                    exact = FALSE)
comb <- rename(comb, year = variable)

# Add in iso2c codes
comb$iso2c <- countrycode(comb$country, origin = 'country.name',
                          destination = 'iso2c')

# Final cleanup
comb <- MoveFront(comb, c('country', 'iso2c', 'year'))

comb <- arrange(comb, country, year)

# Write file
write.csv(comb, 'IndexData/FRTIndex.csv',
          row.names = FALSE)

# Create data package version
meta_list <- list(name = 'frt_datapackage', 
                  title = 'The Financial Regulatory Transparency Index',
                  version = '0.2.4.1',
                  maintainer = 'Christopher Gandrurd',
                  license = 'PDDL-1.0',
                  last_updated = Sys.Date(),
                  homepage = 'https://github.com/FGCH/FRTIndex')

datapackage_init(comb, meta = meta_list, 
                 source_cleaner = c('source/RawDataGather.R',
                                    'source/FRT_Stan_in_Parallel.R', 
                                    'source/FRT.stan'), 
                 source_cleaner_rename = F)


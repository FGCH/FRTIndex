############################
# Extract FRT Point Estimates and uncertainty
# Christopher Gandrud
# 4 December 2014
# MIT License
############################

# Load packages
library(repmis)
library(reshape2)
library(dplyr)
library(DataCombine)
library(countrycode)

# Load base data # Load data
BaseSub <-
    'https://raw.githubusercontent.com/FGCH/FRTIndex/master/source/RawData/wdi_fred_combined.csv' %>%
    source_data(stringsAsFactors = FALSE)

#### Keep only countries that report at least 1 item for the entire period  ####
binary_vars <- names(BaseSub)[grep('^Rep_', names(BaseSub))]
BaseSub$sums <- rowSums(BaseSub[, binary_vars])
report_zero <- group_by(BaseSub, country) %>%
    summarize(added = sum(sums)) %>%
    subset(., added == 0) %>%
    as.data.frame()

# Subset
BaseSub <- subset(BaseSub, !(country %in% report_zero[, 1]))

# Country list from FRT_Stan_v_02_1990.R
countries <- unique(BaseSub$country)

# Load simulations
load('~/Desktop/fit_2014-12-9_a.RData')

# Years
years <- 1990:2011

# Convert simulations to data.frame
fit_df <- as.data.frame(fit_NonIndp)
rm(fit_NonIndp)

# Keep only FRT scores (alpha)
fit_df_sub <- fit_df[, grep(pattern = 'alpha', x = names(fit_df))]

# Melt into long format and group
molten_fit <- melt(fit_df_sub)
molten_fit <- group_by(molten_fit, variable)

median <- summarize(molten_fit, median = round(median(value), digits = 3))
lower_95 <- summarize(molten_fit, lower_95 = round(quantile(value,
                                                    probs = 0.025), digits = 3))
lower_90 <- summarize(molten_fit, lower_90 = round(quantile(value, probs = 0.05),
                                                   digits = 3))
upper_90 <- summarize(molten_fit, upper_90 = round(quantile(value, probs = 0.95),
                                                   digits = 3))
upper_95 <- summarize(molten_fit, upper_95 = round(quantile(value, probs = 0.975),
                                                   digits = 3))

comb <- merge(lower_95, lower_90) %>%
            merge(., median) %>%
            merge(., upper_90) %>%
            merge(., upper_95)

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
write.csv(comb, '/git_repositories/FRTIndex/IndexData/FRTIndex.csv',
          row.names = FALSE)

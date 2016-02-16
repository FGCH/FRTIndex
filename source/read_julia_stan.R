############################
# Read in Stan Output created by Julia/CMDStan and extract FRT Point Estimates and uncertainty
# Christopher Gandrud
# MIT License
############################

# Load packages
if (!('StanCat' %in% installed.packages()[, 1])) devtools::install_github('christophergandrud/StanCat')
library(StanCat)
library(dpmr)
library(tidyr)
library(dplyr)
library(DataCombine)
library(countrycode)
library(rio)
library(rstan)
library(repmis)

# Set working directory to save index data to. Change as needed
setwd('/git_repositories/FRTIndex')

# Load function to subset the data frame to countries that report
# at least 1 item.
source('source/miscFunctions/report_min_once.R')

# Load data
BaseSub <-
  'source/RawData/wdi_fred_combined_GFDDv2015.csv' %>%
  import
#### Keep only countries that report at least 1 item for the entire period  ####
BaseSub <- report_min_once(BaseSub)

# Country list from FRT_Stan_v_02_1990.R
countries <- unique(BaseSub$country)

# Load in simulations ------
csv_from_julia <- sprintf('tmp/frt_model_samples_%s.csv', 1:4)

fit <- read_stan_csv(csv_from_julia)


# Years
years <- unique(BaseSub$year)
years <- min(years):max(years)

# Convert simulations to data.frame
fit_df <- as.data.frame(fit)
#rm(fit)

# Keep only FRT scores (alpha)
fit_df_sub <- fit_df[, grep(pattern = 'alpha', x = names(fit_df))]

# Melt into long format and group
gathered <- gather(fit_df_sub, variable, value)
gathered$variable <- gathered$variable %>% as.character
gathered <- group_by(gathered, variable)

median <- dplyr::summarize(gathered, median = median(value))
lower_95 <- dplyr::summarize(gathered, lower_95 = StanCat:::HPD(value, 
                                                                prob = 0.95, 
                                                                side = 'lower'))
lower_90 <- dplyr::summarize(gathered, lower_90 = StanCat:::HPD(value, 
                                                                prob = 0.9, 
                                                                side = 'lower'))
upper_90 <- dplyr::summarize(gathered, upper_90 = StanCat:::HPD(value, 
                                                                prob = 0.9, 
                                                                side = 'upper'))
upper_95 <- dplyr::summarize(gathered, upper_95 = StanCat:::HPD(value, 
                                                                prob = 0.95, 
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
comb <- dplyr::rename(comb, year = variable)

# Add in iso2c codes
comb$iso2c <- countrycode(comb$country, origin = 'country.name',
                          destination = 'iso2c')

# Final cleanup
comb <- MoveFront(comb, c('country', 'iso2c', 'year'))

comb <- arrange(comb, country, year)

# Drop non-country transparency alpha paramaters
comb <- comb %>% DropNA('iso2c')

# Write file
export(comb, 'IndexData/FRTIndex_v2.csv')

# Create data package version
meta_list <- list(name = 'frt_datapackage',
                  title = 'The Financial Regulatory Transparency Index',
                  version = '2.0',
                  maintainer = 'Christopher Gandrurd',
                  license = 'PDDL-1.0',
                  last_updated = Sys.Date(),
                  homepage = 'https://github.com/FGCH/FRTIndex')

datapackage_init(comb, meta = meta_list,
                 source_cleaner = c('source/RawDataGather.R',
                                    'source/FRT_Stan_in_Parallel.R',
                                    'source/frt_stan.jl',
                                    'source/read_julia_stan.R',
                                    'source/FRT.stan'),
                 source_cleaner_rename = F)


# Create plots for non-alpha parameters of interest ------------------------------------
## Not included in frt_plots_v2.R in order to avoid loading fit in more than once, which is
## computationally expensive

# Main figure output directory
dir <- 'paper/paper_plots/'

indicators_df <- import('paper/IndicatorDescript/IndicatorDescription.csv')
indicator_labels <- indicators_df[, 2]

# Difficulty
pdf(file = paste0(dir, 'difficultyPlot.pdf'), width = 10, height = 5.5)
stan_caterpillar(fit, 'beta\\[.*\\]',
                 pars_labels = indicator_labels) +
  ylab('') + xlab('\nCoefficient')
dev.off()

# Discrimination
pdf(file = paste0(dir, 'discriminationPlot.pdf'), width = 10, height = 5.5)
stan_caterpillar(fit, 'gamma\\[.*\\]',
                 pars_labels = indicator_labels) +
  ylab('') + xlab('\nCoefficient')
dev.off()
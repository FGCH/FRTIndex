################################################################################
# FRT Index (using Stan)
# Using only variables that are reported back to 1990
# Christopher Gandrud
# 15 December 2014
# MIT License
################################################################################

#### Credits ----------------------------------------------------------------- #
# The Stan Multilevel 2PL Model is built from the following sources:
# Stan Development Team. 2014. Stan Modeling Language Users Guide and Reference
# Manual, Version 2.5. 49-50. http://mc-stan.org/.
#
# Bafumi, J., Gelman, A., Park, D. K., & Kaplan, N. (2005). Practical Issues in
# Implementing and Understanding Bayesian Ideal Point Estimation. Political
# Analysis, 13(2), 171–187. doi:10.1093/pan/mpi010
#
# Hollyer, James R., B. Peter Rosendorff, and James Raymond Vreeland. 2014.
# "Replication data for: Measuring Transparency".
# http://dx.doi.org/10.7910/DVN/24274
#
# Thanks also to the Stan Users Group:
# - https://groups.google.com/forum/?hl=en#!topic/stan-users/j9Ire8EQObY
# - https://groups.google.com/forum/#!topic/stan-users/oSGUrVFCIVw
# ---------------------------------------------------------------------------- #

## Requires: Stan http://mc-stan.org/

# Load packages
library(DataCombine)
library(reshape2)
library(dplyr)
library(rstan)

# Set working directory. Change as needed.
setwd('/git_repositories/FRTIndex/')

## Set out width
options('width' = 200)

# Load function to subset the data frame to countries that report 
# at least 1 item.
source('source/miscFunctions/report_min_once.R')

#### Gather data ####
# Run if needed.
# Note takes approximately 1 hour
# Otherwise load saved data from CSV

data_source <- 'csv' # switch to 'download' if you wish to download data from
                     # World Bank/FRED

if (data_source == 'csv') BaseSub <- read.csv(
                                        'source/RawData/wdi_fred_combined.csv',
                                        stringsAsFactors = FALSE)

if (data_source == 'download') source('source/RawDataGather.R')

# ---------------------------------------------------------------------------- #
#### Keep only countries that report at least 1 item for the entire period  ####
BaseSub <- report_min_once(BaseSub)

#### Data description ####
# Create country/year numbers
BaseSub$countrynum <- as.numeric(as.factor(BaseSub$iso2c))
BaseSub$yearnum <- as.numeric(as.factor(BaseSub$year))

#### Clean up ####
# Keep only complete variables
binary_vars <- names(BaseSub)[grep('^Rep_', names(BaseSub))]
BaseStanVars <- BaseSub[, c('countrynum', 'yearnum', binary_vars)]

# Data descriptions
NCountry <- max(BaseStanVars$countrynum)
NYear <- max(BaseStanVars$yearnum)
NItems <- length(binary_vars)

# Melt data so that it is easy to enter into Stan data list
MoltenBase <- melt(BaseStanVars, id.vars = c('countrynum', 'yearnum'))

# Convert item names to numeric
MoltenBase$variable <- as.factor(MoltenBase$variable) %>% as.numeric()

# Order data
MoltenReady <- arrange(MoltenBase, countrynum, yearnum, variable)

# ---------------------------------------------------------------------------- #
#### Load Model ####
frt_code <- 'source/FRT.stan'

#### Create data list for Stan ####
frt_data <- list(
    C = NCountry,
    T = NYear,
    K = NItems,
    N = nrow(MoltenReady),
    cc = MoltenReady$countrynum,
    tt = MoltenReady$yearnum,
    kk = MoltenReady$variable,
    y = MoltenReady$value
)

##### Run model ####
fit <- stan(file = frt_code, data = frt_data,
                    iter = 50, chains = 4,
                    pars = c('delta', 'alpha', 'beta', 'log_gamma'),
                    diagnostic_file = paste0(
                        'modelOut/frt_sims_diagnostic', Sys.Date()))

# Save results as data.frame
as.data.frame(fit) %>%
    write.csv(file = paste0('modelOut/frt_sims_', Sys.Date(), '.csv'),
                row.names = FALSE)

# Examine results
print(fit)

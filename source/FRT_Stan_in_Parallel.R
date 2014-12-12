###########################
# Run model in parallel
# Christopher Gandrud
# 11 December 2014
# MIT License
###########################

# Load packages
library(repmis)
library(DataCombine)
library(reshape2)
library(dplyr)
library(rstan)
library(parallel)

## Set out width
options('width' = 200)

# Load data
BaseSub <-
    'https://raw.githubusercontent.com/FGCH/FRTIndex/master/source/RawData/wdi_fred_combined.csv' %>%
    source_data(stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------- #
#### Keep only countries that report at least 1 item for the entire period  ####
binary_vars <- names(BaseSub)[grep('^Rep_', names(BaseSub))]
BaseSub$sums <- rowSums(BaseSub[, binary_vars])
report_zero <- group_by(BaseSub, country) %>%
                        summarize(added = sum(sums)) %>%
                        subset(., added == 0) %>%
                        as.data.frame()

# Subset
BaseSub <- subset(BaseSub, !(country %in% report_zero[, 1]))

#### Data description ####
# Create country/year numbers
BaseSub$countrynum <- as.numeric(as.factor(BaseSub$iso2c))
BaseSub$yearnum <- as.numeric(as.factor(BaseSub$year))

#### Clean up ####
# Keep only complete variables
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
#### Specify Model ####
frt_code <- 'https://raw.githubusercontent.com/FGCH/FRTIndex/usaPriors/source/FRT.stan' %>%
            scan_https()

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


# Create Empty Stan model (so it only needs to compile once)
empty_stan <- stan(model_code = frt_code, data = frt_data, chains = 0)

empty_stan <- stan(file = frt_code, data = frt_data, chains = 0)

# Run on 4 cores
sflist <-
    mclapply(1:4, mc.cores = 4,
        function(i) stan(fit = empty_stan, data = frt_data,
                        seed = i, chains = 1,
                        iter = 3000, chain_id = i,
                        pars = c('delta', 'alpha', 'beta', 'log_gamma'),
                        diagnostic_file = paste0(
                            'frt_sims_diagnostic', Sys.Date())
        )
    )

# Collect in to Stan fit object
fit <- sflist2stanfit(sflist)

# Save Stan fit object
save(fit, file = paste0('fit_', Sys.Date(), '.RData'))

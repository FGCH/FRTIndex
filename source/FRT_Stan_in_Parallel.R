###########################
# Run model in parallel
# Christopher Gandrud
# 9 December 2014
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
    source_data()

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
    MoltenBase$variable <- as.numeric(as.factor(MoltenBase$variable))

    # Order data
    MoltenReady <- arrange(MoltenBase, countrynum, yearnum, variable)

# ---------------------------------------------------------------------------- #
#### Specify Model ####
frt_code <- "
    data {
        int<lower=1> C;                // number of countries
        int<lower=1> T;                // number of years
        int<lower=1> K;                // number of items
        int<lower=1> N;                // number of observations
        int<lower=1> cc[N];            // country for observation n
        int<lower=1> tt[N];            // time for observation n
        int<lower=1,upper=K> kk[N];    // item for observation n
        int<lower=0,upper=1> y[N];     // response for observation n
    }

    parameters {
        real delta;                // mean transparency
        vector[C] alpha1;          // initial alpha for t = 1 before recentering
        matrix[C,T] alpha;         // transparency for c,t - mean
        vector[K] beta;            // difficulty of item k
        vector[K] log_gamma;       // discrimination of k

        //// all scale parameters have an implicit half Cauchy prior ////
        real<lower=0> sigma_alpha[C];     // scale of abilities, per country
        real<lower=0> sigma_beta;         // scale of difficulties
        real<lower=0> sigma_gamma;        // scale of log discrimination
    }

    transformed parameters {
        //// recenters transparency for t = 1 ////
        vector[C] recentered_alpha1;
        real mean_alpha1;
        real<lower=0> sd_alpha1;

        mean_alpha1 <- mean(alpha1);
        sd_alpha1 <- sd(alpha1);
        for (c in 1:C)
        recentered_alpha1[c] <- ( alpha1[c] - mean_alpha1 ) / sd_alpha1;
    }

    model {
        alpha1 ~ normal(0,1);   // informed constraints on the ability
        // numerical issues with larger sd
        for (c in 1:C) {
            alpha[c,1] ~ normal(recentered_alpha1[c], 0.001);   // horrible hack
            for (t in 2:T)
            alpha[c,t] ~ normal(alpha[c,t-1], sigma_alpha[c]);
        }

        beta ~ normal(0,sigma_beta);
        log_gamma ~ normal(0,sigma_gamma);
        delta ~ cauchy(0,0.25);

        sigma_alpha ~ cauchy(0,0.25);
        sigma_beta ~ cauchy(0,0.25);
        sigma_gamma ~ cauchy(0,0.25);

        for (n in 1:N)
        y[n] ~ bernoulli_logit(
            exp(log_gamma[kk[n]])
            * (alpha[cc[n],tt[n]] - beta[kk[n]] + delta) );
    }
"

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

# Run on 2 cores
sflist <-
    mclapply(1:4, mc.cores = 4,
            function(i) stan(fit = empty_stan, data = frt_data,
                            seed = i, chains = 1,
                            iter = 500,
                            chain_id = i))

# Collect in to Stan fit object
fit <- sflist2stanfit(sflist)

# Save Stan fit object
save(fit, file = paste0('fit_', Sys.Date(), '.RData'))

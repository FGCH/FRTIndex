################################################################################
# FRT Index (using Stan)
# Using only variables that are reported back to 1990
# Christopher Gandrud
# 4 December 2014
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
library(WDI)
library(DataCombine)
library(reshape2)
library(dplyr)
library(rstan)

## Set out width
options('width' = 200)

# Set working directory for saving the simulation output. Change as needed.
setwd('/git_repositories/FRTIndex/')

# ---------------------------------------------------------------------------- #
#### Create Indicator Data Set ####
# Download GFDD data from the World Bank
Indicators <- c('GFDD.DI.01', 'GFDD.DI.03', 'GFDD.DI.04',
                'GFDD.DI.05', 'GFDD.DI.06', 'GFDD.DI.07',
                'GFDD.DI.08', 'GFDD.DI.11', 'GFDD.DI.14',
                'GFDD.EI.02', 'GFDD.EI.08', 'GFDD.OI.02',
                'GFDD.OI.07', 'GFDD.SI.04')

# Download indicators
Base <- WDI(indicator = Indicators, start = 1990, end = 2013, extra = TRUE)

# Keep countries with 'High income' (OECD and non-OECD classification)
BaseSub <- grepl.sub(data = Base, Var = 'income', pattern = 'High income')
Droppers <- c("iso3c", "region",  "capital", "longitude", "latitude",
              "income", "lending")
BaseSub <- BaseSub[, !(names(BaseSub) %in% Droppers)]

# ---------------------------------------------------------------------------- #
#### Create missingness indicators ####
KeeperLength <- length(Indicators)
IndSub <- Indicators[1:KeeperLength]
VarVec <- vector()

for (i in IndSub){
    BaseSub[, paste0('Rep_', i)] <- 1
    BaseSub[, paste0('Rep_', i)][is.na(BaseSub[, i])] <- 0

    temp <- paste0('Rep_', i)
    VarVec <- append(VarVec, temp)
}

# ---------------------------------------------------------------------------- #
#### Manually correct World Bank missingness error for UK Bank Deposits/GDP ####
# Data was reported from 1960 through 2009 and is available at:
# http://research.stlouisfed.org/fred2/series/DDOI02GBA156NWDB
# Accessed December 2014
BaseSub[, 'Rep_GFDD.OI.02'][BaseSub$country == 'United Kingdom' &
                                BaseSub$year <= 2009] <- 1

# ---------------------------------------------------------------------------- #
#### Find the proportion of items reported ####
source('source/miscFunctions/PropReported.R')
PropRepor <- PropReported(BaseSub)
PropRepor <- PropRepor[order(PropRepor$country, PropRepor$year), ]
write.csv(PropRepor, file = paste0('IndexData/alternate/PropReported.csv'),
          row.names = FALSE)

# ---------------------------------------------------------------------------- #
#### Data description ####
# Create country/year numbers
BaseSub$countrynum <- as.numeric(as.factor(BaseSub$iso2c))
BaseSub$yearnum <- as.numeric(as.factor(BaseSub$year))

#### Clean up ####
# Keep only complete variables
BaseStanVars <- BaseSub[, c('countrynum', 'yearnum', VarVec)]

# Data descriptions
NCountry <- max(BaseStanVars$countrynum)
NYear <- max(BaseStanVars$yearnum)
NItems <- length(VarVec)

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
        real delta;                    // mean transparency
        vector[C] alpha1;              // initial alpha for t = 1 before recentering
        matrix[C,T] alpha;             // transparency for c,t - mean
        vector[K] beta;                // difficulty of item k
        vector[K] log_gamma;           // discrimination of k

        // all scale parameters have an implicit half Cauchy prior
        real<lower=0> sigma_alpha[C];     // scale of abilities, per country
        real<lower=0> sigma_beta;         // scale of difficulties
        real<lower=0> sigma_gamma;        // scale of log discrimination
    }

    transformed parameters {
        // recenters transparency for t = 1
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

##### Run model ####
fit_NonIndp <- stan(model_code = frt_code, data = frt_data,
                    iter = 50, chains = 4,
                    pars = c('delta', 'alpha', 'beta', 'log_gamma'),
                    diagnostic_file = paste0(
                        'modelOut/frt_sims_diagnostic', Sys.Date()))

# Save results as data.frame
as.data.frame(fit_NonIndp) %>%
    write.csv(file = paste0('frt_sims_', Sys.Date(), '.csv'), row.names = FALSE)

# Examine results
print(fit_NonIndp)

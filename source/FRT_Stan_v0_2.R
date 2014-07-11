##############
# FRT Index Stan Test (version 0.2): Adding time
# Christopher Gandrud
# 11 July 2014
# MIT License
##############

#### Credits ----------------------------------------------------------------- #
# The Stan model is built on two sources:
# The Multilevel 2PL Model from
# Stan Development Team. 2014. Stan Modeling Language Users Guide and Reference
# Manual, Version 2.3. 32-35. http://mc-stan.org/.
#
# Hollyer, James R., B. Peter Rosendorff, and James Raymond Vreeland. 2014. 
# "Replication data for: Measuring Transparency". 
# http://dx.doi.org/10.7910/DVN/24274
# 
# Thanks also to the Stan Users Group for syntax assistance:
# https://groups.google.com/forum/?hl=en#!topic/stan-users/j9Ire8EQObY
# ---------------------------------------------------------------------------- #

# Load packages
library(WDI)
library(DataCombine)
library(reshape2)
library(dplyr)
library(rstan)

# ---------------------------------------------------------------------------- #
#### Create Indicator Data Set ####
# Download GFDD data from the World Bank
Indicators <- c('GFDD.DI.01', 'GFDD.DI.03', 'GFDD.DI.04', 'GFDD.DI.05', 
                'GFDD.DI.06', 'GFDD.DI.07', 'GFDD.DI.08',
                'GFDD.DI.11', 'GFDD.DI.13', 'GFDD.DI.14',
                'GFDD.EI.02', 'GFDD.EI.08', 'GFDD.OI.02', 
                'GFDD.OI.07', 'GFDD.SI.02', 'GFDD.SI.03', 'GFDD.SI.04', 
                'GFDD.SI.05', 'GFDD.SI.07')

# Download indicators
Base <- WDI(indicator = Indicators, start = 1998, end = 2011, extra = TRUE)

# Keep countries with 'High income' (OECD and non-OECD classification)
BaseSub <- grepl.sub(data = Base, Var = 'income', pattern = 'High income')
Droppers <- c("iso3c", "region",  "capital", "longitude", "latitude",
              "income", "lending")
BaseSub <- BaseSub[, !(names(BaseSub) %in% Droppers)]

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
frt_code <- '
    data {
        int<lower=1> J;                // number of countries
        int<lower=1> T;                // number of years
        int<lower=1> K;                // number of items
        int<lower=1> N;                // number of observations
        int<lower=1> jj[N];            // country for observation n
        int<lower=1> tt[N];            // time for observation n
        int<lower=1,upper=K> kk[N];    // question for observation n
        int<lower=0,upper=1> y[N];     // correctness for observation n
    }

    parameters {
        real delta;                    // mean transparency
        real alpha[J,T];               // transparency for j,t - mean
        real beta[K];                  // difficulty of item k
        real log_gamma[K];             // discrimination of k
        real<lower=0> sigma_alpha;     // scale of abilities
        real<lower=0> sigma_beta;      // scale of difficulties
        real<lower=0> sigma_gamma;     // scale of log discrimiation
    }

    model {
        for (j in 1:J)
            for (t in 1:T)
                alpha[j, t] ~ normal(0, sigma_alpha);
        beta ~ normal(0,sigma_beta);
        log_gamma ~ normal(0,sigma_gamma);
        delta ~ cauchy(0,5);
        sigma_alpha ~ cauchy(0,5);
        sigma_beta ~ cauchy(0,5);
        sigma_gamma ~ cauchy(0,5);
        for (n in 1:N)
            y[n] ~ bernoulli_logit( exp(log_gamma[kk[n]])
                                * (alpha[jj[n],tt[n]] - beta[kk[n]] + delta) );
    }
'

#### Create data list for Stan ####
frt_data <- list(
    J = NCountry,
    T = NYear,
    K = NItems,
    N = nrow(MoltenReady),
    jj = MoltenReady$countrynum,
    tt = MoltenReady$yearnum,
    kk = MoltenReady$variable,
    y = MoltenReady$value
)

##### Run model ####
fit1 <- stan(model_code = frt_code, data = frt_data, iter = 100, chains = 4)

# Examine results
print(fit1)
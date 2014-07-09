##############
# FRT Index Stan Test
# Christopher Gandrud
# 9 July 2014
##############

# Load packages
library(WDI)
library(DataCombine)
library(rstan)

# --------------------------------------------------- #
#### Create Indicator Data Set ####
# Download GFDD data
Indicators <- c('GFDD.DI.01', 'GFDD.DI.03', 'GFDD.DI.04',
                'GFDD.DI.05', 'GFDD.DI.06', 'GFDD.DI.07', 'GFDD.DI.08',
                'GFDD.DI.11', 'GFDD.DI.13', 'GFDD.DI.14',
                'GFDD.EI.02', 'GFDD.EI.08', 'GFDD.OI.02', 'GFDD.OI.07',
                'GFDD.SI.02', 'GFDD.SI.03', 'GFDD.SI.04', 'GFDD.SI.05',
                'GFDD.SI.07')

# Download indicators
# Unable to download 'GFDD.DM.011', 'GFDD.OI.14'
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
BaseStanReady <- BaseSub[, c('countrynum', 'yearnum', VarVec)]

# Create vector of parameters to estimate
Betas <- paste0('beta', VarCount)
ParamsEst <- c("transparency", "tau", Betas)
NVar <- 1:length(VarVec)
NObs <- nrow(BaseStanReady)
NCountry <- max(BaseStanReady$countrynum)
NYear <- max(BaseStanReady$yearnum)

# --------------------------------------------------- #
#### Specify Model ###

frt_code <- paste(
    'data {
    int<
        '
    )

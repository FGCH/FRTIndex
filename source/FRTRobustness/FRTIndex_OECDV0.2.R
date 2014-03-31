##############
# Financial Regulatory Transparency Index OECD V0.2
# Christopher Gandrud
# 31 March 2014
#############

## Inspired by:
# Hollyer, James R. ; Rosendorff, B. Peter; Vreeland, James Raymond, 2014,
# "Replication data for: Measuring Transparency",
# http://dx.doi.org/10.7910/DVN/24274 UNF:5:Mj2tZ0rivXJPhuFqdQS0JA==
# IQSS Dataverse Network [Distributor] V1 [Version]

# --------------------------------------------------- #
setwd('~/FRTOutFiles/')
GitDir <- '/git_repositories/FRTIndex/'
GitDir <- '/home/cjg/FRTIndex/'

# Load packages
library(WDI)
library(DataCombine)
library(rjags)
library(ggmcmc)
source(paste0(GitDir, 'source/miscFunctions/PropReported.R'))

# --------------------------------------------------- #
#### Create Indicator Data Set ####
# Download GFDD data
Indicators <- c('GFDD.DI.01', 'GFDD.DI.02', 'GFDD.DI.03', 'GFDD.DI.04',
                'GFDD.DI.05', 'GFDD.DI.06', 'GFDD.DI.07', 'GFDD.DI.08',
                'GFDD.DI.11', 'GFDD.DI.12', 'GFDD.DI.13', 'GFDD.DI.14',
                'GFDD.EI.08', 'GFDD.OI.02', 'GFDD.SI.02', 'GFDD.SI.03', 
                'GFDD.SI.04', 'GFDD.SI.05', 'GFDD.SI.07')

# Download indicators
# Unable to download 'GFDD.DM.011', 'GFDD.OI.14'
Base <- WDI(indicator = Indicators, start = 1998, end = 2011, extra = TRUE)

# Keep countries with 'High income' (OECD) classification
BaseSub <- grepl.sub(data = Base, Var = 'income', patterns = 'High income: OECD')
Droppers <- c("iso3c", "region",  "capital", "longitude", "latitude",
              "income", "lending")
BaseSub <- BaseSub[, !(names(BaseSub) %in% Droppers)]

# --------------------------------------------------- #
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

# --------------------------------------------------- #
#### Find the proportion of items reported ####
PropRepor <- PropReported(BaseSub)
PropRepor <- PropRepor[order(PropRepor$country, PropRepor$year), ]
write.csv(PropRepor, file = paste0(GitDir, 
          'IndexData/alternate/PropReportedOECD.csv'))

# --------------------------------------------------- #
#### Data description ####
# Create country numbers
BaseSub$countrynum <- as.numeric(as.factor(BaseSub$iso2c))
BaseSub$yearnum <- as.numeric(as.factor(BaseSub$year))

# Create keys
CountryKey <- BaseSub[, c('countrynum', 'iso2c', 'country')]
CountryKey <- CountryKey[!duplicated(CountryKey$countrynum), ]

YearKey <- BaseSub[, c('yearnum', 'year')]
YearKey <- YearKey[!duplicated(YearKey$yearnum), ]

IndKey <- data.frame(IndID = 1:length(Indicators), Indicator = Indicators)

# Country name/number list
write.csv(CountryKey, row.names = FALSE,
  file = paste0(GitDir, 'source/ParameterDescript/CountryNumbersOECD.csv'))
write.csv(YearKey, row.names = FALSE,
          file = paste0(GitDir, 'source/ParameterDescript/YearNumbersOECD.csv'))

# --------------------------------------------------- #
#### Clean up ####
# Keep only complete variables
BaseJagsReady <- BaseSub[, c('countrynum', 'yearnum', VarVec)]

# Create vector of parameters to estimate
VarCount <- 1:length(VarVec)
Betas <- paste0('beta', VarCount)
ParamsEst <- c("transparency", "tau", Betas)
Num <- nrow(BaseJagsReady)
NCountry <- max(BaseJagsReady$countrynum)
Nyear <- max(BaseJagsReady$yearnum)

# --------------------------------------------------- #
#### Write JAGS model ####
Xs <- as.character()
Ps <- as.character()
Qs <- as.character()
Vs <- as.character()
Bs <- as.character()

for (n in VarCount){
  temp <- paste0('x', n, '[n] <- beta', n,
            '[1] + transparency[countrynum[n], yearnum[n]]*beta', n,'[2]')
  Xs <- paste(Xs, temp, sep = '\n')

  temp <- paste0('p', n, '[n] <- 1/(1 + exp(-x', n, '[n]))')
  Ps <- paste(Ps, temp, sep = '\n')

  temp <- paste0('q', n, '[n] <- max(min(p', n, '[n],1),0)')
  Qs <- paste(Qs, temp, sep = '\n')

  temp <- paste0(VarVec[n], '[n]~dbern(q', n, '[n])')
  Vs <- paste(Vs, temp, sep = '\n')

  temp <- paste0('beta', n, '[1:2] ~ dmnorm(mu[1:2],alpha[1:2,1:2])')
  Bs <- paste(Bs, temp, sep = '\n')

}

cat(paste0('
model{
    for (n in 1:', Num, '){\n',
      Xs, '\n',
      Ps, '\n',
      Qs, '\n',
      Vs,
    '\n }',
  '\n# Model priors\n',
    '
mu[1] <- 0
mu[2] <- 0

alpha[1,1] <- 0.01
alpha[1,2] <- 0
alpha[2,1] <- 0
alpha[2,2] <- 0.01\n',

Bs,

'\n\n# Transparency priors\n',

paste0('
  for (n in 1:', NCountry, '){
    transinit[n] ~ dnorm(0, 0.01)
  }
  mean.trans <- mean(transinit[1:', NCountry,'])
  sd.trans <- sd(transinit[1:', NCountry, '])

  for (n in 1:', NCountry, '){
    transcentered[n] <- (transinit[n] - mean.trans)*pow(sd.trans, 1)
  }

  for (n in 1:', NCountry, '){
    transparency[n,1] <-transcentered[n]
    tau[n] ~ dgamma(20, 4)

    for (j in 2:', Nyear, '){
      transparency[n,j] ~ dnorm(transparency[n,(j-1)], tau[n])
    }
  }'),
'\n}'), file = 'OECDModel_V0.2.bug')

# Copy file into git repo for version control
file.copy(from = 'OECDModel_V0.2.bug',
          to = '/home/cjg/FRTIndex/source/FRTRobustness/OECDModel_V0.2.bug',
          overwrite = TRUE)

# --------------------------------------------------- #
#### Run JAGS Model ####
# Create list of data objects used by the model
# DataList <- append(list('countrynum', 'year'), as.list(VarVec))

DataList <- list('countrynum' = BaseJagsReady$countrynum,
                 'yearnum' = BaseJagsReady$yearnum,
                 'Rep_GFDD.DI.01' = BaseJagsReady$Rep_GFDD.DI.01,
                 'Rep_GFDD.DI.02' = BaseJagsReady$Rep_GFDD.DI.02,
                 'Rep_GFDD.DI.03' = BaseJagsReady$Rep_GFDD.DI.03,
                 'Rep_GFDD.DI.04' = BaseJagsReady$Rep_GFDD.DI.04,
                 'Rep_GFDD.DI.05' = BaseJagsReady$Rep_GFDD.DI.05,
                 'Rep_GFDD.DI.06' = BaseJagsReady$Rep_GFDD.DI.06,
                 'Rep_GFDD.DI.07' = BaseJagsReady$Rep_GFDD.DI.07,
                 'Rep_GFDD.DI.08' = BaseJagsReady$Rep_GFDD.DI.08,
                 'Rep_GFDD.DI.11' = BaseJagsReady$Rep_GFDD.DI.11,
                 'Rep_GFDD.DI.12' = BaseJagsReady$Rep_GFDD.DI.12,
                 'Rep_GFDD.DI.13' = BaseJagsReady$Rep_GFDD.DI.13,
                 'Rep_GFDD.DI.14' = BaseJagsReady$Rep_GFDD.DI.14,
                 'Rep_GFDD.EI.08' = BaseJagsReady$Rep_GFDD.EI.08,
                 'Rep_GFDD.OI.02' = BaseJagsReady$Rep_GFDD.OI.02,
                 'Rep_GFDD.SI.02' = BaseJagsReady$Rep_GFDD.SI.02,
                 'Rep_GFDD.SI.03' = BaseJagsReady$Rep_GFDD.SI.03,
                 'Rep_GFDD.SI.04' = BaseJagsReady$Rep_GFDD.SI.04,
                 'Rep_GFDD.SI.05' = BaseJagsReady$Rep_GFDD.SI.05,
                 'Rep_GFDD.SI.07' = BaseJagsReady$Rep_GFDD.SI.07)

# Betas <- paste0('beta', VarCount)
parameters <- c("transparency", "tau", Betas)

# Compile model
system.time(
  EstOECD <- jags.model('OECDModel_V0.2.bug', data = DataList,
                   n.chains = 2, n.adapt = 5000)
)
save.image(file = 'OECDSampOut.RData')

# Draw random samples from the posterior
system.time(
  SampOECD <- coda.samples(EstOECD, parameters, n.iter = 1000)
)

# Convert to ggs data frame and save
system.time(
  Set <- ggs(Samp1)
)
save(Set, file = 'SetOutOECD.RData')
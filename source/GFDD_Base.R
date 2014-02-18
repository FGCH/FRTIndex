##############
# Play with Global Financial Development Data
# Christopher Gandrud
# 18 February 2014
#############

## Inspired by:
# Hollyer, James R. ; Rosendorff, B. Peter; Vreeland, James Raymond, 2014, 
# "Replication data for: Measuring Transparency", 
# http://dx.doi.org/10.7910/DVN/24274 UNF:5:Mj2tZ0rivXJPhuFqdQS0JA== IQSS Dataverse Network 
# [Distributor] V1 [Version]

setwd('/git_repositories/FRTIndex/source/')

# Load packages
library(WDI)
library(DataCombine)
library(arm)
library(rjags)
library(R2jags)

# Download GFDD data
Indicators <- c('GFDD.AI.01', 'GFDD.AI.02', 'GFDD.AM.03', 'GFDD.DI.01', 'GFDD.DI.02', 'GFDD.DI.03', 'GFDD.DI.04',
                'GFDD.DI.05', 'GFDD.DI.06', 
                'GFDD.OI.19') # final value is Laeven and Valencia crisis variable

Base <- WDI(indicator = Indicators, start = 2005)

# Use Laeven and Valencia data as an indicator of whether or not observation is a country
BaseSub <- DropNA(Base, 'GFDD.OI.19')

# Create missingness indicators
Last <- length(Indicators)
IndSub <- Indicators[-Last]
VarVec <- vector()

for (i in IndSub){
  BaseSub[, paste0('Reported_', i)] <- 1
  BaseSub[, paste0('Reported_', i)][is.na(BaseSub[, i])] <- 0
  
  temp <- paste0('Reported_', i)
  VarVec <- append(VarVec, temp)
}

# Create country numbers
BaseSub$countrynum <- as.numeric(as.factor(BaseSub$iso2c)) 
BaseSub$year <- as.numeric(as.factor(BaseSub$year)) 


# Keep only complete variables
BaseJagsReady <- BaseSub[, c('countrynum', 'year', VarVec)]

# Create vector of parameters to estimate
VarCount <- 1:length(VarVec)
Betas <- paste0('beta', VarCount)
ParamsEst <- c("transparency", "tau", Betas)
Num <- nrow(BaseJagsReady)
NCountry <- max(BaseJagsReady$countrynum)
Nyear <- max(BaseJagsReady$year)

# Write JAGS model
Xs <- as.character()
Ps <- as.character()
Qs <- as.character()
Vs <- as.character()
Bs <- as.character()

for (n in VarCount){
  temp <- paste0('x', n, '[n] <- beta', n, '[1] + transparency[countrynum[n], year[n]]*beta', n,'[2]')
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
    transcentered[n] <- (transinit[n] - mean.trans)*pow(sd.trans, -1)
  }

  for (n in 1:', NCountry, '){
    transparency[n,1] <-transcentered[n]
    tau[n] ~ dgamma(20, 4)
  
    for (j in 2:', Nyear, '){
      transparency[n,j] ~ dnorm(transparency[n,(j-1)], tau[n])
    }
  }'), 
'\n}'), file = 'BasicModel1.bug')



attach(BaseJagsReady)
# Create list of data objects used by the model
countrynum <- BaseJagsReady$countrynum
year <- BaseJagsReady$year

DataList <- append(list('countrynum', 'year'), as.list(VarVec))

DataList <- list('countrynum' = countrynum, 'year' = year, 
                        "Reported_GFDD.AI.01" = BaseJagsReady$Reported_GFDD.AI.01,
                        "Reported_GFDD.AI.02" = BaseJagsReady$Reported_GFDD.AI.02,
                        "Reported_GFDD.AM.03" = BaseJagsReady$Reported_GFDD.AM.03,
                        'Reported_GFDD.DI.01' = BaseJagsReady$Reported_GFDD.DI.01,
                        'Reported_GFDD.DI.02' = BaseJagsReady$Reported_GFDD.DI.02,
                        'Reported_GFDD.DI.03' = BaseJagsReady$Reported_GFDD.DI.03,
                        'Reported_GFDD.DI.04' = BaseJagsReady$Reported_GFDD.DI.04,
                        'Reported_GFDD.DI.05' = BaseJagsReady$Reported_GFDD.DI.05,
                        'Reported_GFDD.DI.06' = BaseJagsReady$Reported_GFDD.DI.06)


Betas <- paste0('beta', VarCount)
parameters <- c("transparency", "tau", Betas)

# Send to JAGS
Est1 <- jags(data = DataList, inits = NULL, parameters, model.file = "BasicModel1.bug",
             n.chains = 2, n.iter = 1000, n.burnin = 50)

Est1 <- jags.model('BasicModel1.bug', data = DataList)

update(Est1, 1000)

jags.samples(Est1, parameters, 1000)

detach(BaseJagsReady)


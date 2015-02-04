################################################################################
# Visualise main effect in ECM model
# Christopher Gandrud
# 4 February 2015
# MIT License
################################################################################

# Load packages
library(foreign)
library(plm)
library(effects)

# Set working directory. Change as needed.
setwd('/git_repositories/FRTIndex/paper/')

# Load data
main <- read.dta('analysis/frt_hrv_obi_bond.dta')

# Run model
main$iso2c <- as.factor(main$iso2c)
main_noca <- subset(main, country != "Canada")

##### FRT & Volatility #####
m1 <- lm(dratecov ~ lltratecov + lfrt + dfrt + lpubdebtgdp +
                  dpubdebtgdp + linfl + dinfl + lus3mrate + dus3mrate + 
                  loecdgrowth + doecdgrowth + lvix + dvix + iso2c,
            data = main_noca)

# Estimate effect
m1_e <- Effect('lfrt', m1)
plot(m1_e, xlab = 'Financial Regulatory Transparency', 
     ylab = expression(paste(Delta, 'Bond Volatility')), 
     main = '')

#### FRT and Basic Rate ####
m2 <- lm(dltrate ~ lltrate + lfrt + dfrt + lpubdebtgdp +
             dpubdebtgdp + linfl + dinfl + lus3mrate + dus3mrate + 
             loecdgrowth + doecdgrowth + lvix + dvix + iso2c,
         data = main_noca)

# Estimate effect
m2_e <- Effect('lfrt', m2)
plot(m2_e, xlab = 'Financial Regulatory Transparency', 
     ylab = expression(paste(Delta, 'Long Term Interest Rate')), 
     main = '')

#### FRT and Spread ####
m3 <- lm(dltspreadus ~ lltspreadus + lfrt + dfrt + lpubdebtgdp +
             dpubdebtgdp + linfl + dinfl + lus3mrate + dus3mrate + 
             loecdgrowth + doecdgrowth + lvix + dvix + iso2c,
         data = main_noca)

# Estimate effect
m3_e <- Effect('lfrt', m3)

plot(m2_e, xlab = 'Financial Regulatory Transparency', 
     ylab = expression(paste(Delta, 'Long Term Interest Rate')), 
     main = '')
plot(m3_e, xlab = 'Financial Regulatory Transparency', 
     ylab = expression(paste(Delta, 'LT Rate Spread')), 
     main = '')


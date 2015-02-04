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

##### FRT #####
m1 <- lm(dltrate ~ lltrate + lfrt + dfrt + lpubdebtgdp +
                  dpubdebtgdp + linfl + dinfl + lus3mrate + dus3mrate + 
                  loecdgrowth + doecdgrowth + lvix + dvix + iso2c,
            data = main_noca)

# Estimate effect
m1_e <- Effect('lfrt', m1)
plot(m1_e, xlab = 'Financial Regulatory Transparency', ylab = 'Bond Volatility', 
     main = '', cex = 2)

#############
# FRT Modelling Explore
# Christopher Gandrud
# 29 March 2014
#############

# Data created by BasicPaperSource2_v1.R

GitDir <- '/git_repositories/FRTIndex/'

library(Zelig)
library(DataCombine)
library(ggplot2)
library(wesanderson)

source(paste0(GitDir, 'source/miscFunctions/simZelig.R'))

# Create lags
Comb <- Comb[order(Comb$country, Comb$year), ]
Comb <- slide(Comb, Var = 'median', GroupVar = 'country', NewVar = 'FRT_Lag')
Comb <- slide(Comb, Var = 'economic_abs', GroupVar = 'country', NewVar = 'Econ_Lag')

# Run basic model
M1 <- zelig(ZScore ~ FRT_Lag*Econ_Lag,
            data = Comb, model = 'ls', method = 'weave', cite = FALSE)


scenarios <- expand.grid(FRT_Lag = seq(from = -1, to = 5, by = 0.1) ,
                         Econ_Lag = c(0.25, 0.9))

# Sim1 <- simZelig(M1, scen = scenarios, secondVar = 'economic_abs')
Sim1 <- simZelig(M1, scen = scenarios)

Sim1 <- SmoothSimulations(SimIn = Sim1, xaxis = 'median', group = 'Econ_Lag')

Sim1$Econ_Lag <- factor(Sim1$Econ_Lag, labels = c('low', 'high'))

ggplot(Sim1, aes(FRT_Lag, QI, colour = Econ_Lag)) +
    geom_line(aes(group = interaction(ID, eEcon_Lag)), alpha = 0.1) +
    scale_color_manual(values = wes.palette(2, "Royal1"), name = 'Economic\nInst. Qual.') +
    scale_alpha(guide = FALSE) +
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2))) +
    xlab('\nFRT Transparency Level') + ylab('Predicted Z-Score\n') +
    theme_bw(base_size = 15)

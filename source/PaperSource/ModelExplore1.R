#############
# FRT Modelling Explore
# Christopher Gandrud
# 28 March 2014
#############

# Data created by BasicPaperSource2_v1.R

GitDir <- '/git_repositories/FRTIndex/'

library(Zelig)
library(ggplot2)
library(wesanderson)

source(paste0(GitDir, 'source/miscFunctions/simZelig.R'))

M1 <- zelig(ZScore ~ median*economic_abs,
            data = Comb, model = 'ls', method = 'weave', cite = FALSE)


scenarios <- expand.grid(median = seq(from = -1, to = 5, by = 0.1) ,
                         economic_abs = c(0.25, 0.9))

Sim1 <- simZelig(M1, scen = scenarios, secondVar = 'economic_abs')
Sim1 <- simZelig(M1, scen = scenarios)

Sim1 <- SmoothSimulations(SimIn = Sim1, xaxis = 'median', group = 'economic_abs')

Sim1$economic_abs <- factor(Sim1$economic_abs, labels = c('low', 'high'))

ggplot(Sim1, aes(median, QI, colour = economic_abs)) +
    geom_line(aes(group = interaction(ID, economic_abs)), alpha = 0.1) +
    scale_color_manual(values = wes.palette(2, "Royal1"), name = 'Economic\nInst. Qual.') +
    scale_alpha(guide = FALSE) +
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2))) +
    xlab('\nFRT Transparency Level') + ylab('Predicted Z-Score\n') +
    theme_bw(base_size = 15)

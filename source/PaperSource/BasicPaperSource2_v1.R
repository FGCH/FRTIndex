##############
# Source 2 for figures in the basic FRT index description paper
# Christopher Gandrud
# 24 March 2014
##############

# Load packages
library(countrycode)
library(WDI)
library(ggplot2)
library(gridExtra)

GitDir <- '/git_repositories/FRTIndex/'

# --------------------------------------------------- #
# Load FRT Index
FRT <- read.csv(paste0(GitDir, 'IndexData/FRTIndex_v0_1.csv'),
                stringsAsFactors = FALSE)

# Burearucratic Institutional Quality
Qual_URL <- "https://docs.google.com/spreadsheet/pub?key=0AtSgiY60tn0_dEtYUVo4TWlFOU01dnRjTE1WZmFTUWc&single=true&gid=0&output=csv"
Qual <- repmis::source_data(Qual_URL)

Qual$iso2c <- countrycode(Qual$country, origin = 'country.name',
                             destination = 'iso2c')
QualSub <- Qual[, c('iso2c', 'year', 'economic_abs')]

# Z-Score
ZScore <- WDI(indicator = 'GFDD.SI.01', start = 1998, end = 2011)
ZScore <- ZScore[, c('iso2c', 'year', 'GFDD.SI.01')]
names(ZScore) <- c('iso2c', 'year', 'ZScore')

# Merge
Comb <- merge(FRT, QualSub, by = c('iso2c', 'year'), all.x = TRUE)
Comb <- merge(Comb, ZScore, by = c('iso2c', 'year'), all.x = TRUE)

# --------------------------------------------------- #
# Basic correlation plots
EconQual <- ggplot(Comb, aes(median, economic_abs)) + geom_point() +
                stat_smooth(se = FALSE) +
                ylab('Economic Institutional Quality\n') + xlab('\nMedian FRT Index') +
                theme_bw()


ZPlot <- ggplot(Comb, aes(median, ZScore)) + geom_point() +
                stat_smooth(se = FALSE) +
                ylab('Z-Score\n') + xlab('\nMedian FRT Index') +
                theme_bw()

pdf(file = paste0(GitDir, 'paper/figures/Associations1.pdf'), width = 12)
    grid.arrange(EconQual, ZPlot, nrow = 1)
dev.off()

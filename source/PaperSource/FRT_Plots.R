####################################
# Plot results from FRT_Stan
# Christopher Gandrud
# 28 November 2014
# MIT License
####################################

# Set working directory. Change as needed.
setwd('/git_repositories/FRTIndex/')

# Main figure output directory
dir <- 'paper/paper_plots/'

# Load packages
library(gridExtra)
library(ggplot2)
library(countrycode)
library(dplyr)

# Load stan_catterpillar function
SourceURL <- 'https://gist.githubusercontent.com/christophergandrud/9b6caf8fa6ed0cbb33c4/raw/211f98590903e96e87686b02e4436a207f49f2ad/stan_caterpillar.R'
devtools::source_url(SourceURL)

# Country list from FRT_Stan_v_02_1990.R
countries <- unique(BaseSub$country)

# Load simulations
load('~/Desktop/fit_2014-11-26.RData')

# ---------------------------------------------------------------------------- #
#### Plot by year ####
sc_year <- function(year, yrange = 1990:2011) {
    ynumber <- grep(pattern = paste0('^', year, '$'), x = as.character(yrange))
    stan_catterpillar(fit_NonIndp, params = paste0('alpha\\[.*,', ynumber, '\\]'),
                      params_labels = countries) +
        scale_x_continuous(breaks = c(-20, -10, -3, -1, 0, 1, 3, 5)) +
        ylab('') + xlab('') + ggtitle(year)
}

# 1990, 2005, 2011
y1 <- sc_year(1990)
y2 <- sc_year(2005)
y3 <- sc_year(2011)

pdf(file = paste0(dir, 'FRT_years.pdf'),
    width = 18)
    grid.arrange(y1, y2, y3, ncol = 3, sub = 'FRT Index (HPD)')
dev.off()

# ---------------------------------------------------------------------------- #
#### Plot individual countries ####
sc_country <- function(country) {
    cnumber <- grep(pattern = country, x = countries)
    param_temp <- paste0('alpha\\[', cnumber, ',.*\\]')
    stan_catterpillar(fit_NonIndp, params = param_temp,
                      params_labels = 1990:2011, horizontal = FALSE,
                      order_medians = FALSE) +
        scale_y_discrete(breaks = c('1990', '1995', '2000', '2005', '2010')) +
        ggtitle(paste0(country, '\n'))
}

# Hungary
pdf(file = paste0(dir, 'FRT_Hungary.pdf'))
    sc_country('Hungary')
dev.off()

# Create plots for all countries
pc <- list()
for (i in countries){
    message(i)
    pc[[i]] <- suppressMessages(sc_country(i))
}
# Put in alphabetical order
pc <- pc[order(names(pc))]

# First 20
pdf(file = paste0(dir, 'FRT_countries_1.pdf'),
    width = 15, height = 11)
    do.call(grid.arrange, c(pc[1:20]))
dev.off()

# Second 20
pdf(file = paste0(dir, 'FRT_countries_2.pdf'),
    width = 15, height = 11)
    do.call(grid.arrange, c(pc[21:40]))
dev.off()

# Third 20
pdf(file = paste0(dir, 'FRT_countries_3.pdf'),
    width = 15, height = 11)
do.call(grid.arrange, c(pc[41:60]))
dev.off()

# ---------------------------------------------------------------------------- #
#### Other Paremeters of Interest ####
indicators_df <- read.csv('/git_repositories/FRTIndex/source/PaperSource/IndicatorDescript/IndicatorDescription.csv',
                          stringsAsFactors = FALSE)
indicator_labels <- indicators_df[, 2]

# Difficulty
pdf(file = paste0(dir, 'difficultyPlot.pdf'), width = 10, height = 5.5)
    stan_catterpillar(fit_NonIndp, 'beta\\[.*\\]',
                    params_labels = indicator_labels) +
        ylab('') + xlab('\nCoefficient')
dev.off()

# Discrimination
pdf(file = paste0(dir, 'discriminationPlot.pdf'), width = 10, height = 5.5)
stan_catterpillar(fit_NonIndp, 'log_gamma\\[.*\\]',
                  params_labels = indicator_labels) +
    ylab('') + xlab('\nCoefficient')
dev.off()

# ---------------------------------------------------------------------------- #
#### Compare FRT to Proportion Reported method ####
# Load data
FRT <- read.csv('IndexData/FRTIndex_v0_2.csv', stringsAsFactors = FALSE)
FRTProp <- read.csv('IndexData/alternate/PropReported.csv',
                    stringsAsFactors = FALSE)

# Simple function to rescale the variables
rescale <- function(variable){
    rescaled <- (variable - mean(variable)) / sd(variable)
    return(rescaled)
}

FRTStand <- rescale(FRT$median)
FRTPropStand <- rescale(FRTProp$FRT_PropReport)
Comb <- data.frame(FRT = FRTStand, FRTPropStand)

# Plot
pdf(file = paste0(dir, 'FRT_Prop_Compare.pdf'))
    ggplot(Comb, aes(FRTStand, FRTPropStand)) + geom_point(alpha = 0.5) +
        stat_smooth(method = 'loess', se = FALSE, size = 1) +
        stat_smooth(method = 'lm', se = FALSE, linetype = 'dashed',
                    color = 'grey', size = 1) +
        ylab('Proportion Reported (standardized)\n') +
        xlab('\nMedian FRT Index (standardized)') +
        theme_bw()
dev.off()

# ---------------------------------------------------------------------------- #
#### Compare FRT to Liedorp et al. 2013 ####
# Load Liedorp et al. data (2013, 318)
liedorp <- read.csv('misc/liedorp_et_al_2013_index.csv',
                    stringsAsFactors = FALSE)

# Clean to Merge
liedorp$iso2c <- countrycode(liedorp$country, origin = 'country.name',
                             destination = 'iso2c')
liedorp$year <- 2010
liedorp <- select(liedorp, -country)

# Rescale
liedorp_vars <- grep(pattern = '.*liedorp', names(liedorp))
for (i in liedorp_vars){
    liedorp[, i] <- rescale(liedorp[, i])
}
FRT$median_rescale <- rescale(FRT$median)
FRT_rescale <- select(FRT, iso2c, year, median_rescale)

# Merge with main data
frt_liedorp <- merge(FRT_rescale, liedorp)

#### Functions to create FRT Liedorp plots
# Capitalisation function
# From http://stackoverflow.com/a/6364905
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep = "", collapse = " ")
}

# Plotting function
fl_plot <- function(y){
    frt_liedorp_temp <- rename_(frt_liedorp, yvar = y)
    ylabel <- gsub(pattern = '_liedorp', replacement = '', x = y) %>% simpleCap()
    p_temp <- ggplot(frt_liedorp_temp, aes(x = median_rescale, y = yvar)) +
                    geom_point() + stat_smooth() +
                    ylab(paste(ylabel, '(Liedorp et al. stnd)\n')) +
                    xlab('\nFRT (median stnd)') +
                    theme_bw()
    rm(frt_liedorp_temp)
    return(p_temp)
}

liedorp_vars <- names(liedorp)[1:max(liedorp_vars)]

pfl <- list()
for (i in liedorp_vars){
    pfl[[i]] <- fl_plot(i) 
}

pdf(file = paste0(dir, 'FRT_Liedorp.pdf'), width = 8, height = 11.5)
    do.call(grid.arrange, pfl[1:6])
dev.off()

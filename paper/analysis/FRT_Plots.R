####################################
# Plot results from FRT_Stan
# Christopher Gandrud
# MIT License
####################################

# Set working directory. Change as needed.
setwd('/git_repositories/FRTIndex/')

# Main figure output directory
dir <- 'paper/paper_plots/'

# Load packages
if (!('StanCat' %in% installed.packages()[, 1])) devtools::install_github('christophergandrud/StanCat')
library(StanCat)
library(rio)
library(devtools)
library(gridExtra)
library(ggplot2)
library(countrycode)
library(dplyr)

# Load function to subset the data frame to countries that report
# at least 1 item.
source('source/miscFunctions/report_min_once.R')

# Load base data
BaseSub <-
    'https://raw.githubusercontent.com/FGCH/FRTIndex/master/source/RawData/wdi_fred_combined.csv' %>%
    import
#### Keep only countries that report at least 1 item for the entire period  ####
dropped <- report_min_once(BaseSub, drop_names = TRUE)
BaseSub <- report_min_once(BaseSub)

# Country list from FRT_Stan_v_02_1990.R
countries <- unique(BaseSub$country)

# Load simulations
# Change location as needed
load('/Volumes/Gandrud1TB/frt/fit_2015-06-12.RData')

# ---------------------------------------------------------------------------- #
#### Plot by year ####
sc_year <- function(year, yrange = 1990:2011) {
    ynumber <- grep(pattern = paste0('^', year, '$'), x = as.character(yrange))
    stan_caterpillar(fit, pars = paste0('alpha\\[.*,', ynumber, '\\]'),
                      pars_labels = countries, hpd = TRUE) +
        scale_x_continuous(breaks = c(-5, -3, -1, 0, 1, 3, 5)) +
        ylab('') + xlab('') + ggtitle(year)
}

# 1990, 2011
y1 <- sc_year(1990)
y2 <- sc_year(2011)

# For github
png(file = 'FRT_overview.png', width = 900)
    grid.arrange(y1, y2, ncol = 2, bottom = 'FRT Index (HPD)')
dev.off()

# For paper
pdf(file = paste0(dir, 'FRT_years.pdf'), width = 20, height = 16)
    grid.arrange(y1, y2, ncol = 2, bottom = 'FRT Index (HPD)')
dev.off()

# For presentation
pdf(file = paste0(dir, 'frt_1990.pdf'), width = 10, height = 10)
    y1 + xlab('\nFRT Index (HPD)')
dev.off()

pdf(file = paste0(dir, 'frt_2011.pdf'), width = 10, height = 10)
    y2 + xlab('\nFRT Index (HPD)')
dev.off()

# ---------------------------------------------------------------------------- #
#### Plot individual countries ####
sc_country <- function(country) {
    cnumber <- grep(pattern = country, x = countries)
    param_temp <- paste0('alpha\\[', cnumber, ',.*\\]')
    stan_caterpillar(fit, pars = param_temp,
                      pars_labels = 1990:2011, horizontal = FALSE,
                      order_medians = FALSE, hpd = TRUE) +
        scale_y_discrete(breaks = c('1990', '1995', '2000', '2005', '2010')) +
        ggtitle(paste0(country, '\n'))
}

# Hungary
pdf(file = paste0(dir, 'FRT_Hungary.pdf'))
    sc_country('Hungary')
dev.off()

# Create plots for all countries
pc <- list()
for (i in countries) {
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

# Final 8
pdf(file = paste0(dir, 'FRT_countries_4.pdf'),
    width = 15, height = 11)
    do.call(grid.arrange, c(pc[61:68]))
dev.off()

# ---------------------------------------------------------------------------- #
#### Other Paremeters of Interest ####
indicators_df <- 'paper/IndicatorDescript/IndicatorDescription.csv' %>%
                    read.csv(stringsAsFactors = FALSE)
indicator_labels <- indicators_df[, 2]

# Difficulty
pdf(file = paste0(dir, 'difficultyPlot.pdf'), width = 10, height = 5.5)
    stan_caterpillar(fit, 'beta\\[.*\\]',
                    pars_labels = indicator_labels) +
        ylab('') + xlab('\nCoefficient')
dev.off()

# Discrimination
pdf(file = paste0(dir, 'discriminationPlot.pdf'), width = 10, height = 5.5)
    stan_caterpillar(fit, 'gamma\\[.*\\]',
                  pars_labels = indicator_labels) +
    ylab('') + xlab('\nCoefficient')
dev.off()

# ---------------------------------------------------------------------------- #
#### Median trend overview ####
# Load data
FRT <- read.csv('IndexData/FRTIndex.csv', stringsAsFactors = FALSE)

all_plot <- ggplot(FRT, aes(year, median, group = iso2c)) +
        geom_line(alpha = 0.3) +
        ylab('Median FRT Index\n') + xlab('') +
        theme_bw()

# ---------------------------------------------------------------------------- #
#### Compare FRT to Proportion Reported method ####
# Load data
FRTProp <- read.csv('IndexData/alternate/PropReported.csv',
                    stringsAsFactors = FALSE) %>%
                subset(!(country %in% dropped))

# Simple function to rescale the variables
rescale <- function(variable){
    rescaled <- (variable - median(variable)) / sd(variable)
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
#### Raw proportion reported plots ####
FRTProp <- subset(FRTProp, !(country %in% dropped))
pprop <- list()
for (u in unique(FRTProp$country)) {
    message(u)
    temp <- subset(FRTProp, country == u)
    pprop[[u]] <- ggplot(temp, aes(year, FRT_PropReport)) +
#        scale_x_continuous(breaks = c('1990', '1995', '2000', '2005', '2010')) +
        scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
        geom_line() + xlab('') + ylab('') +
        ggtitle(u) + theme_bw()
}

# First 20
pdf(file = paste0(dir, 'PropReported_countries_1.pdf'),
    width = 15, height = 11)
    do.call(grid.arrange, pprop[1:20])
dev.off()

# Second 20
pdf(file = paste0(dir, 'PropReported_countries_2.pdf'),
    width = 15, height = 11)
    do.call(grid.arrange, pprop[21:40])
dev.off()

# Third 20
pdf(file = paste0(dir, 'PropReported_countries_3.pdf'),
    width = 15, height = 11)
    do.call(grid.arrange, pprop[41:60])
dev.off()

# Final 8
pdf(file = paste0(dir, 'PropReported_countries_4.pdf'),
    width = 15, height = 11)
    do.call(grid.arrange, pprop[61:68])
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
liedorp <- dplyr::select(liedorp, -country)

# Rescale
liedorp_vars <- grep(pattern = '.*liedorp', names(liedorp))
for (i in liedorp_vars) {
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
for (i in liedorp_vars) {
    pfl[[i]] <- fl_plot(i)
}

pdf(file = paste0(dir, 'FRT_Liedorp.pdf'), width = 11.5, height = 8)
    do.call(grid.arrange, c(pfl[1:6], ncol = 3))
dev.off()

# ---------------------------------------------------------------------------- #
#### Compare FRT to Hollyer et al. 2014 ####
# Load HRV means
hrv <- 'source/Hollyer_et_al_Compare/hrv_means.csv' %>%
        read.csv(stringsAsFactors = F)

# Standardise & merge
hrv$hrv_median_rescale <- rescale(hrv$hrv_mean)
hrv_rescale <- select(hrv, iso2c, year, hrv_median_rescale)

FRT_HRV <- merge(FRT_rescale, hrv_rescale, all.x = T)

cor.test(FRT_HRV$median_rescale, FRT_HRV$hrv_median_rescale)

# Plot
pdf(file = paste0(dir, 'FRT_HRV_Compare.pdf'))
ggplot(FRT_HRV, aes(median_rescale, hrv_median_rescale)) +
    geom_point(alpha = 0.5) +
    stat_smooth(method = 'loess', se = FALSE, size = 1) +
    stat_smooth(method = 'lm', se = FALSE, linetype = 'dashed',
                color = 'grey', size = 1) +
    ylab('Mean HRV (standardized)\n') +
    xlab('\nMedian FRT Index (standardized)') +
    theme_bw()
dev.off()

####################################
# Plot results from FRT_Stan version 2
# Christopher Gandrud
# MIT License
# Note: discrimination and difficulty plots are created in read_julia_stan.R
# for computational efficiency reasons.
####################################

library(rio)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(countrycode)

# Set working directory. Change as needed.
setwd('/git_repositories/FRTIndex/')

# Main figure output directory
dir <- 'paper/paper_plots/'

# Import data
frt <- import('IndexData/FRTIndex_v2.csv')

# Functions --------------------------------------------------------------------
# plot year summary
frt_year <- function(df = frt, y, xlabel = '') {
    sub <- subset(df, year == y)
    out <- ggplot(sub, aes(median, y = reorder(country, median))) +
        geom_point() +
        geom_segment(aes(x = lower_95, xend = upper_95,
                         yend = reorder(country, median)), size = 0.5,
                     alpha = 0.5) +
        geom_segment(aes(x = lower_90, xend = upper_90,
                         yend = reorder(country, median)), size = 1.5,
                     alpha = 0.5) +
        ylab('') + xlab(xlabel) + ggtitle(sprintf('%s\n', y)) +
        theme_bw()
    return(out)
}

# plot country summary
frt_country <- function(df = frt, id, xlabel = '') {
    sub <- subset(df, country == id)
    out <- ggplot(sub, aes(median, y = year)) +
        geom_point() +
        geom_segment(aes(x = lower_95, xend = upper_95,
                         yend = year), size = 0.5,
                     alpha = 0.5) +
        geom_segment(aes(x = lower_90, xend = upper_90,
                         yend = year), size = 1.5,
                     alpha = 0.5) +
        scale_y_discrete(breaks = c('1990', '1995', '2000', '2005', '2010')) +
        ylab('') + xlab(xlabel) + ggtitle(sprintf('%s\n', id)) +
        theme_bw() +
        coord_flip()
    return(out)
}


# Plot yearly summaries --------------------------------------------------------
y1 <- frt_year(y = 1990)
y2 <- frt_year(y = 2013)

# For github
png(file = 'FRT_overview.png', width = 1500, height = 1000)
    grid.arrange(y1, y2, ncol = 2, bottom = 'FRT Index (HPD)')
dev.off()

# For paper
pdf(file = paste0(dir, 'FRT_years.pdf'), width = 20, height = 16)
    grid.arrange(y1, y2, ncol = 2, bottom = 'FRT Index (HPD)')
dev.off()

# Plot country summaries -------------------------------------------------------
# Hungary
pdf(file = paste0(dir, 'FRT_Hungary.pdf'))
    frt_country(id = 'Hungary')
dev.off()

# Create plots for all countries
countries <- unique(frt$country)
pc <- list()
for (i in countries) {
    message(i)
    pc[[i]] <- suppressMessages(frt_country(id = i))
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
    do.call(grid.arrange, c(pc[61:69]))
dev.off()

#### Compare FRT to Proportion Reported method ---------------------------------
# Load data
BaseSub <- import('source/RawData/wdi_fred_combined_GFDDv2015.csv')
#### Keep only countries that report at least 1 item for the entire period
source('source/miscFunctions/report_min_once.R')
dropped <- report_min_once(BaseSub, drop_names = TRUE)

FRTProp <- read.csv('IndexData/alternate/PropReported_GFDDv2015.csv',
                    stringsAsFactors = FALSE) %>%
    subset(!(country %in% dropped))

# Simple function to rescale the variables
rescale <- function(variable){
    rescaled <- (variable - median(variable)) / sd(variable)
    return(rescaled)
}

FRTStand <- rescale(frt$median)
FRTPropStand <- rescale(FRTProp$FRT_PropReport)

Comb <- data.frame(frt = FRTStand, FRTPropStand)

# Plot
pdf(file = paste0(dir, 'FRT_Prop_Compare.pdf'))
    ggplot(Comb, aes(FRTStand, FRTPropStand)) + geom_point(alpha = 0.5) +
        #stat_smooth(method = 'loess', se = FALSE, size = 1) +
        stat_smooth(method = 'lm', se = FALSE, linetype = 'dashed',
                    color = 'grey', size = 1) +
        ylab('Proportion Reported (standardized)\n') +
        xlab('\nMedian FRT Index (standardized)') +
        theme_bw()
dev.off()

#### Raw proportion reported plots ---------------------------------------------
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
    do.call(grid.arrange, pprop[61:69])
dev.off()

#### Compare FRT to Liedorp et al. 2013 ----------------------------------------
# Load Liedorp et al. data (2013, 318)
liedorp <- import('misc/liedorp_et_al_2013_index.csv')

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
frt$median_rescale <- rescale(frt$median)
FRT_rescale <- select(frt, iso2c, year, median_rescale)
FRT_rescale_2010 <- subset(FRT_rescale, year == 2010)

# Merge with main data
frt_liedorp <- merge(FRT_rescale_2010, liedorp)

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

#### Compare FRT to Hollyer et al. 2014 ----------------------------------------
# Load HRV means
hrv <- import('paper/analysis/Hollyer_et_al_Compare/hrv_means.csv')

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

# ---------------------------------------------------------------------------- #
# Bruegel FRT plots
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Set working directory. Change as needed.
setwd('/git_repositories/FRTIndex/')

# Main figure output directory
dir <- 'misc/bruegel_wp/figures/'

# Load packages
if (!('StanCat' %in% installed.packages()[, 1])) devtools::install_github('christophergandrud/StanCat')
library(StanCat)
library(rio)
library(devtools)
library(gridExtra)
library(ggplot2)
library(countrycode)
library(dplyr)
library(lubridate)
library(DataCombine)
library(wesanderson)
library(WDI)
library(tidyr)

# Load function to subset the data frame to countries that report
# at least 1 item.
source('source/miscFunctions/report_min_once.R')

# Load base data
try(
    BaseSub <-
        'https://raw.githubusercontent.com/FGCH/FRTIndex/master/source/RawData/wdi_fred_combined.csv' %>%
        import, silent = T
)
try(BaseSub <- 'source/RawData/wdi_fred_combined.csv' %>% import, silent = T)
#### Keep only countries that report at least 1 item for the entire period  ####
dropped <- report_min_once(BaseSub, drop_names = TRUE)
BaseSub <- report_min_once(BaseSub)

# Country list from FRT_Stan_v_02_1990.R
countries <- unique(BaseSub$country)

# Load simulations
# Change location as needed
FRT <- import('IndexData/FRTIndex.csv')
load('/Volumes/Gandrud1TB/frt/fit_2015-06-12.RData')

# ---------------------------------------------------------------------------- #
## Compare top and bottom 5
FRT$year <- FRT$year %>% as.numeric

# Custom plotting functions ----------
sc_year <- function(x = FRT, selection) {
    temp <- subset(x, year == selection)
    # Find top 10 and bottom 10
    temp <- arrange(temp, desc(median))
    top_temp <- temp[1:5, ] %>% cbind(., ranked = rep('Top 5', 5))
    rows = nrow(temp)
    rows_5 = rows - 4
    bottom_temp <- temp[rows_5:rows, ] %>% cbind(., ranked = rep('Bottom 5', 5))
    ten <- rbind(top_temp, bottom_temp)
    
    ggplot(ten, aes(y = median, x = reorder(country, median), 
                    ymin = lower_90, ymax = upper_90, 
                    colour = as.factor(ranked))) + 
        geom_pointrange(size = 1) +
        coord_flip() +
        scale_colour_manual(values = c('#990000', '#CC00FF'), guide = F) +
        xlab('') + ylab('') + 
        ggtitle(paste0(selection, '\n')) +
        theme_bw()
}

# One country plotting function
sc_country <- function(country) {
    cnumber <- grep(pattern = country, x = countries)
    param_temp <- paste0('alpha\\[', cnumber, ',.*\\]')
    stan_caterpillar(fit, pars = param_temp,
                     pars_labels = 1990:2011, horizontal = FALSE,
                     order_medians = FALSE, hpd = TRUE) +
        scale_y_discrete(breaks = c('1990','2010')) +
        #scale_x_continuous(breaks = c(1990, 2000, 2011)) +
        ggtitle(paste0(country, '\n'))
}

y1 <- sc_year(selection = 1990)
y2 <- sc_year(selection = 2011)

# For github
png(file = paste0(dir, 'FRT_overview.png'), width = 750, height = 500)
    grid.arrange(y1, y2, nrow = 1, bottom = '\nFRT Transparency Index')
dev.off()

# ---------------------------------------------------------------------------- #
## Overall trend
year_means <- FRT %>% group_by(year) %>% 
    dplyr::summarise(centre = mean(median))
year_means$trend = 1
year_means$iso2c = 'none'
year_means <- year_means %>% MoveFront(c('year', 'centre', 'trend'))

mean_trend <- ggplot(year_means, aes(year, centre)) +
    geom_point(size = 2) +
    geom_line(alpha = 0.5) +
    xlab('') + ylab('Mean FRT Score\n') +
    theme_bw()

ggsave(mean_trend, filename = paste0(dir, 'FRT_mean_trend.png'))


FRT_stripped <- FRT[, c('iso2c', 'year', 'median')]
FRT_stripped <- FRT_stripped %>% dplyr::rename(centre = median)
FRT_stripped$trend = 0

comb <- rbind(FRT_stripped, year_means)

ggplot(comb, aes(year, centre, group = iso2c, color = as.factor(trend),
                 alpha = as.factor(trend), size = as.factor(trend))) +
    geom_line() +
    scale_color_manual(values = c('gray', 'red'), name = '') +
    scale_alpha_discrete(range = c(0.5, 1), name = '') +
    scale_size_manual(values = c(0.5, 3), name = '') +
    ylab('Median FRT Index\n') + xlab('') +
    theme_bw()

## EU vs. High Income ---------------------------------------------------------
# Find high income not-EU
eu <- import('https://raw.githubusercontent.com/christophergandrud/eu_members/master/eu_membership.csv') %>%
    select(-country)
eu$eu_member <- 1

FRT <- merge(FRT, eu, by = c('iso2c', 'year'), all.x = T)
FRT$eu_member[is.na(FRT$eu_member)] <- 0

wdi <- WDI(start = 1990, end = 2011, extra = T) %>% 
    select(iso2c, year, income)

FRT <- merge(FRT, wdi, by = c('iso2c', 'year'), all.x = T)

FRT$eu_not[FRT$eu_member == 1] <- 'EU'
FRT$eu_not[FRT$income == 'High income: OECD' & FRT$eu_member != 1] <- 
    'Other High income OECD,\n not EU'
FRT$eu_not[FRT$iso2c == 'US'] <- 'US'

frt_eu_comp <- DropNA(FRT, 'eu_not')

# Find group means
year_means_eu <- frt_eu_comp %>% group_by(eu_not, year) %>% 
    dplyr::summarise(centre = mean(median),
                     max_frt = max(median))

mean_eu_high <- ggplot(year_means_eu, aes(year, centre, 
                                          group = eu_not, colour = eu_not)) +
    geom_point(size = 2) +
    geom_line(alpha = 0.5) +
    scale_color_manual(values = wes_palette('Moonrise2'), name = '',
                       guide = guide_legend(reverse = TRUE)) +
    xlab('') + ylab('Mean FRT Score\n') +
    theme_bw()

ggsave(mean_eu_high, filename = paste0(dir, 'FRT_eu_high_mean_trend.png'))


# Find current Eurozone members
eurozone <- import('https://raw.githubusercontent.com/christophergandrud/euro_membership/master/data/euro_membership_data.csv') %>% 
    select(-country)
eurozone$euro_member <- 'EU'

frt_euro <- merge(FRT, eurozone, by = c('iso2c', 'year'), all.x = T)

frt_euro$euro_member[is.na(frt_euro$euro_member)] <- 'blank'
frt_euro$euro_member[frt_euro$income == 'High income: OECD' & frt_euro$euro_member != 'EU'] <- 
    'Other High income OECD,\n not Eurozone'
frt_euro$euro_member[FRT$iso2c == 'US'] <- 'US'

frt_euro <- frt_euro %>% filter(euro_member != 'blank')

# Find group means
year_means_euro <- frt_euro %>% group_by(euro_member, year) %>% 
    dplyr::summarise(centre = mean(median),
                     max_frt = max(median))

mean_eurozone_high <- ggplot(year_means_euro, aes(year, centre, 
                          group = euro_member, colour = euro_member)) +
    geom_point(size = 2) +
    geom_line(alpha = 0.5) +
    scale_color_manual(values = wes_palette('Moonrise2'), name = '',
                       guide = FALSE) +
    scale_y_continuous(limits = c(-0.2, 1.2)) +
    xlab('') + ylab('Mean FRT Score\n') + ggtitle('Eurozone\n') +
    theme_bw(base_size = 15)

## EU-15
eu_15 <- c('Austria', 'Belgium', 'Denmark', 'Finland', 'France', 'Germany', 
           'Greece', 'Ireland', 'Italy', 'Luxembourg', 'Netherlands', 'Portugal',
        'Spain', 'Sweden', 'United Kingdom')

FRT$eu15_not <- 'blank'
FRT$eu15_not[FRT$country %in% eu_15 & FRT$eu_member == 1] <- 'EU'
FRT$eu15_not[FRT$income == 'High income: OECD' & FRT$eu15_not != 'EU'] <- 
    'Other high income OECD,\n not EU'
FRT$eu15_not[FRT$iso2c == 'US'] <- 'US'


frt_eu15_comp <- FRT %>% filter(eu15_not != 'blank')

# Find group means
year_means_eu15 <- frt_eu15_comp %>% group_by(eu15_not, year) %>% 
    dplyr::summarise(centre = mean(median),
                     max_frt = max(median))

mean_eu15_high <- ggplot(year_means_eu15, aes(year, centre, 
                                          group = eu15_not, colour = eu15_not)) +
    geom_point(size = 2) +
    geom_line(alpha = 0.5) +
    scale_color_manual(values = wes_palette('Moonrise2'), name = '',
                       guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(limits = c(-0.2, 1.2)) +
    xlab('') + ylab('') + ggtitle('EU-15\n') +
    theme_bw(base_size = 15)

png(file = paste0(dir, 'FRT_eurozone_eu15.png'), width = 1000, height = 700)
    grid.arrange(mean_eurozone_high, mean_eu15_high, nrow = 1, widths = c(1.65, 2))
dev.off()

grid.arrange(mean_eurozone_high, mean_eu15_high, nrow = 1, widths = c(1.65, 2))

# All EU
sc_country_eu_scale <- function(country) {
    cnumber <- grep(pattern = country, x = countries)
    param_temp <- paste0('alpha\\[', cnumber, ',.*\\]')
    stan_caterpillar(fit, pars = param_temp,
                     pars_labels = 1990:2011, horizontal = FALSE,
                     order_medians = FALSE, hpd = TRUE) +
        scale_y_discrete(breaks = c('1990','2010')) +
        scale_x_continuous(limits = c(-0.5, 2.1), breaks = c(-0.5, 0, 0.5, 1, 2)) +
        ggtitle(paste0(country, '\n'))
}

eurozone_vector <- countrycode(euro_all, origin = 'iso2c', 
                               destination = 'country.name')

eurozone_vector <- eurozone_vector[eurozone_vector %in% unique(FRT$country)]

eu_list <- list()
for (i in eurozone_vector) {
    message(i)
    eu_list[[i]] <- suppressMessages(sc_country_eu_scale(i))
}

png(file = paste0(dir, 'FRT_eurozone_indiv.png'), width = 900, height =750)
    do.call(grid.arrange, eu_list)
dev.off()

# ---------------------------------------------------------------------------- #
# Most changed ------
changed <- change(FRT, Var = 'median', GroupVar = 'iso2c', type = 'absolute',
                   NewVar = 'median_diff')

changed$median_diff[is.na(changed$median_diff)] <- 0

cum_changed <- changed %>% group_by(iso2c) %>% 
    mutate(cum_diff = cumsum(median_diff)) %>% ungroup

# Most change by 2000 
cum_2000 <- cum_changed %>% filter(year == 2000) %>% arrange(desc(cum_diff))
cum_2005 <- cum_changed %>% filter(year == 2005) %>% arrange(desc(cum_diff))
cum_2011 <- cum_changed %>% filter(year == 2011) %>% arrange(desc(cum_diff))

# Most improved plots
top_15_list <- list()
top_15_countries <- cum_2011 %>% as.data.frame %>% .[1:15, 'country'] %>% as.vector
for (i in top_15_countries) {
    message(i)
    top_15_list[[i]] <- suppressMessages(sc_country(i))
}

png(file = paste0(dir, 'FRT_top_15.png'), width = 600, height = 750)
    do.call(grid.arrange, top_15_list)
dev.off()

# Most declining
bottom_15_list <- list()
bottom_15_countries <- cum_2011 %>% as.data.frame %>% 
    .[(nrow(cum_2011) - 14):nrow(cum_2011), 'country'] %>% as.vector
for (i in bottom_15_countries) {
    message(i)
    bottom_15_list[[i]] <- suppressMessages(sc_country(i))
}

png(file = paste0(dir, 'FRT_bottom_15.png'), width = 600, height = 750)
    do.call(grid.arrange, bottom_15_list)
dev.off()

# Gulf states
gulf <- c('Oman', 'Qatar','United Arab Emirates')
gulf_list <- list()

for (i in gulf) {
    message(i)
    gulf_list[[i]] <- suppressMessages(sc_country(i))
}

png(file = paste0(dir, 'FRT_gulf.png'), width = 600, height = 750)
    do.call(grid.arrange, gulf_list)
dev.off()

fr_benelux <- c('France', 'Austria', 'Belgium', 'Luxembourg', 'Netherlands')
fr_benelux_list <- list()

for (i in fr_benelux) {
    message(i)
    fr_benelux_list[[i]] <- suppressMessages(sc_country(i))
}

png(file = paste0(dir, 'FRT_fr_benelux.png'), width = 700, height = 600)
    do.call(grid.arrange, fr_benelux_list)
dev.off()

# ---------------------------------------------------------------------------- #
## Compare to capital market openess
# Download KAOPEN
kaopen <- import('http://web.pdx.edu/~ito/kaopen_2013.xls', format = 'xls')
kaopen <- import('~/Desktop/kaopen_2013.xls', format = 'xls')

kaopen$iso2c <- countrycode(kaopen$cn, origin = 'imf', destination = 'iso2c')
kaopen <- kaopen %>% select(iso2c, year, kaopen)

# Cumulative change
kaopen <- change(kaopen, Var = 'kaopen', GroupVar = 'iso2c', type = 'absolute',
                   NewVar = 'kaopen_diff')

kaopen$kaopen_diff[is.na(kaopen$kaopen_diff)] <- 0

kaopen <- kaopen %>% group_by(iso2c) %>% 
    mutate(kaopen_cum_diff = cumsum(kaopen_diff)) %>% ungroup

comb <- merge(cum_changed, kaopen, by = c('iso2c', 'year'))

library(WDI)
wdi <- WDI(start = 1990, extra = T) %>% select(iso2c, year, income)

comb <- merge(comb, wdi, by = c('iso2c', 'year'), all.x = T)

comb_sub <- comb %>% filter(cum_diff >= -0.1)

ggplot(comb_sub, aes(cum_diff, kaopen_cum_diff, group = income, color = income)) + 
    geom_point(alpha = 0.3) +
    stat_smooth(method = 'lm', size = 2) +
    scale_color_manual(values = wes_palette('Moonrise2'), name = '') +
    xlab('\nCumulative Difference in Median FRT Score') +
    ylab('Cumulative Difference in KAOPEN\n') +
    theme_bw()

ggplot(comb_sub, aes(median_diff, kaopen_diff, group = income, color = income)) +
    geom_point(alpha = 0.3) +
    stat_smooth(method = 'lm') +
    theme_bw()

# Reporting of individual items ---------------------------------------------- 
items <- merge(BaseSub, eu, by = c('iso2c', 'year'), all.x = T)
items$eu_member[is.na(items$eu_member)] <- 0
items <- merge(items, eurozone, by = c('iso2c', 'year'), all.x = T)
items$euro_member[is.na(items$euro_member)] <- 0

prop_reported <- function(x) {round(sum(x) / length(x), digits = 2)}

# Overall changes
items_all <- items[, c(2, 17:29)]
items_sum <- items_all %>% group_by(year) %>% 
    summarise_each(funs(prop_reported))

items_sum_tidy <- items_sum %>% gather(item_id, value, 2:14) %>% arrange(year)

ggplot(items_sum_tidy, aes(year, value, group = item_id, color = item_id)) +
    geom_vline(xintercept = 2008, linetype = 'dotted') +
    geom_line() +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2008, 2011)) +
    xlab('') + ylab('Proportion Reported\n') + 
    theme_bw()

# EU vs non-EU
items_eu <- items[, c(31, 2, 17:29)]
items_sum <- items_eu %>% group_by(eu_member, year) %>% 
    summarise_each(funs(prop_reported))

items_sum_tidy <- items_sum %>% gather(item_id, value, 3:15) %>% 
    arrange(eu_member, year)

items_sum_tidy$highlight <- 'Other Items'
items_sum_tidy$highlight[items_sum_tidy$item_id == 'Rep_GFDDDI11'] <- 
    'Insurance Companies'
items_sum_tidy$highlight[items_sum_tidy$item_id == 'Rep_GFDDEI02'] <- 
    'Banking and Private Credit'
items_sum_tidy$highlight[items_sum_tidy$item_id == 'Rep_GFDDDI01'] <- 
    'Banking and Private Credit'
items_sum_tidy$highlight[items_sum_tidy$item_id == 'Rep_GFDDSI04'] <- 
    'Banking and Private Credit'
items_sum_tidy$highlight[items_sum_tidy$item_id == 'Rep_GFDDDI04'] <- 
    'Banking and Private Credit'
items_sum_tidy$highlight[items_sum_tidy$item_id == 'Rep_GFDDDI14'] <- 
    'Banking and Private Credit'
items_sum_tidy$highlight[items_sum_tidy$item_id == 'Rep_GFDDOI02'] <- 
    'Banking and Private Credit'
items_sum_tidy$highlight[items_sum_tidy$item_id == 'Rep_GFDDDI03'] <- 
    "Nonbank Financial Institutions"
items_sum_tidy$highlight[items_sum_tidy$item_id == 'Rep_GFDDDI07'] <- 
    "Mutual Funds"
items_sum_tidy$highlight[items_sum_tidy$item_id == 'Rep_GFDDDI06'] <- 
    "Central Bank"
items_sum_tidy$highlight[items_sum_tidy$item_id == 'Rep_GFDDEI08'] <- 
    "Credit to Government Owned"
items_sum_tidy$highlight[items_sum_tidy$item_id == 'Rep_GFDDDI08'] <- 
    "Financial System Deposits/Liabilities"
items_sum_tidy$highlight[items_sum_tidy$item_id == 'Rep_GFDDDI05'] <- 
    "Financial System Deposits/Liabilities"

items_sum_tidy$eu_member <- items_sum_tidy$eu_member %>%
    factor(labels = c('Not-EU Member', 'EU Member')) 

ggplot(items_sum_tidy, aes(year, value, group = item_id, color = highlight)) +
    facet_wrap(~ eu_member) +
    geom_vline(xintercept = 2008, linetype = 'dotted') +
    geom_line() +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2008, 2011)) +
    scale_colour_manual(values = c('#1f78b4', '#fdbf6f', '#33a02c', '#6a3d9a', 
                                   '#b15928', '#e31a1c', '#a6cee3'
                                   ), name = '') +
    xlab('') + ylab('Proportion Reported\n') + 
    theme_bw()


items_tidy_sub <- items_sum_tidy %>% filter(year %in% c(2000, 2005, 2008, 2011))

eu_items <- ggplot(items_tidy_sub, aes(year, value, group = item_id, color = highlight,
                           linetype = highlight)) +
    facet_wrap(~ eu_member) +
    geom_line() +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(breaks = c(2000, 2005, 2008, 2011)) +
    scale_colour_manual(values = c('#1f78b4', '#fdbf6f', '#33a02c', '#6a3d9a', 
                                   '#b15928', '#e31a1c', '#a6cee3'
    ), name = '') +
    scale_linetype_discrete(name = '') +
    xlab('') + ylab('Proportion Reported\n') + 
    theme_bw()

ggsave(eu_items, filename = paste0(dir, 'FRT_eu_items.png'))

# Euro vs non-Euro
items_euro <- items[, c(32, 2, 17:29)]
items_sum <- items_euro %>% group_by(euro_member, year) %>% 
    summarise_each(funs(prop_reported))

items_sum_tidy <- items_sum %>% gather(item_id, value, 3:15) %>% 
    arrange(euro_member, year)

ggplot(items_sum_tidy, aes(year, value, group = item_id, color = item_id)) +
    facet_wrap(~ euro_member) +
    geom_vline(xintercept = 2008, linetype = 'dotted') +
    geom_line() +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2008, 2011)) +
    xlab('') + ylab('Proportion Reported\n') + 
    theme_bw()


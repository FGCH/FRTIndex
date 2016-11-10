################################################################################
# Spatial Dependence Exploration
# Christopher Gandrud
# MIT License
################################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(rio)
library(wesanderson)
library(spatialWeights) # Use devtools::install_github('christophergandrud/spatialWeights') if not installed
library(xtable)
library(DataCombine)
library(lubridate)
library(countrycode)
theme_set(theme_bw())

# Set working directory. Change as needed.
setwd('/git_repositories/FRTIndex/paper/')

# Import data set
main <- import('analysis/frt08_16_v2.dta')

# Group labels
main$region <- factor(main$region, labels = c('East Asia and Pacific',
                                              'Europe/Central Asia',
                                              'Latin America/Caribbean',
                                              'Middle East/North Africa',
                                              'North America',
                                              'South Asia',
                                              'Africa'))

# Plot change in FRED Spreads among OECD member countries
oecd <- subset(main, oecd_member == 1)

# Moran's I spatial autocorrelation index tables
region_weights_spread <- monadic_spatial_weights(oecd, id_var = 'iso2c', 
                                                 location_var = 'region',
                                                 y_var = 'd_bond_spread_fred', 
                                                 time = 'year', mc_cores = 1,
                                                 location_var_class = 'categorical', 
                                                 morans_i = 'table')
names(region_weights_spread) <- c("Year", "Observed Moran's I", "P-Value")

region_weights_volatility <- monadic_spatial_weights(oecd, id_var = 'iso2c', 
                                                     location_var = 'region',
                                                     y_var = 'd_lt_ratecov_fred', 
                                                     time_var = 'year', mc_cores = 1,
                                                     location_var_class = 'categorical',
                                                     morans_i = 'table')
names(region_weights_volatility) <- c("Year", "Observed Moran's I", "P-Value")

# Print LaTeX versions of Moran's I tables
print(xtable(region_weights_spread, 
             caption = "Moran's I Test Statistic of Spatial Autocorrelation, $\\Delta$ Long-term (10-year) bond spread (US 10-year bond, \\%)",
             label = 'moransISpreads'), 
      file = 'tables/morans_i_spreads.tex', size = 'tiny',
      caption.placement = 'top', include.rownames = FALSE) 

print(xtable(region_weights_volatility, 
             caption = "Moran's I Test Statistic of Spatial Autocorrelation, $\\Delta$ Coefficient of variation, LT bond yields (annual, based on monthly data)",
             label = 'moransIVolatility'), 
      file = 'tables/morans_i_volatility.tex', size = 'tiny',
      caption.placement = 'top', include.rownames = FALSE) 

# Regional Peers Plots ---------------------------------------------------------
# Full sample, no regional groupings
overall <- ggplot(oecd, aes(year, d_bond_spread_fred, group = iso2c)) +
    geom_line(alpha = 0.3) +
    xlab('') + ylab('10-year Bond Spread (change)')

# Regional Peers
wb_spreads <- ggplot(oecd, aes(year, d_bond_spread_fred, group = iso2c,
                               colour = region, linetype = region)) +
    geom_line(alpha = 0.6) +
    scale_linetype(name = 'World Bank Region') +
    scale_color_manual(values = wes_palette("Darjeeling2"), 
                       name = 'World Bank Region') +
    xlab('') + ylab('10-year Bond Spread (change)') +
    annotate('text', x = 2008, y = 3, label = 'Iceland') +
    annotate('text', x = 2011, y = 7, label = 'Greece') +
    annotate('text', x = 2011, y = 5, label = 'Portugal') +
    annotate('text', x = 2011, y = 4, label = 'Ireland') 

ggsave(wb_spreads, width = 10.7, height = 5.68,
       filename = 'paper_plots/reviewer_suggestions/spread_change_wb.pdf')

wb_volatility <- ggplot(oecd, aes(year, d_lt_ratecov_fred, group = iso2c,
                               colour = region, linetype = region)) +
    geom_line(alpha = 0.6) +
    scale_linetype(name = 'World Bank Region') +
    scale_color_manual(values = wes_palette("Darjeeling2"), 
                       name = 'World Bank Region') +
    xlab('') + ylab('10-year Bond Spread (change)')


# Credit rating agencies -------------------------------------------------------
## Downloaded from: https://www.fitchratings.com/web_content/ratings/sovereign_ratings_history.xls
fitch <- import('analysis/data_and_misc/sovereign_ratings_history.xls',
                skip = 4)
fitch <- fitch[, 1:3] %>% setNames(c('country', 'date', 'fitch_lt_rating'))

# Keep last rating per year
fitch$year <- fitch$date %>% ymd %>% year
fitch <- fitch[!duplicated(fitch[, c('country', 'year')], fromLast = TRUE), ] 

fitch$iso2c <- countrycode(fitch$country, origin = 'country.name', 
                           destination = 'iso2c')
fitch <- fitch %>% select(iso2c, year, fitch_lt_rating) %>% 
    arrange(iso2c, year)
fitch$fitch_lt_rating[fitch$fitch_lt_rating == '-'] <- NA
fitch$fitch_lt_rating[fitch$fitch_lt_rating == 'withdrawn'] <- NA
fitch$fitch_lt_rating <- gsub('-', '_minus', fitch$fitch_lt_rating)
fitch$fitch_lt_rating <- gsub('\\+', '_plus', fitch$fitch_lt_rating)
fitch <- DropNA(fitch, c('iso2c', 'year'))

fitch <- TimeExpand(fitch, GroupVar = 'iso2c', TimeVar = 'year')
fitch <- fitch %>% group_by(iso2c) %>% 
    mutate(fitch_lt_rating = FillDown(Var = fitch_lt_rating))

fitch$fitch_lt_reduced <- NA
fitch$fitch_lt_reduced[grepl('A', fitch$fitch_lt_rating)] <- 'A_minus_to_AAA'
fitch$fitch_lt_reduced[grepl('B', fitch$fitch_lt_rating)] <- 'B_minus_to_BBB'
fitch$fitch_lt_reduced[grepl('C', fitch$fitch_lt_rating)] <- 'C_to_CCC'
fitch$fitch_lt_reduced[grepl('D', fitch$fitch_lt_rating)] <- 'D_to_DDD'
fitch$fitch_lt_reduced[grepl('RD', fitch$fitch_lt_rating)] <- 'RD'

table(fitch$fitch_lt_reduced)

# Convert to factors
fitch$fitch_lt_reduced <- factor(fitch$fitch_lt_reduced)

fitch$fitch_lt_rating <- factor(fitch$fitch_lt_rating)
ratings <- c('AAA', 'AA_plus', 'AA', 'AA_minus', 
             'A_plus', 'A', 'A_minus', 
             'BBB_plus', 'BBB', 'BBB_minus', 
             'BB_plus', 'BB',
             'BB_minus', 'B_plus', 'B', 'B_minus', 
             'CCC_plus', 'CCC', 'CCC_minus', 'C',
             'DDD', 'DD', 'D', 'RD')
fitch$fitch_lt_rating_labelled <- factor(fitch$fitch_lt_rating, 
                                levels = rev(ratings))

#fitch$fitch_lt_rating_labelled <- factor(fitch$fitch_lt_rating_labelled, 
#                                         levels = rev(levels(fitch$fitch_lt_rating_labelled)))

oecd <- merge(oecd, fitch, by = c('iso2c', 'year'), all.x = TRUE)
oecd_no_na <- DropNA(oecd, 'fitch_lt_rating_labelled')

ggplot(oecd_no_na, aes(year, fitch_lt_rating_labelled, group = iso2c)) +
    scale_y_discrete(limits = oecd$fitch_lt_rating_labelled) +
    geom_line(alpha = 0.5) 

# Level changes 
oecd$fitch_numeric <- 20 - as.numeric(oecd$fitch_lt_rating_labelled)

oecd <- change(oecd, Var = 'fitch_numeric', GroupVar = 'iso2c', 
               NewVar = 'd_fitch', type = 'absolute')
oecd <- slide(oecd, Var = 'd_fitch', GroupVar = 'iso2c', 
               NewVar = 'l_d_fitch')
oecd <- slide(oecd, Var = 'd_frt_2015', GroupVar = 'iso2c', 
              NewVar = 'l_d_frt_2015')

oecd <- subset(oecd, d_bond_spread_fred > -6)

ggplot(oecd, aes(d_fitch, d_bond_spread_fred)) +
    stat_bin2d(bins = 75) +
    stat_smooth(method = 'lm', se = FALSE) +
    geom_vline(xintercept = 0, linetype = 'dashed', alpha = '0.3') +
    geom_hline(yintercept = 0, linetype = 'dashed', alpha = '0.3') +
    xlab('\nFitch LT Rating Step Changes (lag)') + ylab('Bond Spread Change\n')

ggplot(oecd, aes(d_fitch, d_lt_ratecov_fred)) +
    stat_bin2d(bins = 75) +
    stat_smooth(method = 'lm', se = FALSE) +
    geom_vline(xintercept = 0, linetype = 'dashed', alpha = '0.3') +
    geom_hline(yintercept = 0, linetype = 'dashed', alpha = '0.3') +
    xlab('\nFitch LT Rating Step Changes') + ylab('Bond Volatility\n')

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
theme_set(theme_bw())

# Import data set
main <- import('/git_repositories/FRTIndex/paper/analysis/frt08_16_v2.dta')

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

overall <- ggplot(oecd, aes(year, d_bond_spread_fred, group = iso2c)) +
    geom_line(alpha = 0.3) +
    xlab('') + ylab('10-year Bond Spread (change)')

# Regional Peers
wb_regions <- ggplot(oecd, aes(year, d_bond_spread_fred, group = iso2c,
                               colour = region, linetype = region)) +
    geom_line(alpha = 0.4) +
    scale_linetype(name = 'World Bank Region') +
    scale_color_manual(values = wes_palette("Darjeeling2"), 
                       name = 'World Bank Region') +
    xlab('') + ylab('10-year Bond Spread (change)')

ggsave(wb_regions, width = 10.7, height = 5.68,
       filename = 'paper/paper_plots/reviewer_suggestions/spread_change_wb.pdf')

# Fitch Credit Rating
fitch_groups <- ggplot(oecd, aes(year, d_bond_spread_fred, group = iso2c,
                               colour = as.factor(fitch_lt_rating)
                               )) +
    geom_line(alpha = 0.4) +
#    scale_linetype(name = 'World Bank Region') +
#    scale_color_brewer(palette = 'Set2', name = 'World Bank Region') +
    xlab('') + ylab('10-year Bond Spread (change)')

# OECD credit ratings

plot(as.factor(oecd$fitch_lt_rating), oecd$frt_2015)
plot(as.factor(oecd$fitch_lt_rating), oecd$d_frt_2015)

# ---------------------------------------------------------------------------- #
# Japan outlier exporation
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #


# Load packages
library(rio)
library(dplyr)
library(dplyr)
library(ggplot2)

# Set working directory. Change as needed.
setwd('/git_repositories/FRTIndex/paper/')

# Load regression data set
main <- import('analysis/frt04_16_v1.dta')

# Find regression sample
sub <- main %>% DropNA(c('d_bond_spread_fred', 'l_frt', 'l_pubdebtgdp',
                         'l_cgdpgrowth'))

# Create Japan dummy
sub$japan <- 'Other'
sub$japan[sub$iso2c == 'JP'] <- 'Japan'
sub$japan <- as.factor(sub$japan)

ggplot(sub, aes(l_pubdebtgdp, fill = japan, 
                colour = japan, group = japan)) +
            geom_density(alpha = 0.5) +
            scale_color_manual(values = c('#fdbb84', '#bdbdbd'), name = '') +
            scale_fill_manual(values = c('#fdbb84', '#bdbdbd'), name = '') +
            ylab('Density\n') + xlab('\nPublic Debt/GDP (%)') +
            theme_bw()

ggsave('paper_plots/japan_density.pdf', width = 7.92, heigh = 6.65)

jp <- sub %>% filter(country == 'Japan')
ggplot(jp, aes(year, d_pubdebtgdp)) +
    geom_line() + theme_bw()

ggplot(jp, aes(year, pubdebtgdp)) +
    geom_line()



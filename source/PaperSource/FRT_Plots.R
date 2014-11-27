####################################
# Plot results from FRT_Stan
# Christopher Gandrud
# 27 November 2014
# MIT License
####################################

# Main figure output directory
dir <- '/git_repositories/FRTIndex/paper/paper_plots/'

# Load packages
library(gridExtra)

# Load stan_catterpillar function
SourceURL <- 'https://gist.githubusercontent.com/christophergandrud/9b6caf8fa6ed0cbb33c4/raw/211f98590903e96e87686b02e4436a207f49f2ad/stan_caterpillar.R'
devtools::source_url(SourceURL)

# Country list from FRT_Stan_v_02_1990.R
countries <- unique(BaseSub$country)

# Load simulations
load('~/Desktop/fit_2014-11-26.RData')

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


## Late 90s weirdness
sc_country('Austria')

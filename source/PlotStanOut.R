####################################
# Plot results from FRT_Stan
# Christopher Gandrud
# 26 November 2014
# MIT License
#################################### 

# Load packages
library(gridExtra)

# Load stan_catterpillar function
SourceURL <- 'https://gist.githubusercontent.com/christophergandrud/9b6caf8fa6ed0cbb33c4/raw/211f98590903e96e87686b02e4436a207f49f2ad/stan_caterpillar.R'
devtools::source_url(SourceURL)

# Country list from FRT_Stan_v_02_1990.R
countries <- unique(BaseSub$country)

# Load simulations
load('~/Desktop/fit_2014-11-26.RData')

# 1993

## 1998

## 2007

## 2011
stan_catterpillar(fit_NonIndp, params = 'alpha\\[.*,22\\]', 
                  params_labels = countries) +
                ylab('') + xlab('\nFRT Index (HPD)')


#### Plot individual countries ####
sc_country <- function(country) {
    number <- grep(pattern = country, x = countries)
    param_temp <- paste0('alpha\\[', number, ',.*\\]')
    stan_catterpillar(fit_NonIndp, params = param_temp, 
                      params_labels = 1990:2011, horizontal = FALSE, 
                      order_medians = FALSE) +
        ggtitle(paste0(country, '\n'))
}

sc_country('Canada')
sc_country('United States')
sc_country('Norway')
sc_country('Korea')
sc_country('Japan')
sc_country('United Kingdom')

p <- list()

for (i in countries){
    message(i)
    p[[i]] <- sc_country(i)
}

do.call(grid.arrange, c(p))

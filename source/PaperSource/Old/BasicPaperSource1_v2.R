##############
# Source 1 for figures in the FRT index paper
# Christopher Gandrud
# 29 September 2014
##############

# Model created using source/FRT_Stan_v_02_1990.R
# Item description table created in source/FRTIndex_CreatorV2.R

# --------------------------------------------------- #
#### Directories ####
# Set working directory
WDir <- '/git_repositories/FRTIndex/'
setwd(WDir)


# Load stan_catterpillar function
devtools::source_url('https://gist.githubusercontent.com/christophergandrud/9b6caf8fa6ed0cbb33c4/raw/211f98590903e96e87686b02e4436a207f49f2ad/stan_caterpillar.R')

# Create list of countries
countries <- unique(BaseSub$country)

# Create list of years
years <- 1990:2011

#### Create year summaries ####
# 1993
pdf('paper/figures/FRT_1993.pdf')
stan_catterpillar(fit, params = 'alpha\\[.*,3\\]', params_labels = countries) +
    ylab('') + xlab('\nFRT Index (HPD)') + theme_bw()
dev.off()

## 1998

## 2007

## 2011
pdf('paper/figures/FRT_2011.pdf')
stan_catterpillar(fit, params = 'alpha\\[.*,22\\]', params_labels = countries) +
    ylab('') + xlab('\nFRT Index (HPD)') + theme_bw()
dev.off()

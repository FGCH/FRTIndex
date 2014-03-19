##############
# Source for figures and tables in the basic FRT index description paper
# Christopher Gandrud
# 19 March 2014
##############

# Model created using source/FRTIndex_CreatorV2.R
# Item description table created in source/FRTIndex_CreatorV2.R

# Load libraries and functions
library(ggmcmc)

CatURL <- 'https://gist.githubusercontent.com/christophergandrud/9640110/raw/cf66251dbab4e176334e5b58a257eab358baf28c/ggs_caterpillar_label.R'
devtools::source_url(CatURL)

# --------------------------------------------------- #
#### Directories ####
# Set working directory
WDir <- '/git_repositories/FRTIndex/'
setwd(WDir)

# --------------------------------------------------- #
#### Load estimates ####
# Load main estimation model
load('modelOut/SetOut.RData')

# Load country identifyer data
Countries <- read.csv('source/ParameterDescript/CountryNumbers.csv',
                      stringsAsFactors = FALSE)


# --------------------------------------------------- #
#### Transparency plots ####
Countries$countrynum <- paste0('transparency\\[', Countries$countrynum, ',10.*')

ggs_caterpillar_label(Set, family = 'transparency.*,10.*',
                      param_label_from = Countries$countrynum,
                      param_label_to = Countries$country) +
  ylab('') + xlab('\nFRT Index (HPD)') + ggtitle('2007\n') +
  theme_bw()
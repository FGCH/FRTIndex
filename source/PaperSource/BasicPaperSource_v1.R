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
# Create country number identifier
Countries$countrynum <- paste0('transparency\\[', Countries$countrynum, ',.*')

pdf(file = 'paper/figures/FRT_1998.pdf', width = 12, height = 12)
ggs_caterpillar_label(Set, family = 'transparency.*,1\\].*',
                      param_label_from = Countries$countrynum,
                      param_label_to = Countries$country) +
  scale_x_continuous(limits = c(-17, 10)) + 
  ylab('') + xlab('\nFRT Index (HPD)') +
  theme_bw()
dev.off()

pdf(file = 'paper/figures/FRT_2007.pdf', width = 12, height = 12)
ggs_caterpillar_label(Set, family = 'transparency.*,10.*',
                      param_label_from = Countries$countrynum,
                      param_label_to = Countries$country) +
  scale_x_continuous(limits = c(-17, 10)) + 
  ylab('') + xlab('\nFRT Index (HPD)') +
  theme_bw()
dev.off()

pdf(file = 'paper/figures/FRT_2011.pdf', width = 12, height = 12)
ggs_caterpillar_label(Set, family = 'transparency.*,14.*',
                      param_label_from = Countries$countrynum,
                      param_label_to = Countries$country) +
  scale_x_continuous(limits = c(-17, 10)) + 
  ylab('') + xlab('\nFRT Index (HPD)') +
  theme_bw()
dev.off()

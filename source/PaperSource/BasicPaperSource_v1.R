##############
# Source for figures and tables in the basic FRT index description paper
# Christopher Gandrud
# 21 March 2014
##############

# Model created using source/FRTIndex_CreatorV2.R
# Item description table created in source/FRTIndex_CreatorV2.R

# Load libraries and functions
library(ggmcmc)

CatURL <- 'https://gist.githubusercontent.com/christophergandrud/9640110/raw/4a34144becd94bdbb2259a32410b21b7d3705c52/ggs_caterpillar_label.R'
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

Years <- read.csv('source/ParameterDescript/YearNumbers.csv',
                      stringsAsFactors = FALSE)

# --------------------------------------------------- #
#### Build identifiers ####
# Create country number identifier
Countries$countrynumMod <- paste0('transparency\\[', Countries$countrynum, ',.*')

# Create year identifiers
Years$yearnumMod <- paste0('transparency\\[.*,', Years$yearnum, '\\]')

# --------------------------------------------------- #
#### Transparency plots ####


pdf(file = 'paper/figures/FRT_1998.pdf', width = 12, height = 12)
ggs_caterpillar_label(Set, family = 'transparency.*,1\\].*',
                      param_label_from = Countries$countrynumMod,
                      param_label_to = Countries$country) +
  scale_x_continuous(limits = c(-17, 10)) + 
  ylab('') + xlab('\nFRT Index (HPD)') +
  theme_bw()
dev.off()

pdf(file = 'paper/figures/FRT_2007.pdf', width = 12, height = 12)
ggs_caterpillar_label(Set, family = 'transparency.*,10.*',
                      param_label_from = Countries$countrynumMod,
                      param_label_to = Countries$country) +
  scale_x_continuous(limits = c(-17, 10)) + 
  ylab('') + xlab('\nFRT Index (HPD)') +
  theme_bw()
dev.off()

pdf(file = 'paper/figures/FRT_2011.pdf', width = 12, height = 12)
ggs_caterpillar_label(Set, family = 'transparency.*,14.*',
                      param_label_from = Countries$countrynumMod,
                      param_label_to = Countries$country) +
  scale_x_continuous(limits = c(-17, 10)) + 
  ylab('') + xlab('\nFRT Index (HPD)') +
  theme_bw()
dev.off()

# --------------------------------------------------- #
#### Plot only Hungary ####
# Hungary identifyier
Hung <- Countries$countrynumMod[Countries$country == 'Hungary']

pdf(file = 'paper/figures/FRT_Hungary.pdf', width = 12, height = 10)
ggs_caterpillar_label(Set, family = Hung,
                      horizontal = FALSE,
                      param_label_from = Years$yearnumMod,
                      param_label_to = Years$year,
                      order = FALSE) +
  xlab('FRT Index (HPD)\n') + ylab('') +
  scale_y_discrete(breaks = c(1998, 2003, 2008, 2011)) +
  theme_bw()
dev.off()

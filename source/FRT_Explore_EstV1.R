############
# Explore FRT Index Estimates V1
# Christopher Gandrud
# 18 March 2014
############

# Set working directory

# Load libraries
library(ggmcmc)
library(DataCombine)

# Load ggs data frame created with 
load('~/Desktop/SetOut.RData')

## Subset for different parameter groups
Set$Parameter <- as.character(Set$Parameter)

# Difficulty parameters beta.*[1]
ggs_traceplot(Set, family = 'beta.\\[1\\]') + theme_bw()

ggs_traceplot(Set, family = 'beta.*\\[1\\]') + theme_bw()

ggs_caterpillar(Set, family = 'beta.*\\[1\\]') + theme_bw()

# Descrimination parameters beta.*[2]

ggs_caterpillar(Set, family = 'beta.*\\[2\\]') + theme_bw()


# Transparency by country
ggs_caterpillar(Set, family = 'transparency\\[30,.*\\]') + theme_bw()


#### FRT Index by year ####
# Load country identifyer data
Countries <- read.csv('/git_repositories/FRTIndex/source/ParameterDescript/CountryNumbers.csv',
                      stringsAsFactors = FALSE)

Countries$countrynum <- paste0('transparency\\[', Countries$countrynum, ',')
Countries$country <- paste0('FRT_', Countries$country)


ReplaceCountry <- data.frame(from = Countries$countrynum, to = Countries$country)
SetReName <- FindReplace(Set, Var = 'Parameter', replaceData = ReplaceCountry, exact = FALSE)
SetReName$Parameter <- gsub('FRT_', ' ')

# 2000 (yearnum 3)
ggs_caterpillar(SetReName, family = 'FRT.*[a-z]3') + 
  ylab('') + xlab('\nFRT Index (HPD)') + ggtitle('2000\n') +
  theme_bw()

# 2007 (yearnum 10)
ggs_caterpillar(SetReName, family = 'FRT.*10') + 
  ylab('') + xlab('\nFRT Index (HPD)') + ggtitle('2007\n') +
  theme_bw()

# 2011 (yearnum 14)
ggs_caterpillar(SetReName, family = 'FRT.*14') + 
  ylab('') + xlab('\nFRT Index (HPD)') + ggtitle('2011\n') +
  theme_bw()


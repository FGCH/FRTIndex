############
# Extract FRT Index point estimates (OECD robustness check)
# Christopher Gandrud
# 31 March 2014
############

# --------------------------------------------------- #
#### Directories ####
# Set working directory
WDir <- '/git_repositories/FRTIndex/'
setwd(WDir)

# --------------------------------------------------- #
# Load packages and functions
library(stringr)
library(DataCombine)
library(countrycode)

# Load ggs_summary function
source('source/miscFunctions/ggs_summary.R')

# --------------------------------------------------- #
#### Load estimates ####
# Load main estimation model
load('modelOut/SetOutOECD.RData')

# --------------------------------------------------- #
#### Load country/year/difficulty/discrimination number data ####
Countries <- read.csv('source/ParameterDescript/CountryNumbersOECD.csv',
                      stringsAsFactors = FALSE)

Years <- read.csv('source/ParameterDescript/YearNumbersOECD.csv',
                  stringsAsFactors = FALSE)

#Indicators <- read.csv('source/IndicatorDescript/IncludedIndicatorsOECD.csv',
                       stringsAsFactors = FALSE)
#Indicators$ID <- 1:nrow(Indicators)

# --------------------------------------------------- #
#### Build identifiers ####
# Create country/year/difficulty/discrimination number identifiers
Countries$countrynumMod <- paste0('transparency\\[',
                                  Countries$countrynum, '$')

# Create year identifiers
Years$yearnumMod <- paste0('^', Years$yearnum, '\\]')

# Create difficulty identifiers
#Indicators$difficultyID <- paste0('beta', Indicators$ID, '\\[1\\]')

# Create discrimination identifiers
#Indicators$discriminateID <- paste0('beta', Indicators$ID, '\\[2\\]')

# --------------------------------------------------- #
#### Financial Regualtory Transparency Index ####
FRT <- ggs_summary(Set, family = 'transparency')

ParamSplit <- data.frame(str_split_fixed(FRT$Parameter, ',', n = 2),
                         stringsAsFactors = FALSE)
names(ParamSplit) <- c('country', 'year')
ParamSplit$ID <- 1:nrow(ParamSplit) # Add merge ID

# Clean country names
ParamSplit <- FindReplace(ParamSplit, Var = 'country',
                          replaceData = Countries,
                          from = 'countrynumMod',
                          to = 'country', exact = FALSE)

# Clean years
ParamSplit <- FindReplace(ParamSplit, Var = 'year',
                          replaceData = Years,
                          from = 'yearnumMod',
                          to = 'year', exact = FALSE)

# Add iso2c countrycode
ParamSplit$iso2c <- countrycode(ParamSplit$country, origin = 'country.name',
                                destination = 'iso2c')

FRT <- FRT[, -1] # Drop Parameter variable
FRT$ID <- 1:nrow(FRT) # Add merge ID

# Merge with country-year names
FRT <- merge(ParamSplit, FRT, by = 'ID')

# Clean final data
FRT <- FRT[, -1]
FRT <- FRT[order(FRT$country, FRT$year), ]

FRT <- MoveFront(FRT, c('country', 'iso2c'))
names(FRT) <- c('country', 'iso2c', 'year', 'lower_95', 'lower_90', 'median',
               'upper_90', 'upper_95')

# Save output
write.csv(FRT, file = 'IndexData/FRTIndex_OECDv0_2.csv', row.names = FALSE)

# --------------------------------------------------- #
#### Difficulty Parameters ####
Diff <- ggs_summary(Set, family = 'beta.*\\[1\\].*')
Diff$Parameter <- as.character(Diff$Parameter)

Diff <- FindReplace(Diff, Var = 'Parameter', replaceData = Indicators,
                    from = 'difficultyID', to = 'Indicator.Name',
                    exact = FALSE)

names(Diff) <- c('Parameter', 'lower_95', 'lower_90', 'median', 'upper_90',
                 'upper_95')

write.csv(Diff, file = 'IndexData/OtherParameters/DifficultyParams.csv',
          row.names = FALSE)

# --------------------------------------------------- #
#### Discrimination Parameters ####
Disc <- ggs_summary(Set, family = 'beta.*\\[2\\].*')
Disc$Parameter <- as.character(Disc$Parameter)

Disc <- FindReplace(Disc, Var = 'Parameter', replaceData = Indicators,
                    from = 'discriminateID', to = 'Indicator.Name',
                    exact = FALSE)

names(Disc) <- c('Parameter', 'lower_95', 'lower_90', 'median', 'upper_90',
                 'upper_95')

write.csv(Disc, file = 'IndexData/OtherParameters/DiscriminationParams.csv',
          row.names = FALSE)

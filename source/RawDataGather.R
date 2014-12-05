################################################################################
# Gather raw indicator data from:
# - World Bank Development Indicators: http://data.worldbank.org/indicator
# - FRED: http://research.stlouisfed.org/fred2/
# Also generates simple graphs/statistics comparing the data sources.
# Christopher Gandrud
# 5 December 2014
# MIT License
################################################################################

# Set working directory. Change as needed.
setwd('/git_repositories/FRTIndex/')

# Load packages
library(repmis)
library(dplyr)
library(quantmod)
library(reshape2)
library(WDI)
library(DataCombine)
library(ggplot2)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#### Download GFDD data from the World Bank ####
Indicators <- c('GFDD.DI.01', 'GFDD.DI.03', 'GFDD.DI.04',
                'GFDD.DI.05', 'GFDD.DI.06', 'GFDD.DI.07',
                'GFDD.DI.08', 'GFDD.DI.11', 'GFDD.DI.14',
                'GFDD.EI.02', 'GFDD.EI.08', 'GFDD.OI.02',
                'GFDD.OI.07', 'GFDD.SI.04')

# Download indicators
Base <- WDI(indicator = Indicators, start = 1990, end = 2013, extra = TRUE)

# Keep countries with 'High income' (OECD and non-OECD classification)
BaseSub <- grepl.sub(data = Base, Var = 'income', pattern = 'High income')
Droppers <- c("iso3c", "region",  "capital", "longitude", "latitude",
            "income", "lending")
BaseSub <- BaseSub[, !(names(BaseSub) %in% Droppers)]

# ---------------------------------------------------------------------------- #
#### Create missingness indicators ####
KeeperLength <- length(Indicators)
IndSub <- Indicators[1:KeeperLength]
VarVec <- vector()

for (i in IndSub){
    BaseSub[, paste0('Rep_', i)] <- 1
    BaseSub[, paste0('Rep_', i)][is.na(BaseSub[, i])] <- 0

    temp <- paste0('Rep_', i)
    VarVec <- append(VarVec, temp)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#### Download GFDD data from the World Bank ####
#### Create indicator IDs ####
# Load included indicators from the GFDD
prefix <- read.csv('source/PaperSource/IndicatorDescript/IndicatorDescription.csv',
                    stringsAsFactors = FALSE)[, 'SeriesCode'] %>%
                gsub(pattern = 'GF', replacement = '', .) %>%
                gsub(pattern = '\\.', replacement = '', .)

# Load country codes included in the data
URL <- 'https://raw.githubusercontent.com/FGCH/FRTIndex/master/IndexData/FRTIndex.csv'
countries <- source_data(URL, stringsAsFactors = FALSE)[, 'iso2c'] %>% unique()

# Add FRED suffix
countries <- paste0(countries, 'A156NWDB')

# Combine into FRED variable ID
fred_id_temp <- sapply(prefix, paste0, countries)

fred_id <- NULL
for (i in 1:ncol(fred_id_temp)) fred_id <- c(fred_id,
    fred_id_temp[1:nrow(fred_id_temp), i])

    #### Gather data ####
    missing <- NULL
    fred_combined <- NULL
    for (u in fred_id){
        message(u)
        marker <- tryCatch(
            data.frame(getSymbols(Symbols = u, src = 'FRED',
            auto.assign = F)),
            error = function(e) e
            )
            if (inherits(marker, 'error')) {
                missing <- c(missing, u)
                next
            }

            # Clean up
            temp_name <- names(marker)
            names(marker) <- substring(temp_name, 1, 6) %>% paste0('GF', .)
            marker$iso2c <- substring(temp_name, 7, 8)
            marker$year <- rownames(marker) %>% substring(., 1, 4) %>%
            as.numeric()

            marker <- melt(marker, id.vars = c('iso2c', 'year'))

            if (is.null(fred_combined)) {
                fred_combined <- marker
                } else
                fred_combined <- rbind(fred_combined, marker)

                # Sleep to avoid being locked out
                Sys.sleep(3)
            }

## Clean
fred_combined <- subset(fred_combined, year >= 1990)
fred_combined_cast <- dcast(fred_combined, iso2c + year ~ variable)

#### Create missingness indicators ####
IndSub <- names(fred_combined_cast)[3:length(names(fred_combined_cast))]
VarVec <- vector()

for (i in IndSub){
    fred_combined_cast[, paste0('Rep_', i)] <- 1
    fred_combined_cast[, paste0('Rep_', i)][is.na(fred_combined_cast[, i])] <- 0

    temp <- paste0('Rep_', i)
    VarVec <- append(VarVec, temp)
}


# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#### Use FRED to Fill In Missing Values in World Bank version ####








# ---------------------------------------------------------------------------- #
#### Find the proportion of items reported ####
source('source/miscFunctions/PropReported.R')
PropRepor <- PropReported(BaseSub)
PropRepor <- PropRepor[order(PropRepor$country, PropRepor$year), ]
write.csv(PropRepor, file = paste0('IndexData/alternate/PropReported.csv'),
          row.names = FALSE)

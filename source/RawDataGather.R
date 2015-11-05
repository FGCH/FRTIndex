################################################################################
# Gather raw indicator data from:
# - World Bank Development Indicators: http://data.worldbank.org/indicator
# - FRED: http://research.stlouisfed.org/fred2/
# Also generates simple graphs/statistics comparing the data sources.
# Christopher Gandrud
# 5 November 2015
# MIT License
################################################################################

# Load packages
library(repmis)
library(dplyr)
library(quantmod)
library(reshape2)
library(WDI)
library(DataCombine)
library(ggplot2)
library(countrycode)

# Set working directory. Change as needed.
possibles <- c('/git_repositories/FRTIndex/',
              'FRTIndex/')

set_valid_wd(possibles)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#### Download GFDD data from the World Bank ####
Indicators <- c('GFDD.DI.01', 'GFDD.DI.03', 'GFDD.DI.04',
                'GFDD.DI.05', 'GFDD.DI.06', 'GFDD.DI.07',
                'GFDD.DI.08', 'GFDD.DI.11', 'GFDD.DI.14',
                'GFDD.EI.02', 'GFDD.EI.08', 'GFDD.OI.02',
                'GFDD.SI.04')

## EMBI+ + China + India country list
EMBI <- c(
    'Argentina',
    'Brazil',
    'Bulgaria',
    'China',
    'Colombia',
    'Ecuador',
    'Egypt',
    'India',
    'Mexico',
    'Morocco',
    'Nigeria',
    'Panama',
    'Peru',
    'Philippines',
    'Poland',
    'Russian Federation',
    'South Africa',
    'Turkey',
    'Ukraine',
    'Venezuela'
)

# Download indicators
Base <- WDI(indicator = Indicators, start = 1990, end = 2015, extra = TRUE)

# Keep countries with 'High income' (OECD and non-OECD classification)
BaseSub <- grepl.sub(data = Base, Var = 'income', pattern = 'High income')

# Subset for EMBI+, + China
embi_iso <- countrycode(EMBI, origin = 'country.name', destination = 'iso2c')
financial_embi <- grepl.sub(data = Base, Var = 'iso2c', pattern = embi_iso)

# Combine
BaseSub <- rbind(BaseSub, financial_embi)
BaseSub <- BaseSub[!duplicated(BaseSub[, c('country', 'year')]), ]

Droppers <- c("iso3c", "region",  "capital", "longitude", "latitude",
              "income", "lending")
BaseSub <- BaseSub[, !(names(BaseSub) %in% Droppers)]

BaseSub <- BaseSub %>% arrange(country, year)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#### Download GFDD data from the World Bank ####
#### Create indicator IDs ####
# Load included indicators from the GFDD
prefix <- read.csv('paper/IndicatorDescript/IndicatorDescription.csv',
                    stringsAsFactors = FALSE)[, 'SeriesCode'] %>%
                gsub(pattern = 'GF', replacement = '', .) %>%
                gsub(pattern = '\\.', replacement = '', .)

# Load country codes included in the data
countries <- BaseSub[, 'iso2c'] %>% unique()

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

# ---------------------------------------------------------------------------- #
#### Create comparative missingness plots for the two versions ####
#### Create missingness indicators ####
IndSub <- names(fred_combined_cast)[3:length(names(fred_combined_cast))]
#
for (i in IndSub){
    fred_combined_cast[, paste0('Rep_', i)] <- 1
    fred_combined_cast[, paste0('Rep_', i)][is.na(fred_combined_cast[, i])] <- 0
}

#### Find the proportion of items reported ####
source('source/miscFunctions/PropReported.R')
fredProp <- PropReported(fred_combined_cast)
fredProp <- fredProp[order(fredProp$iso2c, fredProp$year), ]
fredProp <- rename(fredProp, fred_PropReport = FRT_PropReport)

#### Create missingness indicators for World Bank version ####
names(BaseSub) <- gsub('\\.', '', names(BaseSub))
WBProp <- BaseSub[, c('iso2c', 'country', 'year', IndSub)]
KeeperLength <- length(IndSub)

for (i in IndSub){
    WBProp[, paste0('Rep_', i)] <- 1
    WBProp[, paste0('Rep_', i)][is.na(WBProp[, i])] <- 0
}

PropRepor <- PropReported(WBProp)

#### Merge both data sets ####
prop_combined <- merge(PropRepor, fredProp, by = c('iso2c', 'year'))

prop_combined$diff <- prop_combined$fred_PropReport -
                        prop_combined$FRT_PropReport

# Only plot those country-years where there is a difference
prop_comb_sub <- subset(prop_combined, diff != 0)

pdf(file = 'paper/paper_plots/FRED_vs_WorldBank.pdf')
    ggplot(prop_comb_sub, aes(FRT_PropReport, fred_PropReport, label = iso2c)) +
        geom_text(position = position_jitter(w = 0.05), alpha = 0.5) +
        scale_y_continuous(limits = c(0, 1)) +
        geom_abline(yintercept = 0, slope = 1, linetype = 'dashed') +
        ylab('Proportion Reported in FRED\n') +
        xlab('\nProportion Reported in World Bank') +
        theme_bw()
dev.off()

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#### Use FRED to Fill In Missing Values in World Bank version ####

# Remove . from BaseSub
names(BaseSub) <- gsub('\\.', '', names(BaseSub))

# Names for variables to FillIn
fred_names <- names(fred_combined_cast)[grep('^GFDD', names(fred_combined_cast))]

for (i in fred_names){
    message(i)
    fred_sub <- fred_combined_cast[, c('iso2c', 'year', i)]
    BaseSub <- FillIn(D1 = BaseSub, D2 = fred_sub, Var1 = i, Var2 = i)
}

# ---------------------------------------------------------------------------- #
#### Create missingness indicators ####
IndSub <- names(BaseSub)[grep('^GFDD', names(BaseSub))]

for (i in IndSub){
    BaseSub[, paste0('Rep_', i)] <- 1
    BaseSub[, paste0('Rep_', i)][is.na(BaseSub[, i])] <- 0
}

# ---------------------------------------------------------------------------- #
#### Find the proportion of items reported ####
source('source/miscFunctions/PropReported.R')
PropRepor <- PropReported(BaseSub)
PropRepor <- PropRepor[order(PropRepor$country, PropRepor$year), ]
write.csv(PropRepor, file = paste0('IndexData/alternate/PropReported.csv'),
          row.names = FALSE)

# ---------------------------------------------------------------------------- #
#### Save data ####
write.csv(BaseSub, 'source/RawData/wdi_fred_combined_GFDDv2015.csv', row.names = FALSE)

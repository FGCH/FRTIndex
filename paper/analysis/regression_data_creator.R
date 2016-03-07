#comb <- import('paper/analysis/frt0526.csv')

# ---------------------------------------------------------------------------- #
# Regression data set creator
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

library(rio)
library(dplyr)
library(WDI)
library(quantmod)
library(countrycode)
library(lubridate)
library(DataCombine)
library(tidyr)

# Set working directory. Change as needed.
setwd('/git_repositories/FRTIndex/')


# Load frt -------------------
frt <- import('IndexData/FRTIndex_v2.csv')
frt <- frt %>% select(iso2c, year, median) %>% rename(frt = median)

# Load hrv ----------------
hrv <- import('source/Hollyer_et_al_Compare/hrv_means.csv')

# Download data from WDI data ---------------
max_year <- max(frt$year)
indicators <- c('GC.DOD.TOTL.GD.ZS', 'FP.CPI.TOTL.ZG', 'NY.GDP.MKTP.KD.ZG',
                'NY.GDP.PCAP.KD')
wdi <- WDI(indicator = indicators, start = 1989, end = max_year, extra = T)

wdi <- wdi %>%
        rename(pubdebtgdp = GC.DOD.TOTL.GD.ZS) %>%
        rename(infl = FP.CPI.TOTL.ZG) %>%
        rename(cgdpgrowth = NY.GDP.MKTP.KD.ZG) %>%
        rename(pcgdp2005l = NY.GDP.PCAP.KD)

# Create list of OECD countries
oecd <- subset(wdi, income == 'High income: OECD')
oecd_growth <- oecd %>% group_by(year) %>%
                summarize(oecdgrowth = mean(cgdpgrowth, na.rm = T))

wdi <- wdi %>% select(iso2c, year, pubdebtgdp, infl, cgdpgrowth,
                      pcgdp2005l, income)

wdi <- merge(wdi, oecd_growth, by = 'year')


## IMF Historical Public Debt Data
# From: https://www.imf.org/external/pubs/cat/longres.aspx?sk=24332.0
hist_pubdebtgdp <- import('paper/analysis/data_and_misc/Debt Database Fall 2013 Vintage.xlsx',
                          sheet = 2)
hist_pubdebtgdp <- hist_pubdebtgdp[, c(1, 4:322)]
hist_pubdebtgdp <- hist_pubdebtgdp %>% gather(year, pubdebtgdp, 
                          2:ncol(hist_pubdebtgdp))

hist_pubdebtgdp$iso2c <- countrycode(hist_pubdebtgdp$country, 
                                     origin = 'country.name', 
                                     destination = 'iso2c')

hist_pubdebtgdp[, 2:3] <- sapply(hist_pubdebtgdp[, 2:3], as.numeric)
hist_pubdebtgdp <- hist_pubdebtgdp %>% filter(year >= 1989) %>% 
                        arrange(country, year) %>% select(-country)

wdi <- FillIn(D1 = wdi, D2 = hist_pubdebtgdp, Var1 = 'pubdebtgdp', 
              Var2 = 'pubdebtgdp')

# FRED Data --------------------------
# US 3-Month Yield (WTB3MS)
us_3month <- getSymbols(Symbols = 'WTB3MS', src = 'FRED', auto.assign = FALSE) %>%
                        data.frame %>% rename(us_3month = WTB3MS)

us_3month$year <- ymd(row.names(us_3month)) %>% year

us_3month <- us_3month %>% group_by(year) %>%
                summarise(us3mrate = mean(us_3month, na.rm = T))

# VIX (VIXCLS)
vix <- getSymbols(Symbols = 'VIXCLS', src = 'FRED', auto.assign = FALSE) %>%
    data.frame %>% rename(vix = VIXCLS)

vix$year <- ymd(row.names(vix)) %>% year

vix <- vix %>% group_by(year) %>%
    summarise(vix = mean(vix, na.rm = T))

fred_iv <- merge(us_3month, vix, by = 'year', all = T)

# Bond yields from FRED ---------------------------------------------------
# Load country codes included in the data
countries <- frt[, 'iso2c'] %>% unique()

# Create FRED ID IRLTLT01 iso2c  M156N
fred_id <- sprintf('IRLTLT01%sM156N', countries)

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
    marker$iso2c <- gsub('IRLTLT01', '', temp_name)
    marker$iso2c <- gsub('M156N', '', marker$iso2c)

    marker$date <- rownames(marker) %>% ymd

    names(marker) <- c('bond_10yr', 'iso2c', 'date')

    if (is.null(fred_combined)) {
        fred_combined <- marker
    } else
        fred_combined <- rbind(fred_combined, marker)

    # Sleep to avoid being locked out
    Sys.sleep(3)
}

# EMBI Bonds ---------------
# From: https://datamarket.com/data/set/1dme/jp-morgan-emerging-markets-bond-index-embi#!ds=1dme!x88=7.k.b.9.a.i.4.c.f.g.e.m.2.d.5.h.8.n&display=choropleth&map=world&classifier=natural&numclasses=5
embi_dir <- 'paper/analysis/data_and_misc/embi_datamarket/'
files_embi <- list.files(embi_dir)

embi_bonds <- data.frame()
for (i in files_embi) {
    path_temp <- paste0(embi_dir, i)
    temp <- import(path_temp)
    temp <- temp %>% gather(country, bond_10yr, 2:ncol(temp))
    temp$iso2c <- countrycode(temp$country, origin = 'country.name',
                              destination = 'iso2c')
    temp <- temp %>% select(iso2c, Year, bond_10yr)
    names(temp) <- names(temp) %>% tolower
    embi_bonds <- rbind(embi_bonds, temp) %>% arrange(iso2c, year)
}

# rescale
embi_bonds$bond_10yr <- embi_bonds$bond_10yr / 100

# Find bond yield spread vs US 10-year -------
usa <- fred_combined %>% filter(iso2c == 'US') %>% select(-iso2c) %>%
            rename(us_bond_10yr = bond_10yr)
bonds_fred <- merge(fred_combined, usa, by = 'date')
bonds_fred$fredspread <- bonds_fred$bond_10yr - bonds_fred$us_bond_10yr

bonds_fred$year <- bonds_fred$date %>% ymd %>% year

# Create coefficient of variation
cOv <- function(x) {
    (sd(x, na.rm = T) / mean(x, na.rm = T)) * 100
}

bonds_cov <- bonds_fred %>% group_by(iso2c, year) %>%
                summarise(ltratecov = cOv(bond_10yr))

# Combine with FRED and EMBI Bonds -------
bonds_fred_annual <- bonds_fred %>% group_by(iso2c, year) %>%
                        summarise(spread_feb2016 = mean(fredspread, na.rm = T)) %>%
                        arrange(iso2c, year)

# Use FRED data when available
embi_bonds <- embi_bonds %>% filter(!(iso2c %in% unique(bonds_fred_annual$iso2c)))

usa$year <- usa$date %>% ymd %>% year
usa_annual <- usa %>% group_by(year) %>%
                summarise(usa_10yr = mean(us_bond_10yr, na.rm = T))

embi_bonds <- merge(embi_bonds, usa_annual, by = 'year')
embi_bonds$spread_feb2016 <- embi_bonds$bond_10yr - embi_bonds$usa_10yr

embi_bonds <- embi_bonds %>% select(iso2c, year, bond_10yr) %>%
                rename(spread_feb2016 = bond_10yr)

comb_bonds <- rbind(bonds_fred_annual, embi_bonds)
comb_bonds <- merge(comb_bonds, bonds_cov, by = c('iso2c', 'year'), all = T)
FindDups(comb_bonds, c('iso2c', 'year'))

# DataStream Bonds -------------------------------------------------------
bonds_datastream <- import('paper/analysis/data_and_misc/datastream_bond_yld_quarterly.csv') %>%
                        select(iso2c, quarter, bond_yld_datastream)

bonds_datastream$year <- bonds_datastream$quarter %>% as.character %>%
                            substr(start = 1, stop = 4) %>% as.numeric

bonds_datastream <- bonds_datastream %>% group_by(iso2c, year) %>%
                        summarise(bond_10yr_ds = mean(bond_yld_datastream,
                                                      na.rm = T))

# Find bond yield spread vs US 10-year -------
usa_datastream <- bonds_datastream %>% filter(iso2c == 'US') %>%
    rename(us_bond_10yr = bond_10yr_ds)
usa_datastream <- usa_datastream[, c('year', 'us_bond_10yr')]
bonds_datastream <- merge(bonds_datastream, usa_datastream, by = 'year')
bonds_datastream$ds_spread <- bonds_datastream$bond_10yr_ds -
                                bonds_datastream$us_bond_10yr
bonds_datastream <- subset(bonds_datastream, !is.nan(ds_spread))

bonds_datastream <- bonds_datastream %>% select(iso2c, year, ds_spread) %>%
                        arrange(iso2c, year)


# Combine with original (need source) ---------
old <- import('paper/analysis/frt0526.csv')
old <- old %>% select(iso2c, year, newspread, ltratecov)

comb <- merge(old, comb_bonds, by = c('iso2c', 'year'))

plot(comb$newspread, comb$spread_feb2016, xlab = "Previous Spread Variable",
     ylab = "New Spread Variable (FRED + Data)")


plot(comb$ltratecov.x, comb$ltratecov.y, xlab = 'Previous Coef of Var.',
     ylab = "New Coef of Var. (FRED only)")

test <- subset(comb, comb$newspread - comb$spread_feb2016 > 0.1)

comb_ds <- merge(old, bonds_datastream, by = c('iso2c', 'year'))
plot(comb_ds$newspread, comb_ds$ds_spread, xlab = "Previous Spread Variable",
     ylab = "New Spread Variable (Datastream)")



# Examine series similarity
#test_comb <- merge(old, yr10, by = c('iso2c', 'year'))
#plot(test_comb$newspread.x, test_comb$newspread.y)

#test <- test_comb %>% filter(ltratecov.y < 35 & ltratecov.y > -35)
#plot(test$ltratecov.x, test$ltratecov.y)

#bond_10yr <- bind_rows(old, yr10) %>%
#    arrange(iso2c, year)

# Drop US for whose spread is 0
#bond_10yr$newspread[bond_10yr$iso2c == 'US'] <- NA


# Wang et al. GFS Fiscal transparency -----------
# Downloaded from https://www.imf.org/External/pubs/cat/longres.aspx?sk=43177.0
fiscal_trans <- import('paper/analysis/data_and_misc/wp15188.xlsx', 
                       sheet = "GFS Index Score", skip = 2)
fiscal_trans$iso2c <- countrycode(fiscal_trans$Country, origin = 'country.name',
                                  destination = 'iso2c')
fiscal_trans <- fiscal_trans[, c(-1, -2, -3)]

fiscal_trans <- fiscal_trans[, c(14, 1:10)]

fiscal_trans <- fiscal_trans %>% gather(year, fiscal_trans_gfs,
                                        2:ncol(fiscal_trans)) %>%
                                arrange(iso2c, year)
fiscal_trans$year <- fiscal_trans$year %>% as.numeric

# Combine ------------
comb <- merge(frt, bonds_datastream, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, bonds_cov, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, wdi, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, fred_iv, by = 'year', all.x = T)
comb <- merge(comb, fiscal_trans, by = c('iso2c', 'year'), all.x = T)

comb$year <- comb$year %>% as.numeric
comb <- comb %>% arrange(iso2c, year)
comb <- comb %>% MoveFront('income')

# Create lags and changes
vars <- names(comb)[4:ncol(comb)]

for (i in vars) {
    message(i)
    new_lag <- paste0('l', i)
    new_d <- paste0('d', i)
    comb <- slide(comb, Var = i, TimeVar = 'year', GroupVar = 'iso2c',
                  NewVar = new_lag)
    comb <- change(comb, Var = i, TimeVar = 'year', GroupVar = 'iso2c',
                   NewVar = new_d, type = 'proportion')
    comb[, new_lag][comb[, new_lag] == -Inf] <- NA
    comb[, new_lag][comb[, new_lag] == Inf] <- NA
    comb[, new_lag][is.nan(comb[, new_lag])] <- NA

    comb[, new_d][comb[, new_d] == -Inf] <- NA
    comb[, new_d][comb[, new_d] == Inf] <- NA
    comb[, new_d][is.nan(comb[, new_d])] <- NA
}

comb_full <- comb

# Fill in missing values from the original data set
#vars_comb <- names(comb)
#vars_same <- vars_comb[(vars_comb %in% names(old))]
#comb_sub <- comb %>% filter(year >= 2012)

#old <- old[, c(vars_same)]

#comb_full <- bind_rows(comb_sub, old) %>% arrange(iso2c, year)
#comb_full <- comb_full %>% select(-frt, -lfrt, -dfrt,
#                                  -ltratecov, -lltratecov, -dltratecov,
#                                  -fiscal_trans_gfs, -lfiscal_trans_gfs,
#                                  -dfiscal_trans_gfs)

#comb_full <- merge(comb_full, comb[, c(1, 2,
#                                       agrep('frt', names(comb)),
#                                       agrep('cov', names(comb)),
#                                       agrep('fiscal_trans', names(comb)))
#                                  ],
#                   by = c('iso2c', 'year'), all = T)

# Interactions
comb_full$lfrtxlpub <- comb_full$lfrt * comb_full$lpubdebtgdp
comb_full$dfrtxdpub <- comb_full$dfrt * comb_full$dpubdebtgdp

# Final clean up
comb_full$country <- countrycode(comb_full$iso2c, origin = 'iso2c',
                                 destination = 'country.name')
comb_full$imf_code <- countrycode(comb_full$iso2c, origin = 'iso2c',
                                 destination = 'imf')


comb_full <- MoveFront(comb_full, c('country', 'iso2c', 'imf_code',
                                    'year', 'income', 'frt'))

# Save
foreign::write.dta(comb_full, file = 'paper/analysis/frt0316_v1.dta')

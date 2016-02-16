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
wdi <- WDI(indicator = indicators, start = 1990, end = max_year, extra = T)

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
                      pcgdp2005l)

wdi <- merge(wdi, oecd_growth, by = 'year')

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

fred_iv <- merge(us_3month, vix, by = 'year')

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

# Find bond yield spread vs US 10-year
usa <- fred_combined %>% filter(iso2c == 'US') %>% select(-iso2c) %>%
            rename(us_bond_10yr = bond_10yr)

bonds <- merge(fred_combined, usa, by = 'date')

bonds$fredspread <- bonds$bond_10yr - bonds$us_bond_10yr

bonds$year <- bonds$date %>% ymd %>% year

cOv <- function(x) {
    (sd(x, na.rm = T) / mean(x, na.rm = T)) * 100
}

bonds_cov <- bonds %>% group_by(iso2c, year) %>%
                summarise(ltratecov = cOv(bond_10yr))


# Wang et al. GFS Fiscal transparency -----------
# Downloaded from https://www.imf.org/External/pubs/cat/longres.aspx?sk=43177.0
fiscal_trans <- import('paper/analysis/misc/wp15188.xlsx', sheet = "GFS Index Score", 
                       skip = 2)
fiscal_trans$iso2c <- countrycode(fiscal_trans$Country, origin = 'country.name',
                                    destination = 'iso2c')
fiscal_trans <- fiscal_trans[, c(-1, -2, -3)]

fiscal_trans <- fiscal_trans[, c(14, 1:10)]

fiscal_trans <- fiscal_trans %>% gather(year, fiscal_trans_gfs, 
                                        2:ncol(fiscal_trans)) %>%
    arrange(iso2c, year)
fiscal_trans$year <- fiscal_trans$year %>% as.numeric


# Bloomber and old bonds -------------------------------------------------------
bonds <- import('paper/analysis/misc/weekly_data.csv')

bonds$year <- ymd(bonds$date) %>% year
bonds$bond_spread_local_US_10yr_bloomberg <- bonds$bond_spread_local_US_10yr_bloomberg / 100

# Annual averages
yr10 <- bonds %>% group_by(iso2c, year) %>%
    summarise(yield_10yr = round(mean(bond_spread_local_US_10yr_bloomberg, 
                                      na.rm = T), 
                                 digits = 3))
yr10 <- subset(yr10, !is.nan(yield_10yr))

# Coefficient of variation 
yr10_sd <- bonds %>% group_by(iso2c, year) %>%
    summarise(yield_sd = round(sd(bond_spread_local_US_10yr_bloomberg, 
                                  na.rm = T), 
                               digits = 3))
yr10_sd <- subset(yr10_sd, !is.nan(yield_sd))


cov10 <- merge(yr10, yr10_sd, by = c('iso2c', 'year'))
cov10$ltratecov <- (cov10$yield_sd / cov10$yield_10yr) * 100

yr10 <- yr10 %>% rename(newspread = yield_10yr)
#yr10 <- merge(yr10, cov10, by = c('iso2c', 'year'))
#yr10 <- yr10 %>% select(-yield_sd, -yield_10yr)

# Combine with original (need source)
old <- import('paper/analysis/frt0526.csv')
#old <- old %>% select(iso2c, year, newspread, ltratecov) %>% filter(year < 2005)

# Examine series similarity
#test_comb <- merge(old, yr10, by = c('iso2c', 'year'))
#plot(test_comb$newspread.x, test_comb$newspread.y)

#test <- test_comb %>% filter(ltratecov.y < 35 & ltratecov.y > -35)
#plot(test$ltratecov.x, test$ltratecov.y)

#bond_10yr <- bind_rows(old, yr10) %>% 
#    arrange(iso2c, year)

# Drop US for whose spread is 0
#bond_10yr$newspread[bond_10yr$iso2c == 'US'] <- NA




# Combine ------------
comb <- merge(frt, bonds_cov, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, yr10, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, wdi, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, fred_iv, by = 'year', all.x = T)
comb <- merge(comb, fiscal_trans, by = c('iso2c', 'year'), all.x = T)

comb$year <- comb$year %>% as.numeric 
comb <- comb %>% arrange(iso2c, year)

# Create lags and changes
vars <- names(comb)[3:ncol(comb)]

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

# Fill in missing values from the original data set
vars_comb <- names(comb)
vars_same <- vars_comb[(vars_comb %in% names(old))]
comb_sub <- comb %>% filter(year >= 2012)

old <- old[, c(vars_same)]

comb_full <- bind_rows(comb_sub, old) %>% arrange(iso2c, year)
comb_full <- comb_full %>% select(-frt, -lfrt, -dfrt, 
                                  -ltratecov, -lltratecov, -dltratecov,
                                  -fiscal_trans_gfs, -lfiscal_trans_gfs, 
                                  -dfiscal_trans_gfs)

comb_full <- merge(comb_full, comb[, c(1, 2,
                                       agrep('frt', names(comb)),
                                       agrep('cov', names(comb)),
                                       agrep('fiscal_trans', names(comb)))  
                                  ],
                   by = c('iso2c', 'year'), all = T)

# Interactions
comb_full$lfrtxlpub <- comb_full$lfrt * comb_full$lpubdebtgdp
comb_full$dfrtxdpub <- comb_full$dfrt * comb_full$dpubdebtgdp

# Final clean up
comb_full$country <- countrycode(comb_full$iso2c, origin = 'iso2c',
                                 destination = 'country.name')
comb_full$imf_code <- countrycode(comb_full$iso2c, origin = 'iso2c',
                                 destination = 'imf')


comb_full <- MoveFront(comb_full, c('country', 'iso2c', 'imf_code',
                                    'year', 'frt'))

# Save 
foreign::write.dta(comb_full, file = 'paper/analysis/frt0215.dta')

#comb <- import('paper/analysis/frt0526.csv')

# ---------------------------------------------------------------------------- #
# Regression data set creator for FRT bond spreads paper
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

# Set working directory. Change as needed. --------
setwd('/git_repositories/FRTIndex/')

# Bond yields, spreads, and coefficient of variation --------------
source('paper/analysis/bond_data_creator.R')

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

# Create list of OECD countries ---------------
oecd <- subset(wdi, income == 'High income: OECD')
oecd_growth <- oecd %>% group_by(year) %>%
                summarize(oecdgrowth = mean(cgdpgrowth, na.rm = T))

wdi <- wdi %>% select(iso2c, year, pubdebtgdp, infl, cgdpgrowth,
                      pcgdp2005l, income)

wdi <- merge(wdi, oecd_growth, by = 'year')




## IMF Historical Public Debt Data ---------------
# From: https://www.imf.org/external/pubs/ft/weo/2015/02/weodata/download.aspx
hist_pubdebtgdp <- import('paper/analysis/data_and_misc/WEOOct2015all.csv',
                          na.string = 'n/a', header = TRUE)

hist_pubdebtgdp <- hist_pubdebtgdp %>% filter(`WEO Subject Code` == 'GGXWDG_NGDP')

hist_pubdebtgdp <- hist_pubdebtgdp[, c(2, 11:44)] 
hist_pubdebtgdp <- hist_pubdebtgdp %>%
    gather(year, pubdebtgdp, 2:ncol(hist_pubdebtgdp))

hist_pubdebtgdp$iso2c <- countrycode(hist_pubdebtgdp$ISO, 
                                     origin = 'iso3c', 
                                     destination = 'iso2c')

hist_pubdebtgdp[, 2:3] <- sapply(hist_pubdebtgdp[, 2:3], as.numeric)
hist_pubdebtgdp <- hist_pubdebtgdp %>% filter(year >= 1989) %>% 
    arrange(iso2c, year) %>% select(-ISO)
hist_pubdebtgdp <- hist_pubdebtgdp %>% DropNA('iso2c')

# Fill in Missing values of public debt
wdi <- FillIn(D1 = wdi, D2 = hist_pubdebtgdp, Var1 = 'pubdebtgdp', 
                          Var2 = 'pubdebtgdp')

FindDups(wdi, c('iso2c', 'year'))

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
comb <- merge(frt, hrv, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, bonds, by = c('iso2c', 'year'), all.x = T)
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
    new_lag <- paste0('l_', i)
    new_d <- paste0('d_', i)
    comb <- slide(comb, Var = i, TimeVar = 'year', GroupVar = 'iso2c',
                  NewVar = new_lag)
    comb <- change(comb, Var = i, TimeVar = 'year', GroupVar = 'iso2c',
                   NewVar = new_d, type = 'absolute')
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
comb_full$l_frtxl_pub <- comb_full$l_frt * comb_full$l_pubdebtgdp
comb_full$d_frtxd_pub <- comb_full$d_frt * comb_full$d_pubdebtgdp

# Final clean up
comb_full$country <- countrycode(comb_full$iso2c, origin = 'iso2c',
                                 destination = 'country.name')
comb_full$imf_code <- countrycode(comb_full$iso2c, origin = 'iso2c',
                                 destination = 'imf')


comb_full <- MoveFront(comb_full, c('country', 'iso2c', 'imf_code',
                                    'year', 'income', 'frt'))

FindDups(comb_full, c('iso2c', 'year'))

rmExcept('comb_full')

# Save
foreign::write.dta(comb_full, file = 'paper/analysis/frt04_16_v1.dta')

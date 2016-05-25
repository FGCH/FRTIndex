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
library(R.cache)
library(devtools)
library(fredr) # if not installed use devtools::install_github('christophergandrud/fredr')

# Set working directory. Change as needed. --------
setwd('/git_repositories/FRTIndex/')

# Bond yields, spreads, and coefficient of variation --------------
source('paper/analysis/bond_data_creator.R')

# Load frt (v2016) -------------------
frt <- import('IndexData/FRTIndex_v2.csv')
frt <- frt %>% select(iso2c, year, median) %>% rename(frt = median)

# Load frt (v2015) -------------------
frt_2015 <- import('IndexData/FRTIndex.csv')
frt_2015 <- frt_2015 %>% select(iso2c, year, median) %>% 
    rename(frt_2015 = median)

# Load hrv ----------------
hrv <- import('source/Hollyer_et_al_Compare/hrv_means.csv')

# OECD membership dummy ----------------
source_gist('ca640f6effcdd9fedc3a6452de7c9f48')

# Download data from WDI data ---------------
max_year <- max(frt$year)
indicators <- c('GC.DOD.TOTL.GD.ZS', 'FP.CPI.TOTL.ZG', 'NY.GDP.MKTP.KD.ZG',
                'NY.GDP.PCAP.KD')
wdi <- WDI(indicator = indicators, start = 1989, end = max_year, extra = T)

wdi <- wdi %>%
        rename(pubdebtgdp_cent_wdi = GC.DOD.TOTL.GD.ZS) %>%
        rename(infl = FP.CPI.TOTL.ZG) %>%
        rename(cgdpgrowth = NY.GDP.MKTP.KD.ZG) %>%
        rename(pcgdp2005l = NY.GDP.PCAP.KD)

# Create list of OECD countries ---------------
oecd <- subset(wdi, income == 'High income: OECD')
oecd_growth <- oecd %>% group_by(year) %>%
                summarize(oecdgrowth = mean(cgdpgrowth, na.rm = T))

# Extract central government debt and correct likely coding error
wdi_debt <- wdi[, c('iso2c', 'year', 'pubdebtgdp_cent_wdi')]
wdi_debt$pubdebtgdp_cent_wdi[wdi_debt$pubdebtgdp_cent_wdi >= 5102.68132] <- NA


wdi <- wdi %>% select(iso2c, year, infl, cgdpgrowth,
                      pcgdp2005l, income)

wdi <- merge(wdi, oecd_growth, by = 'year')

## FRED central government debt -----------------
# Time consuming download, so cache
iso2c_fred <- unique(frt$iso2c)
cache_key <- as.list(iso2c_fred)
debt_fred <- loadCache(key = cache_key)

if (is.null(debt_fred)) {
    debt_fred <- fred_loop(prefix = 'DEBTTL', suffix = 'A188A', 
                           iso2c = iso2c_fred,
                           var_name = 'pubdebtgdp_cent_fred')
    saveCache(debt_fred, key = cache_key)
}

debt_fred$year <- year(debt_fred$date)
debt_fred <- debt_fred %>% select(-date)

## Eurostat central government debt -------------
# Downloaded from http://ec.europa.eu/eurostat/data/database
# ESA 1995 consolidated debt
eurostat <- import('paper/analysis/data_and_misc/gov_q_ggdebt_1_Data.csv',
                   na.string = ':')

# Keep only central government debt % of GDP
eurostat <- eurostat %>% filter(SECTOR == 'Central government') %>%
                filter(UNIT == "Percentage of gross domestic product (GDP)") %>%
                filter(INDIC_NA == 'Government consolidated gross debt') %>%
                select(-UNIT, -INDIC_NA, -SECTOR) 

eurostat$year <- as.numeric(gsub('Q.', '', eurostat$TIME))
eurostat$Value <- as.numeric(eurostat$Value)

debt_eurostat <- eurostat %>% group_by(GEO, year) %>%
                summarise(pubdebtgdp_cent_eu = mean(Value, na.rm = TRUE)) %>%
                as.data.frame

debt_eurostat$iso2c <- countrycode(debt_eurostat$GEO, origin = 'country.name',
                             destination = 'iso2c')
debt_eurostat <- DropNA(debt_eurostat, 'iso2c') %>% select(-GEO)

## IMF Historical Public Debt Data ---------------
# From: https://www.imf.org/external/pubs/cat/longres.aspx?sk=24332.0
hist_pubdebtgdp <- import('paper/analysis/data_and_misc/Debt Database Fall 2013 Vintage.xlsx',
                          which = 2)
hist_pubdebtgdp <- hist_pubdebtgdp[, c(1, 4:324)]
hist_pubdebtgdp <- hist_pubdebtgdp %>% gather(year, pubdebtgdp_gen_imf, 
                                              2:ncol(hist_pubdebtgdp))

hist_pubdebtgdp$iso2c <- countrycode(hist_pubdebtgdp$country, 
                                     origin = 'country.name', 
                                     destination = 'iso2c')

hist_pubdebtgdp[, 2:3] <- sapply(hist_pubdebtgdp[, 2:3], as.numeric)
hist_pubdebtgdp <- hist_pubdebtgdp %>% filter(year >= 1989) %>% 
    arrange(country, year) %>% select(-country)


## WEO Public Debt Data ---------------
# From: https://www.imf.org/external/pubs/ft/weo/2015/02/weodata/download.aspx
weo_debt <- import('paper/analysis/data_and_misc/WEOOct2015all.csv',
                          na.string = 'n/a', header = TRUE)

weo_debt <- weo_debt %>% filter(`WEO Subject Code` == 'GGXWDG_NGDP')

weo_debt <- weo_debt[, c(2, 11:44)] 
weo_debt <- weo_debt %>% gather(year, pubdebtgdp_gen_weo, 2:ncol(weo_debt))

weo_debt$iso2c <- countrycode(weo_debt$ISO, 
                                     origin = 'iso3c', 
                                     destination = 'iso2c')

weo_debt[, 2:3] <- sapply(weo_debt[, 2:3], as.numeric)
weo_debt <- weo_debt %>% filter(year >= 1989) %>% arrange(iso2c, year) %>% 
                    select(-ISO)
weo_debt <- weo_debt %>% DropNA('iso2c')

# Create debt measures data frame --------------
debt_comb <- merge(debt_eurostat, wdi_debt, by = c('iso2c', 'year'), 
                   all = TRUE)
debt_comb <- merge(debt_comb, debt_fred, by = c('iso2c', 'year'), 
                   all = TRUE)
debt_comb <- merge(debt_comb, hist_pubdebtgdp, by = c('iso2c', 'year'), 
                   all = TRUE)
debt_comb <- merge(debt_comb, weo_debt, by = c('iso2c', 'year'), 
                   all = TRUE)

debt_comb <- subset(debt_comb, year > 1988)
debt_comb <- DropNA(debt_comb, 'iso2c')


# Fill in Missing values of public debt (t)
debt_comb$pubdebtgdp_cent <- debt_comb$pubdebtgdp_cent_wdi

debt_comb <- FillIn(D1 = debt_comb, D2 = debt_fred, Var1 = 'pubdebtgdp_cent', 
                          Var2 = 'pubdebtgdp_cent_fred')
# 228 filled in
debt_comb <- FillIn(D1 = debt_comb, D2 = debt_eurostat, 
                    Var1 = 'pubdebtgdp_cent', 
                    Var2 = 'pubdebtgdp_cent_eu')
# 37 filled in

# Fill in Missing values of public debt (general government)
debt_comb$pubdebtgdp_gen <- debt_comb$pubdebtgdp_gen_imf

debt_comb <- FillIn(D1 = debt_comb, D2 = weo_debt, 
                    Var1 = 'pubdebtgdp_gen', 
                    Var2 = 'pubdebtgdp_gen_weo')
# 625 filled in

FindDups(wdi, c('iso2c', 'year'))

# FRED (other) data --------------------------
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
                       which = 4, skip = 2)
fiscal_trans$iso2c <- countrycode(fiscal_trans$Country, origin = 'country.name',
                                  destination = 'iso2c')
fiscal_trans <- fiscal_trans[, c(-1, -2, -3)]

fiscal_trans <- fiscal_trans[, c(14, 1:10)]

fiscal_trans <- fiscal_trans %>% gather(year, fiscal_trans_gfs,
                                        2:ncol(fiscal_trans)) %>%
                                arrange(iso2c, year)
fiscal_trans$year <- fiscal_trans$year %>% as.numeric

# Combine ------------
comb <- merge(frt, frt_2015, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, hrv, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, oecd_members, c('iso2c', 'year'), all.x = T)
comb <- merge(comb, bonds, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, debt_comb, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, wdi, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, fred_iv, by = 'year', all.x = T)
comb <- merge(comb, fiscal_trans, by = c('iso2c', 'year'), all.x = T)

comb$year <- comb$year %>% as.numeric
comb <- comb %>% arrange(iso2c, year)
comb$oecd_member[is.na(comb$oecd_member)] <- 0
comb <- comb %>% MoveFront(c('income', 'oecd_member'))

# Create lags and changes --------------
vars <- names(comb)[5:ncol(comb)]

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

# Interactions ------------
comb_full$l_frtxl_pub_cent <- comb_full$l_frt * comb_full$l_pubdebtgdp_cent
comb_full$d_frtxd_pub_cent <- comb_full$d_frt * comb_full$d_pubdebtgdp_cent

comb_full$l_frtxl_pub_gen <- comb_full$l_frt * comb_full$l_pubdebtgdp_gen
comb_full$d_frtxd_pub_gen <- comb_full$d_frt * comb_full$d_pubdebtgdp_gen

# Final clean up
comb_full$country <- countrycode(comb_full$iso2c, origin = 'iso2c',
                                 destination = 'country.name')
comb_full$imf_code <- countrycode(comb_full$iso2c, origin = 'iso2c',
                                 destination = 'imf')


comb_full <- MoveFront(comb_full, c('country', 'iso2c', 'imf_code',
                                    'year', 'oecd_member', 'income', 'frt'))

FindDups(comb_full, c('iso2c', 'year'))

rmExcept('comb_full')

# Save
foreign::write.dta(comb_full, file = 'paper/analysis/frt04_16_v2.dta')

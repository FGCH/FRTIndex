# ---------------------------------------------------------------------------- 
# Regression data set creator for FRT bond spreads paper
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- 

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
library(psData)
library(fredr) # if not installed use devtools::install_github('christophergandrud/fredr')
library(spatialWeights) # if not installed use devtools::install_github('christophergandrud/spatialWeights')

# Set working directory. Change as needed. --------
setwd('/git_repositories/FRTIndex/')

# Bond yields, spreads, and coefficient of variation --------------
source('paper/analysis/bond_data_creator.R')

# Treat all US bond spreads as "missing" 
# (spread created as difference of US and others)
spread_vars <- names(bonds)[grep('spread', names(bonds))]
for (i in spread_vars) bonds[bonds$iso2c == 'US', i] <- NA

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
source('https://gist.githubusercontent.com/christophergandrud/ca640f6effcdd9fedc3a6452de7c9f48/raw/6d8f0ef8081ea02221a79eb7f4b451699725800a/oecd_membership_dummy.R')

# Eurozone membership dummy
euro <- import('https://raw.githubusercontent.com/christophergandrud/euro_membership/master/data/euro_membership_data.csv') %>% 
    select(-country)

euro$euro_member <- 1

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

# Convert Per capita GDP to 1000s of dollars 
wdi$pcgdp2005l <- wdi$pcgdp2005l/1000

# Create list of OECD countries ---------------
oecd <- subset(wdi, income == 'High income: OECD')
oecd_growth <- oecd %>% group_by(year) %>%
                summarize(oecdgrowth = mean(cgdpgrowth, na.rm = T))

# Extract central government debt and correct likely coding error
wdi_debt <- wdi[, c('iso2c', 'year', 'pubdebtgdp_cent_wdi')]
wdi_debt$pubdebtgdp_cent_wdi[wdi_debt$pubdebtgdp_cent_wdi >= 5102.68132] <- NA


wdi <- wdi %>% select(iso2c, year, infl, cgdpgrowth,
                      pcgdp2005l, income, region)

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

# DPI Executive election timing (Gandrud corrected) ----------------------------
elections <- import('https://raw.githubusercontent.com/christophergandrud/yrcurnt_corrected/master/data/yrcurnt_original_corrected.csv')

elections$exec_election_yr <- 0
elections$exec_election_yr[elections$yrcurnt_corrected == 0] <- 1

elections <- elections %>% select(iso2c, year, exec_election_yr)

# DPI Executive left-right ideology --------------------------------------------
dpi <- DpiGet(vars = 'execrlc') %>% select(iso2c, year, execrlc) %>%
        rename(dpi_execrlc = execrlc)

# Remove downloaded file
unlink('DPI2015', recursive = TRUE)

dpi$dpi_execrlc[dpi$dpi_execrlc == -999] <- NA

dpi$dpi_left <- 0
dpi$dpi_left[dpi$dpi_execrlc == 3] <- 1

dpi <- dpi %>% select(iso2c, year, dpi_left)

# Unified Democracy Scores ---------------------------------------------------------
## Downloaded from: http://www.unified-democracy-scores.org/uds.html
uds <- import('paper/analysis/data_and_misc/uds_summary.csv')

uds$iso2c <- countrycode(uds$country, origin = 'country.name',
                         destination = 'iso2c')

uds <- uds %>% select(iso2c, year, mean) %>% rename(uds = mean)

# Fitch Sovereign Bond Ratings -------------------------------------------------
## Downloaded from: https://www.fitchratings.com/web_content/ratings/sovereign_ratings_history.xls
fitch <- import('paper/analysis/data_and_misc/sovereign_ratings_history.xls',
                skip = 4)
fitch <- fitch[, 1:3] %>% setNames(c('country', 'date', 'fitch_lt_rating'))

# Keep last rating per year
fitch$year <- fitch$date %>% ymd %>% year
fitch <- fitch[!duplicated(fitch[, c('country', 'year')], fromLast = TRUE), ] 

fitch$iso2c <- countrycode(fitch$country, origin = 'country.name', 
                           destination = 'iso2c')
fitch <- fitch %>% select(iso2c, year, fitch_lt_rating) %>% 
    arrange(iso2c, year)
fitch$fitch_lt_rating[fitch$fitch_lt_rating == '-'] <- NA
fitch$fitch_lt_rating[fitch$fitch_lt_rating == 'withdrawn'] <- NA
fitch$fitch_lt_rating <- gsub('-', '_minus', fitch$fitch_lt_rating)
fitch$fitch_lt_rating <- gsub('\\+', '_plus', fitch$fitch_lt_rating)
fitch <- DropNA(fitch, c('iso2c', 'year'))

fitch <- TimeExpand(fitch, GroupVar = 'iso2c', TimeVar = 'year')
fitch <- fitch %>% group_by(iso2c) %>% 
    mutate(fitch_lt_rating = FillDown(Var = fitch_lt_rating))

fitch$fitch_lt_reduced <- NA
fitch$fitch_lt_reduced[grepl('A', fitch$fitch_lt_rating)] <- 'A_minus_to_AAA'
fitch$fitch_lt_reduced[grepl('B', fitch$fitch_lt_rating)] <- 'B_minus_to_BBB'
fitch$fitch_lt_reduced[grepl('C', fitch$fitch_lt_rating)] <- 'C_to_CCC'
fitch$fitch_lt_reduced[grepl('D', fitch$fitch_lt_rating)] <- 'D_to_DDD'
fitch$fitch_lt_reduced[grepl('RD', fitch$fitch_lt_rating)] <- 'RD'

# Convert to factors
fitch$fitch_lt_reduced <- factor(fitch$fitch_lt_reduced)

fitch$fitch_lt_rating <- factor(fitch$fitch_lt_rating)
ratings <- c('AAA', 'AA_plus', 'AA', 'AA_minus', 
             'A_plus', 'A', 'A_minus', 
             'BBB_plus', 'BBB', 'BBB_minus', 
             'BB_plus', 'BB',
             'BB_minus', 'B_plus', 'B', 'B_minus', 
             'CCC_plus', 'CCC', 'CCC_minus', 'C',
             'DDD', 'DD', 'D', 'RD')
fitch$fitch_lt_rating_labelled <- factor(fitch$fitch_lt_rating, 
                                         levels = rev(ratings))


# FinStress measure of financial market stress ---------------------------------
URL <- 'https://raw.githubusercontent.com/christophergandrud/EIUCrisesMeasure/master/data/FinStress.csv'
finstress_index <- import(URL) %>% select(-country)

finstress_index$year <- year(finstress_index$date)

finstress_index <- finstress_index %>% group_by(iso2c, year) %>%
                        summarise(finstress = mean(FinStress)) %>%
                        select(iso2c, year, finstress)

# Combine ------------
comb <- merge(frt, frt_2015, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, hrv, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, oecd_members, c('iso2c', 'year'), all.x = T)
comb <- merge(comb, euro, c('iso2c', 'year'), all.x = T)
comb <- merge(comb, bonds, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, debt_comb, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, wdi, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, fred_iv, by = 'year', all.x = T)
comb <- merge(comb, fiscal_trans, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, elections, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, dpi, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, uds, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, fitch, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, finstress_index, by = c('iso2c', 'year'), all.x = T)

# Remove Cyprus (often duplicated and lacks FRED bond spread data)
comb <- subset(comb, iso2c != 'CY')

# Clean up Euro membership
comb$euro_member[is.na(comb$euro_member)] <- 0

comb$year <- comb$year %>% as.numeric
comb <- comb %>% arrange(iso2c, year)

# Create ESM rules dummy 
comb$esm_rules <- 0
comb$esm_rules[comb$euro_member == 1 & comb$year >= 2011] <-1

comb$oecd_member[is.na(comb$oecd_member)] <- 0
comb <- comb %>% MoveFront(c('iso2c', 'year', 'income', 'region', 
                             'oecd_member', 'euro_member', 'esm_rules'))

# Create lags and changes --------------
vars <- names(comb)[6:ncol(comb)] # include Euro_member and ESM lags

lag_changes_creator <- function(comb, vars) {
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
    return(comb)
}

comb <- lag_changes_creator(comb = comb, vars = vars)

# Create spatial weigths (GDP per capita) --------------------------
gdp_weights_spread <- monadic_spatial_weights(comb, id_var = 'iso2c', 
                                              location_var = 'pcgdp2005l',
                                              y_var = 'd_bond_spread_fred', 
                                              time = 'year', mc_cores = 1)

gdp_weights_volatility <- monadic_spatial_weights(comb, id_var = 'iso2c', 
                                                  location_var = 'pcgdp2005l',
                                                  y_var = 'd_lt_ratecov_fred', 
                                                  time = 'year', mc_cores = 1)

comb <- merge(comb, gdp_weights_spread, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, gdp_weights_volatility, by = c('iso2c', 'year'), all.x = T)

# Create geographic region --------------------------
region_weights_spread <- monadic_spatial_weights(comb, id_var = 'iso2c', 
                                                 location_var = 'region',
                                                 y_var = 'd_bond_spread_fred', 
                                                 time = 'year', mc_cores = 1,
                                                 location_var_class = 'categorical')

region_weights_volatility <- monadic_spatial_weights(comb, id_var = 'iso2c', 
                                                     location_var = 'region',
                                                     y_var = 'd_lt_ratecov_fred', 
                                                     time_var = 'year', mc_cores = 1,
                                                     location_var_class = 'categorical')
# Euro membership spatial weights
euro_weights_spread <- monadic_spatial_weights(df = comb, id_var = 'iso2c', 
                                                 location_var = 'euro_member',
                                                 y_var = 'd_bond_spread_fred', 
                                                 time_var = 'year', mc_cores = 1, 
                                                 morans_i = F,
                                                 location_var_class = 'categorical')

euro_weights_volatility <- monadic_spatial_weights(comb, id_var = 'iso2c', 
                                                     location_var = 'euro_member',
                                                     y_var = 'd_lt_ratecov_fred', 
                                                     time = 'year', mc_cores = 1,
                                                     morans_i = F,
                                                     location_var_class = 'categorical')

comb <- merge(comb, region_weights_spread, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, region_weights_volatility, by = c('iso2c', 'year'), 
              all.x = T)
comb <- merge(comb, euro_weights_spread, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, euro_weights_volatility, by = c('iso2c', 'year'), 
              all.x = T)

# Create Fitch credit rating peers --------------------------
fitch_weights_spread <- monadic_spatial_weights(comb, id_var = 'iso2c', 
                                            location_var = 'fitch_lt_reduced',
                                            y_var = 'd_bond_spread_fred', 
                                            time = 'year', mc_cores = 1,
                                            location_var_class = 'categorical')

fitch_weights_volatility <- monadic_spatial_weights(comb, id_var = 'iso2c', 
                                            location_var = 'fitch_lt_reduced',
                                            y_var = 'd_lt_ratecov_fred', 
                                            time_var = 'year', mc_cores = 1,
                                            location_var_class = 'categorical')

comb <- merge(comb, fitch_weights_spread, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, fitch_weights_volatility, by = c('iso2c', 'year'), 
              all.x = T)

comb <- lag_changes_creator(comb = comb, 
                            vars = c('sp_wght_pcgdp2005l_d_bond_spread_fred',
                                     'sp_wght_pcgdp2005l_d_lt_ratecov_fred',
                                     'sp_wght_region_d_bond_spread_fred', 
                                     'sp_wght_region_d_lt_ratecov_fred',
                                     'sp_wght_euro_member_d_bond_spread_fred',
                                     'sp_wght_euro_member_d_lt_ratecov_fred',
                                     'sp_wght_fitch_lt_reduced_d_bond_spread_fred',
                                     'sp_wght_fitch_lt_reduced_d_lt_ratecov_fred'
                                     ))

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

comb_full$l_fitch_lt_reduced <- factor(comb_full$l_fitch_lt_reduced,
                                       labels = c('A_minus_to_AAA', 
                                                  'B_minus_to_BBB', 'C_to_CCC',
                                                  'D_to_DDD', 'RD'))
comb_full$l_fitch_lt_reduced <- as.character(comb_full$l_fitch_lt_reduced)


comb_full <- MoveFront(comb_full, c('country', 'iso2c', 'imf_code',
                                    'year', 'oecd_member', 'income', 'frt'))

FindDups(comb_full, c('iso2c', 'year'))

rmExcept('comb_full')

# Save
foreign::write.dta(comb_full, file = 'paper/analysis/frt08_16_v2.dta')


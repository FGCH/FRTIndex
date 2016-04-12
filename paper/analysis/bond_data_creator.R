# ---------------------------------------------------------------------------- #
# Create bond yield and covariane indicators for the dependent variables
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

library(rio)
library(dplyr)
library(WDI)
library(quantmod)
library(countrycode)
library(lubridate)
library(DataCombine)
library(tidyr)
library(ggplot2)

# Set working directory. Change as needed ---------
setwd('/git_repositories/FRTIndex/')


# Bond yields from FRED ---------------------------------------------------
# Load country codes included in the data
#countries <- frt[, 'iso2c'] %>% unique

# Create FRED ID IRLTLT01 iso2c  M156N
#fred_id <- sprintf('IRLTLT01%sM156N', countries)

# Gather data 
#missing <- NULL
#fred_combined <- NULL
#for (u in fred_id){
#    message(u)
#    marker <- tryCatch(
#        data.frame(getSymbols(Symbols = u, src = 'FRED',
#                              auto.assign = F)),
#        error = function(e) e
#    )
#    if (inherits(marker, 'error')) {
#        missing <- c(missing, u)
#        next
#    }
#    
#    # Clean up
#    temp_name <- names(marker)
#    marker$iso2c <- gsub('IRLTLT01', '', temp_name)
#    marker$iso2c <- gsub('M156N', '', marker$iso2c)
#    
#    marker$date <- rownames(marker) %>% ymd
#    
#    names(marker) <- c('bond_yld_10yr_fred', 'iso2c', 'date')
#    
#    if (is.null(fred_combined)) {
#        fred_combined <- marker
#    } else
#        fred_combined <- rbind(fred_combined, marker)
#    
#    # Sleep to avoid being locked out
#    Sys.sleep(3)
#}

# Save for offline use
# export(fred_combined, 'paper/analysis/data_and_misc/fred_bond_spreads.csv')
fred_combined <- import('paper/analysis/data_and_misc/fred_bond_spreads.csv')

########## Temp ###########
fred_combined <- fred_combined %>% rename(bond_yld_10yr_fred = bond_10yr)
fred_combined$year <- fred_combined$date %>% ymd %>% year

# Create annual average FRED spread -- Calendar year ---------
bonds_fred <- fred_combined %>% select(iso2c, year, bond_yld_10yr_fred) %>%
    group_by(iso2c, year) %>%
    summarise(bond_yld_10yr_fred = mean(bond_yld_10yr_fred))

# Find bond yield spread vs US 10-year -------
usa <- fred_combined %>% filter(iso2c == 'US') %>% 
                group_by(iso2c, year) %>%
                summarise(us_bond_10yr = mean(bond_yld_10yr_fred)) %>%
                as_data_frame

usa <- usa %>% select(-iso2c)

bonds_fred <- merge(bonds_fred, usa, by = 'year')

bonds_fred$bond_spread_fred <- bonds_fred$bond_yld_10yr_fred - bonds_fred$us_bond_10yr

# Create annual average FRED spread -- Data release year ---------
## Data tends to be released after the first quarter
fred_combined <- slide(fred_combined, Var = 'date', GroupVar = 'iso2c', 
                       TimeVar = 'date', slideBy = -3, NewVar = 'release_date')
fred_combined$release_year <- fred_combined$release_date %>% ymd %>% year

bonds_fred_release <- fred_combined %>% 
    select(iso2c, release_year, bond_yld_10yr_fred) %>%
    group_by(iso2c, release_year) %>%
    summarise(bond_yld_10yr_fred = mean(bond_yld_10yr_fred))

# Find bond yield spread vs US 10-year -------
usa_release <- fred_combined %>% filter(iso2c == 'US') %>% 
    group_by(iso2c, release_year) %>%
    summarise(us_bond_10yr = mean(bond_yld_10yr_fred)) %>%
    as_data_frame

usa_release <- usa_release %>% select(-iso2c)

bonds_fred_release <- merge(bonds_fred_release, usa_release, 
                            by = 'release_year')

bonds_fred_release$bond_spread_fred_release_year <- bonds_fred_release$bond_yld_10yr_fred - 
                                        bonds_fred_release$us_bond_10yr

bonds_fred_release <- bonds_fred_release %>% select(iso2c, release_year, 
                                                    bond_spread_fred_release_year) %>%
                            rename(year = release_year) %>%
                            arrange(iso2c, year)

# Create coefficient of variation--Only for Fred as Data Market is annual ---------
cOv <- function(x) {
        (sd(x, na.rm = T) / mean(x, na.rm = T)) * 100
}

bonds_cov_fred <- fred_combined %>% group_by(iso2c, year) %>%
                    summarise(lt_ratecov_fred = cOv(bond_yld_10yr_fred))

bonds_cov_fred_release <- fred_combined %>% group_by(iso2c, release_year) %>%
                            summarise(lt_ratecov_fred_release_year = 
                                          cOv(bond_yld_10yr_fred))

bonds_cov_fred_release <- bonds_cov_fred_release %>% select(iso2c, release_year, 
                                                            lt_ratecov_fred_release_year) %>%
                                rename(year = release_year) %>% arrange(iso2c, year)

# Merge spreads and coefficients of variation
bonds_fred <- merge(bonds_fred, bonds_fred_release, by = c('iso2c', 'year'), 
                    all = TRUE)
bonds_fred <- merge(bonds_fred, bonds_cov_fred, by = c('iso2c', 'year'), 
                    all = TRUE)
bonds_fred <- merge(bonds_fred, bonds_cov_fred_release, by = c('iso2c', 'year'), 
                    all = TRUE)

FindDups(bonds_fred, c('iso2c', 'year'))

# Data Market Bonds ---------------
# From: https://datamarket.com/data/set/1dme/jp-morgan-emerging-markets-bond-index-embi#!ds=1dme!x88=7.k.b.9.a.i.4.c.f.g.e.m.2.d.5.h.8.n&display=choropleth&map=world&classifier=natural&numclasses=5
datamarket_dir <- 'paper/analysis/data_and_misc/embi_datamarket//'
files_datamarket <- list.files(datamarket_dir)

bonds_datamarket <- data.frame()
for (i in files_datamarket) {
    path_temp <- paste0(datamarket_dir, i)
    temp <- import(path_temp)
    temp <- temp %>% gather(country, bond_10yr, 2:ncol(temp))
    temp$iso2c <- countrycode(temp$country, origin = 'country.name',
                              destination = 'iso2c')
    temp <- temp %>% select(iso2c, Year, bond_10yr) %>% 
                rename(bond_yld_datamarket = bond_10yr) # Maturity actually unknown
    names(temp) <- names(temp) %>% tolower
    bonds_datamarket <- rbind(bonds_datamarket, temp) %>% arrange(iso2c, year)
}

# Drop NAs
bonds_datamarket <- DropNA(bonds_datamarket)

# Rescale to match FRED
bonds_datamarket$bond_yld_datamarket <- bonds_datamarket$bond_yld_datamarket / 100

# Create spread vs US (FRED data)
#usa$year <- usa$date %>% ymd %>% year
#usa_annual <- usa %>% group_by(year) %>%
#    summarise(usa_10yr = mean(us_bond_10yr, na.rm = T))

bonds_datamarket <- merge(bonds_datamarket, usa, by = 'year')
bonds_datamarket$bond_spread_datamarket <- bonds_datamarket$bond_yld_datamarket - bonds_datamarket$us_bond_10yr

bonds_datamarket <- bonds_datamarket %>% select(iso2c, year, bond_yld_datamarket, bond_spread_datamarket) 

# DataStream Bonds -------------------------------------------------------
bonds_datastream <- import('paper/analysis/data_and_misc/datastream_bond_yld_quarterly.csv') %>%
                        select(iso2c, quarter, bond_yld_datastream)

bonds_datastream$year <- bonds_datastream$quarter %>% as.character %>%
                            substr(start = 1, stop = 4) %>% as.numeric

bonds_datastream <- bonds_datastream %>% group_by(iso2c, year) %>%
                    summarise(bond_yld_datastream = mean(bond_yld_datastream,
                                  na.rm = T))

# Find bond yield spread vs US 10-year (Datastream) -------
usa_datastream <- bonds_datastream %>% filter(iso2c == 'US') %>%
                    rename(us_bond_10yr = bond_yld_datastream)
usa_datastream <- usa_datastream[, c('year', 'us_bond_10yr')]
bonds_datastream <- merge(bonds_datastream, usa_datastream, by = 'year')
bonds_datastream$bond_spread_datastream <- bonds_datastream$bond_yld_datastream -
    bonds_datastream$us_bond_10yr
bonds_datastream <- subset(bonds_datastream, !is.nan(bond_spread_datastream))

bonds_datastream <- bonds_datastream %>% select(iso2c, year, 
                                                bond_yld_datastream,
                                                bond_spread_datastream) %>%
                        arrange(iso2c, year)
FindDups(bonds_datastream, c('iso2c', 'year'))

# Combine bond indicators
bonds <- merge(bonds_fred, bonds_datastream, by = c('iso2c', 'year'), all = TRUE)
bonds <- merge(bonds, bonds_datamarket, by = c('iso2c', 'year'), all = TRUE)

# Fill in FRED bond spread data with DataStream when FRED is missing  ----------
bonds_ds <- bonds %>% select(iso2c, year, bond_spread_datastream)
bonds$bond_spread_fred_datastream <- bonds$bond_spread_fred
bonds <- FillIn(D1 = bonds, D2 = bonds_ds, Var1 = 'bond_spread_fred_datastream',
                Var2 = 'bond_spread_datastream')

bonds <- DropNA(bonds, 'year')


#bonds_ds <- bonds %>% select(iso2c, year, bond_spread_fred)
#bonds$bond_spread_fred_datastream <- bonds$bond_spread_datastream
#bonds <- FillIn(D1 = bonds, D2 = bonds_ds, Var1 = 'bond_spread_fred_datastream',
#                Var2 = 'bond_spread_fred')


FindDups(bonds, c('iso2c', 'year'))

rmExcept('bonds')

export(bonds, file = 'paper/analysis/data_and_misc/combined_bond_data.csv')
    


# Combine with original (need source) ---------
#old <- import('paper/analysis/frt0526.csv')
#old <- old %>% select(iso2c, year, newspread, ltratecov)

#comb <- merge(old, comb_bonds, by = c('iso2c', 'year'))

#plot(comb$newspread, comb$spread_feb2016, xlab = "Previous Spread Variable",
#     ylab = "New Spread Variable (FRED + Data)")


#plot(comb$ltratecov.x, comb$ltratecov.y, xlab = 'Previous Coef of Var.',
#     ylab = "New Coef of Var. (FRED only)")

#test <- subset(comb, comb$newspread - comb$spread_feb2016 > 0.1)

#comb_ds <- merge(old, bonds_datastream, by = c('iso2c', 'year'))
#plot(comb_ds$newspread, comb_ds$bond_spread_datastream, xlab = "Previous Spread #Variable",
#     ylab = "New Spread Variable (Datastream)")



# Examine series similarity
#test_comb <- merge(old, yr10, by = c('iso2c', 'year'))
#plot(test_comb$newspread.x, test_comb$newspread.y)

#test <- test_comb %>% filter(ltratecov.y < 35 & ltratecov.y > -35)
#plot(test$ltratecov.x, test$ltratecov.y)

#bond_10yr <- bind_rows(old, yr10) %>%
#    arrange(iso2c, year)

# Drop US for whose spread is 0
# bond_10yr$newspread[bond_10yr$iso2c == 'US'] <- NA


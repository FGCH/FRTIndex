################################################################################
# Create Country Sample Table
# Christopher Gandrud
# MIT License
################################################################################

# Set working directory. Change as needed
setwd('/git_repositories/FRTIndex/paper/')

# Load required packages
library(rio)
library(DataCombine)
library(xtable)

# Load data
main <- import('analysis/frt04_16_v2.dta')
main <- main %>% arrange(country, year)

# Drop non-OECD countries
main <- subset(main, country != 'Russian Federation')
main <- subset(main, country != 'South Africa')

model_vars <- c('d_bond_spread_fred', 'l_bond_spread_fred', 
                'd_frt_2015', 'd_pubdebtgdp_gen', 
                'l_frt_2015', 'l_pubdebtgdp_gen', 'l_infl',
                'd_infl', 'l_cgdpgrowth', 'd_cgdpgrowth', 'l_pcgdp2005l',
                'd_pcgdp2005l', 'l_oecdgrowth', 'd_oecdgrowth', 'l_us3mrate',
                'd_us3mrate', 'l_vix', 'd_vix')

cases <- CasesTable(main, GroupVar = 'country', TimeVar = 'year', Vars = model_vars)

names(cases) <- c('Country', 'First Year', 'Last Year')

print(xtable(cases, digits = 0), include.rownames = FALSE, 
      floating = FALSE, size = 'tiny', file = 'tables/oecd_sample.tex')

# ------------------ Old ----------------------------------------------------- #

spreads_sample <- unique(main_sub1$sname)
cov_sample <- unique(main_sub2$sname)

equal_columns <- function(x) {
    round2 = function(x, digits) {
        posneg = sign(x)
        z = abs(x)*10 ^ digits
        z = z + 0.5
        z = trunc(z)
        z = z/10 ^ digits
        z * posneg
    }
    
    
    total <- length(x)
    base_divided <- round2(total / 2, digits = 0)
    
    out_matrix <- matrix(nrow = base_divided, ncol = 0)
    
    for (i in 1:2) {
        if (i == 1) {
            temp <- x[1:base_divided]
        }
        else if (i == 2) {
            i_minus <- i - 1
            divided_plus_i_minus <- (base_divided * i_minus) + i_minus
            temp <- x[divided_plus_i_minus:total]
            temp <- c(temp, rep(NA, base_divided - length(temp)))
        }
        out_matrix <- cbind(out_matrix, temp)
    }
    colnames(out_matrix) <- NULL
    return(out_matrix)
}

spreads_sample <- equal_columns(spreads_sample)
cov_sample <- equal_columns(cov_sample)

#print(xtable(spreads_sample), include.rownames = FALSE, include.colnames = F,
#      floating = FALSE, size = 'tiny', file = 'tables/spreads_sample.tex')

#print(xtable(cov_sample), include.rownames = FALSE, include.colnames = F,
#      floating = FALSE, size = 'tiny', file = 'tables/cof_var_sample.tex')

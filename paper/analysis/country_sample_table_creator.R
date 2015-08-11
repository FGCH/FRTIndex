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
main <- import('analysis/frt0526.csv')
main <- main %>% arrange(sname)

main_sub1 <- main %>% DropNA(c('dnewspread', 'lnewspread', 'dfrt', 'dpubdebtgdp', 
                     'dfrtxdpub', 'lfrt', 'lpubdebtgdp', 'lfrtxlpub', 'linfl',
                     'dinfl', 'lcgdpgrowth', 'dcgdpgrowth', 'lpcgdp2005l',
                     'dpcgdp2005l', 'loecdgrowth', 'doecdgrowth', 'lus3mrate',
                     'dus3mrate', 'lvix', 'dvix'))

main_sub2 <- main %>% DropNA(c('dratecov', 'lltratecov', 'dfrt', 'dpubdebtgdp', 
                               'dfrtxdpub', 'lfrt', 'lpubdebtgdp', 'lfrtxlpub', 'linfl',
                               'dinfl', 'lcgdpgrowth', 'dcgdpgrowth', 'lpcgdp2005l',
                               'dpcgdp2005l', 'loecdgrowth', 'doecdgrowth', 'lus3mrate',
                               'dus3mrate', 'lvix', 'dvix'
                               ))

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

print(xtable(spreads_sample), include.rownames = FALSE, include.colnames = F,
      floating = FALSE, size = 'tiny', file = 'tables/spreads_sample.tex')

print(xtable(cov_sample), include.rownames = FALSE, include.colnames = F,
      floating = FALSE, size = 'tiny', file = 'tables/cof_var_sample.tex')

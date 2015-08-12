################################################################################
# Create LaTeX tables from Stata output
# Christopher Gandrud
# MIT License
################################################################################

# Load packages
library(dplyr)
library(foreign)
library(DataCombine)
library(xtable)
library(rio)

# Raw tables created with bond_models_frt_vs_hrv.do in Stata 12.1

# Don't clean LaTeX syntax
options(xtable.sanitize.text.function = identity)
options(xtable.sanitize.colnames.function = identity)

# Set working directory. Change as needed
setwd('/git_repositories/FRTIndex/source/Hollyer_et_al_Compare/')

# Get list of individual model tables
AllFiles <- list.files('tables/')

filesHRV <- AllFiles[grep('^HRV', AllFiles)]

# Combine into data frames
CombineFiles <- function(file_list, start){
    for (i in file_list) {
        temp <- import(paste0('tables/', i))
        if (i == start) out <- temp
        else out <- merge(out, temp, by = 'var', all = TRUE, sort = FALSE)
    }
    return(out)
}

CleanUp <- data.frame(
    from = c('^.*?_stderr', '_coef', '_cons', 'N_clust', 'N$', 'r2_a',
             'lfrt_residuals', 'dfrt_residuals',
             'lfrt_residxlpub', 'dfrt_residxdpub',
             'dhrvxdpub', 'lhrvxlpub',
             'dfrtxdpub', 'lfrtxlpub', 
             'lcgdpgrowth', 'dcgdpgrowth', 
             'lpcgdp2005l', 'dpcgdp2005l',
             'dnewspread', 'lnewspread',
             '^lltrate$', '^lltspreadus$', '^lltratecov$',
             '^lfrt$', '^dfrt$',
             '^lfrt_log$', '^dfrt_log$',
             'lhrv_mean', 'dhrv_mean',
             'lstrucbalgdp', 'dstrucbalgdp',
             'lpubdebtgdp', 'dpubdebtgdp',
             'linfl', 'dinfl',
             'lus3mrate', 'dus3mrate',
             'loecdgrowth', 'doecdgrowth',
             'lvix', 'dvix',
             'lcountry_growth', 'dcountry_growth',
             'eurozone'
    ),
    to = c('', '', 'Constant', 'Countries', 'Observations', 'Adjusted R-squared',
           'FRT Residuals$_{t-1}$', '$\\\\Delta$ FRT Residuals',
           'FRT Residuals$_{t-1}$ * Public debt/GDP (\\\\%)$_{t-1}$', '$\\\\Delta$ FRT Residuals * $\\\\Delta$ Public debt/GDP',
           '$\\\\Delta$ HRV * $\\\\Delta$ Public debt/GDP', 'HRV$_{t-1}$ * Public debt/GDP (\\\\%)$_{t-1}$',
           '$\\\\Delta$ FRT * $\\\\Delta$ Public debt/GDP', 'FRT$_{t-1}$ * Public debt/GDP (\\\\%)$_{t-1}$',
           'GDP Growth$_{t-1}$', '$\\\\Delta$ GDP Growth',
           'Per Capita GDP$_{t-1}$', '$\\\\Delta$ Per Capita GDP',
           '$\\\\Delta$ Bond Spread', 'Bond Spread$_{t-1}$',
           'LT rate$_{t-1}$', 'LT rate spread$_{t-1}$', 'LT rate COV$_{t-1}$',
           'FRT$_{t-1}$', '$\\\\Delta$ FRT',
           'FRT (log)$_{t-1}$', '$\\\\Delta$ FRT (log)',
           'HRV$_{t-1}$', '$\\\\Delta$ HRV',
           'Structural budget balance/GDP (\\\\%)$_{t-1}$', '$\\\\Delta$ Structural budget balance/GDP',
           'Public debt/GDP (\\\\%)$_{t-1}$', '$\\\\Delta$ Public debt/GDP',
           'Inflation (\\\\%) $_{t-1}$', '$\\\\Delta$ Inflation (\\\\%)',
           'US 3-month interest rate (\\\\%)$_{t-1}$', '$\\\\Delta$ US 3-month interest rate (\\\\%)',
           'OECD average GDP growth$_{t-1}$', '$\\\\Delta$ OECD average GDP growth',
           'VIX index$_{t-1}$', '$\\\\Delta$ VIX index',
           'Domestic GDP growth (\\\\%)$_{t-1}$', '$\\\\Delta$ Domestic GDP growth (\\\\%)',
           'Eurozone Member'
    )
)


outputHRV <- CombineFiles(filesHRV, start = 'HRV_1.dta')
outputHRV <- FindReplace(outputHRV, Var = 'var', replaceData = CleanUp, 
                         exact = F)
outputHRV <- outputHRV[c(74, 73, 84, 83, 75, 76, 77, 78, 87, 88, 
                         89, 86, 1:28, 81, 82, 79, 80, 90, 91, 92, 85,
                         29:30, 36, 32, 40), ]

# Insert blank row for formatting
blank <- c('', '', '', '', '', '')
row.names(outputHRV) <- 1:nrow(outputHRV)
outputHRV <- InsertRow(outputHRV, New = blank, RowNum = 51)
outputHRV <- InsertRow(outputHRV, New = blank, RowNum = 51)

names(outputHRV) <- c('',
                      '$\\Delta$ Long-term (10-year) bond spread (US 10-year bond, \\%)',
                      '$\\Delta$ Long-term (10-year) bond spread (US 10-year bond, \\%)',
                      '$\\Delta$ Coefficient of variation, LT bond spread (annual, based on monthly data)',
                      '$\\Delta$ Coefficient of variation, LT bond spread (annual, based on monthly data)',
                      '$\\Delta$ Long-term (10-year) bond spread (US 10-year bond, \\%)'
)

# Output
tableHRV <- xtable(outputHRV, dcolumn = TRUE, booktabs = TRUE)
align(tableHRV) <- 'llp{3cm}p{3cm}p{3cm}p{3cm}p{3cm}'
print(tableHRV, include.rownames = FALSE, floating = FALSE, size = 'tiny',
      file = 'tables/hrv_bond_results.tex')

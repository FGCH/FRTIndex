################################################################################
# Create LaTeX tables from Stata output
# Christopher Gandrud
# MIT License
################################################################################

# Load packages
library(dplyr)
library(rio)
library(DataCombine)
library(xtable)

# Raw tables created with combined_regressions_aug_2015 in Stata 12.1

# Don't clean LaTeX syntax
options(xtable.sanitize.text.function = identity)
options(xtable.sanitize.colnames.function = identity)

# Set working directory. Change as needed
setwd('/git_repositories/FRTIndex/paper/')

# Get list of individual model tables
AllFiles <- list.files('tables/')

filesFRTnoninteract <- AllFiles[grep('^FRT_noninteract', AllFiles)]

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


#### FRT ####
outputFRT_noninteract <- CombineFiles(filesFRTnoninteract, 
                                      start = 'FRT_noninteract_1.dta')
outputFRT_noninteract <- FindReplace(outputFRT_noninteract, Var = 'var', 
                                     replaceData = CleanUp, exact = F)
                         
outputFRT_noninteract <- outputFRT_noninteract[c(69:72, 1:26, 32, 28, 36), ]

# Insert blank row for formatting
blank <- c('', '', '')
row.names(outputFRT_noninteract) <- 1:nrow(outputFRT_noninteract)
outputFRT_noninteract <- InsertRow(outputFRT_noninteract, New = blank, 
                                   RowNum = 31)
outputFRT_noninteract <- InsertRow(outputFRT_noninteract, New = blank, 
                                   RowNum = 31)

names(outputFRT_noninteract) <- c('',
                      '$\\Delta$ Long-term (10-year) bond spread (US 10-year bond, \\%)',
                      '$\\Delta$ Coefficient of variation, LT bond (annual, based on monthly data)'
)

# Output
tableFRT <- xtable(outputFRT, dcolumn = TRUE, booktabs = TRUE)
align(tableFRT) <- 'llp{2cm}p{2cm}p{2cm}p{2cm}p{2cm}'
print(tableFRT, include.rownames = FALSE, floating = FALSE, size = 'tiny',
      file = 'tables/frt_bond_results.tex')

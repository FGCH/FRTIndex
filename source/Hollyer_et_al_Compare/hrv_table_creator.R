################################################################################
# Create LaTeX tables from Stata output
# Christopher Gandrud
# 28 May 2015
# MIT License
################################################################################

# Load packages
library(dplyr)
library(foreign)
library(DataCombine)
library(xtable)

# Raw tables created with bond_models_frt_vs_hrv.do in Stata 12.1

# Set working directory. Change as needed
setwd('/git_repositories/FRTIndex/source/Hollyer_et_al_Compare/tables')

# Get list of individual model tables
TableA <- data.frame
AllFiles <- list.files()

filesHRV <- AllFiles[grep('HRV', AllFiles)]

# Combine into data frames
CombineFiles <- function(file_list, start){
    for (i in file_list) {
        temp <- read.dta(i)
        if (i == start) out <- temp
        else out <- merge(out, temp, by = 'var', all = TRUE, sort = FALSE)
    }
    return(out)
}

CleanUp <- data.frame(
    from = c("^.*?_stderr", "_coef", "_cons", "N_clust", "N$", "r2_a",
             '^lltrate$', '^lltspreadus$', '^lltratecov$',
             "lhrv_mean", "dhrv_mean", 
             "lstrucbalgdp", "dstrucbalgdp",
             "lpubdebtgdp", "dpubdebtgdp",
             "linfl", "dinfl",
             "lus3mrate", "dus3mrate",
             "loecdgrowth", "doecdgrowth",
             "lvix", "dvix" 
             ),
    to = c('', '', "Constant", "Countries", "Observations", 
           "Adjusted R-squared",
           'LT rate$_{t-1}$', 'LT rate spread$_{t-1}$', 'LT rate COV$_{t-1}$',
           'HRV$_{t-1}$', '$\\\\Delta$ HRV', 
           'Structural budget balance/GDP (\\\\%)$_{t-1}$', '$\\\\Delta$ Structural budget balance/GDP',
           'Public debt/GDP (\\\\%)$_{t-1}$', '$\\\\Delta$ Public debt/GDP',
           'Inflation (\\\\%) $_{t-1}$', '$\\\\Delta$ Inflation (\\\\%)', 
           'US 3-month interest rate (\\\\%)$_{t-1}$', '$\\\\Delta$ US 3-month interest rate (\\\\%)', 
           'OECD average GDP growth$_{t-1}$', '$\\\\Delta$ OECD average GDP growth', 
           'VIX index$_{t-1}$', '$\\\\Delta$ Vix index'
           )
)

# HRV
outputHRV <- CombineFiles(filesHRV, start = 'HRV1.dta')
outputHRV <- FindReplace(outputHRV, Var = 'var', replaceData = CleanUp, exact = F)
outputHRV <- outputHRV[c(76, 73, 74, 75, 77, 78, 1:30, 36, 32, 40), ]

# Insert blank row for formatting
blank <- c('', '', '', '')
outputHRV <- InsertRow(outputHRV, New = blank, RowNum = 35)
outputHRV <- InsertRow(outputHRV, New = blank, RowNum = 35)

names(outputHRV) <- c('', 
                    '$\\Delta$ Long-term (10-year) interest rate (\\%)',
                    '$\\Delta$ LT rate spread (US 10-year bond)',
                    '$\\Delta$ Coefficient of variation, LT bond (annual, based on monthly data)')

# Output
options(xtable.sanitize.text.function=identity)
options(xtable.sanitize.colnames.function=identity)

tableHRV <- xtable(outputHRV, dcolumn = TRUE, booktabs = TRUE)
align(tableHRV) <- 'llp{3cm}p{3cm}p{3cm}'
print(tableHRV, include.rownames = FALSE, floating = FALSE, size = 'small',
      file = 'hrv_bond_results.tex')



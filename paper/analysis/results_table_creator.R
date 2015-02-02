################################################################################
# Create LaTeX tables from Stata output
# Christopher Gandrud
# 2 February 2015
# MIT License
################################################################################

# Load packages
library(dplyr)
library(foreign)
library(DataCombine)
library(xtable)

# Raw tables created with bond_models.do in Stata 12.1

# Don't clean LaTeX syntax
options(xtable.sanitize.text.function = identity)
options(xtable.sanitize.colnames.function = identity)

# Set working directory. Change as needed
setwd('/git_repositories/FRTIndex/paper/')

# Get list of individual model tables
AllFiles <- list.files('tables/')

filesFRT <- AllFiles[grep('^FRT', AllFiles)]
filesLogFRT <- AllFiles[grep('^log_FRT', AllFiles)]
filesHRV <- AllFiles[grep('^HRV', AllFiles)]

# Combine into data frames
CombineFiles <- function(file_list, start){
    for (i in file_list) {
        temp <- read.dta(paste0('tables/', i))
        if (i == start) out <- temp
        else out <- merge(out, temp, by = 'var', all = TRUE, sort = FALSE)
    }
    return(out)
}

CleanUp <- data.frame(
    from = c('^.*?_stderr', '_coef', '_cons', 'N_clust', 'N$', 'r2_a',
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
    to = c('', '', 'Constant', 'Countries', 'Observations',
           'Adjusted R-squared',
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
outputFRT <- CombineFiles(filesFRT, start = 'FRT1.dta')
outputFRT <- FindReplace(outputFRT, Var = 'var', replaceData = CleanUp, 
                         exact = F)
outputFRT <- outputFRT[c(78, 75, 76:77, 69:70, 1:24, 73:74, 72:71, 79:84,
                         25:26, 32, 28, 36), ]

# Insert blank row for formatting
blank <- c('', '', '', '', '', '')
row.names(outputFRT) <- 1:nrow(outputFRT)
outputFRT <- InsertRow(outputFRT, New = blank, RowNum = 43)
outputFRT <- InsertRow(outputFRT, New = blank, RowNum = 43)

# Highlight key finding
outputFRT[7:8, 4] <- paste0('\\textbf{', outputFRT[7:8, 4], '}')
outputFRT[7:8, 5] <- paste0('\\textbf{', outputFRT[7:8, 5], '}')
outputFRT[7:8, 6] <- paste0('\\textbf{', outputFRT[7:8, 6], '}')

names(outputFRT) <- c('',
                      '$\\Delta$ Long-term (10-year) interest rate (\\%)',
                      '$\\Delta$ LT rate spread (US 10-year bond, \\%)',
                      '$\\Delta$ Coefficient of variation, LT bond (annual, based on monthly data)',
                      '$\\Delta$ Coefficient of variation, LT bond (annual, based on monthly data), \\textbf{Excluding Canada}',
                      '$\\Delta$ Coefficient of variation, LT bond (annual, based on monthly data)'
                      )

# Output
tableFRT <- xtable(outputFRT, dcolumn = TRUE, booktabs = TRUE)
align(tableFRT) <- 'llp{2cm}p{2cm}p{2cm}p{2cm}p{2cm}'
print(tableFRT, include.rownames = FALSE, floating = FALSE, size = 'tiny',
      file = 'tables/frt_bond_results.tex')

## Logged FRT ####
outputFRT_log <- CombineFiles(filesLogFRT, start = 'log_FRT1.dta')
outputFRT_log <- FindReplace(outputFRT_log, Var = 'var', replaceData = CleanUp, 
                             exact = F)
outputFRT_log <- outputFRT_log[c(1:28, 34, 30, 38), ]

# Insert blank row for formatting
blank <- c('', '', '')
row.names(outputFRT_log) <- 1:nrow(outputFRT_log)
outputFRT_log <- InsertRow(outputFRT_log, New = blank, RowNum = 29)
outputFRT_log <- InsertRow(outputFRT_log, New = blank, RowNum = 29)

# Highlight key finding
outputFRT_log[3:4, 2] <- paste0('\\textbf{', outputFRT_log[3:4, 2], '}')
outputFRT_log[3:4, 3] <- paste0('\\textbf{', outputFRT_log[3:4, 3], '}')

names(outputFRT_log) <- c('',
                      '$\\Delta$ Coefficient of variation, LT bond (annual, based on monthly data)',
                      '$\\Delta$ Coefficient of variation, LT bond (annual, based on monthly data), \\textbf{Excluding Canada}'
                        )

# Output
tableFRTlog <- xtable(outputFRT_log, dcolumn = TRUE, booktabs = TRUE)
align(tableFRTlog) <- 'llp{3cm}p{3cm}'
print(tableFRTlog, include.rownames = FALSE, floating = FALSE, size = 'tiny',
      file = 'tables/frt_log_bond_results.tex')


#### HRV ####
outputHRV <- CombineFiles(filesHRV, start = 'HRV1.dta')
outputHRV <- FindReplace(outputHRV, Var = 'var', replaceData = CleanUp, 
                         exact = F)
outputHRV <- outputHRV[c(72:73, 74, 71, 77:78, 1:24, 75:76, 69:70, 25:26, 32, 
                         28, 36), ]

# Insert blank row for formatting
blank <- c('', '', '', '')
row.names(outputHRV) <- 1:nrow(outputHRV)
outputHRV <- InsertRow(outputHRV, New = blank, RowNum = 37)
outputHRV <- InsertRow(outputHRV, New = blank, RowNum = 37)

names(outputHRV) <- c('',
                    '$\\Delta$ Long-term (10-year) interest rate (\\%)',
                    '$\\Delta$ LT rate spread (US 10-year bond,  \\%)',
                    '$\\Delta$ Coefficient of variation, LT bond (annual, based on monthly data)'
                    )
# Output
tableHRV <- xtable(outputHRV, dcolumn = TRUE, booktabs = TRUE)
align(tableHRV) <- 'llp{2cm}p{2cm}p{2cm}'
print(tableHRV, include.rownames = FALSE, floating = FALSE, size = 'tiny',
      file = 'tables/hrv_bond_results.tex')


#### Country Sample Table Creator ####
full <- read.dta('analysis/frt_hrv_obi_bond.dta')

model_vars <- c('lltrate', 'lltspreadus', 'lltratecov',
    'lfrt', 'dfrt',
    'lstrucbalgdp', 'dstrucbalgdp',
    'lpubdebtgdp', 'dpubdebtgdp',
    'linfl', 'dinfl',
    'lus3mrate', 'dus3mrate',
    'loecdgrowth', 'doecdgrowth',
    'lvix', 'dvix'
)

complete <- full[complete.cases(full[, model_vars]),] %>% arrange(country, year)

countries <- unique(complete$country)

countries2 <- cbind(countries[1:15], countries[c(16:29, NA)])

print(xtable(countries2), include.rownames = F, include.colnames = F,
      floating = F, size = 'small',
      file = 'tables/regression_country_sample.tex')

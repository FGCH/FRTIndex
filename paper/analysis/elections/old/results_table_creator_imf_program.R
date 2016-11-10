################################################################################
# Create LaTeX tables from Stata output for models with IMF programme
# Christopher Gandrud
# MIT License
################################################################################

# Load packages
library(dplyr)
library(rio)
library(DataCombine)
library(xtable)

# Raw tables created with reviewer_suggestions/frt_imf_program.do in Stata 12.1

# Don't clean LaTeX syntax
options(xtable.sanitize.text.function = identity)
options(xtable.sanitize.colnames.function = identity)

# Set working directory. Change as needed
setwd('/git_repositories/FRTIndex/paper/')

# Get list of individual model tables
AllFiles <- list.files('tables/reviewer_suggestions/')

filesFRT <- AllFiles[grep('^FRT_[1-4]_imf.dta', AllFiles)]

# Combine into data frames
CombineFiles <- function(file_list, start){
    for (i in file_list) {
        temp <- import(paste0('tables/reviewer_suggestions/', i))
        if (i == start) out <- temp
        else out <- merge(out, temp, by = 'var', all = TRUE, sort = FALSE)
    }
    return(out)
}

CleanUp <- data.frame(
    from = c('^.*?_stderr', '_coef', '_cons', 'N_clust', 'N$', 'r2_a',
             'd_hrvxd_pub', 'l_hrvxl_pub',
             'd_frt_2015xd_pubdebtgdp_gen', 'l_frt2015xl_pub_gen',
             'l_cgdpgrowth', 'd_cgdpgrowth',
             'l_pcgdp2005l', 'd_pcgdp2005l',
             'd_bond_spread_fred', '^l_bond_spread_fred',
             '^lltrate$', '^lltspreadus$', '^l_lt_ratecov_fred$',
             '^l_frt_2015$', '^d_frt_2015$',
             '^l_frt_log$', '^d_frt_log$',
             'l_hrv_mean', 'd_hrv_mean',
             'l_strucbalgdp', 'd_strucbalgdp',
             'l_pubdebtgdp_gen', 'd_pubdebtgdp_gen',
             'l_infl', 'd_infl',
             'l_us3mrate', 'd_us3mrate',
             'l_oecdgrowth', 'd_oecdgrowth',
             'l_vix', 'd_vix',
             'l_country_growth', 'd_country_growth',
             'eurozone',
             'l_uds', 'd_uds',
             'l_exec_election_yr', 'd_exec_election_yr',
             'l_dpi_left', 
             'esm_rules',
             'imf_program_lag'
    ),
    to = c('', '', 'Constant', 'Countries', 'Observations', 'Adjusted R-squared',
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
           'Eurozone Member',
           'Democracy (UDS)$_{t-1}$', '$\\\\Delta$ Democracy (UDS)',
           'Exec. Election$_{t-1}$', '$\\\\Delta$ Exec. Election',
           'Left Executive$_{t-1}$',
           'ESM Rules Period',
           'IMF Program Start$_{t-1}$'
    )
)


#### FRT Interacted ####
outputFRT <- CombineFiles(filesFRT, start = 'FRT_1_imf.dta')
outputFRT <- FindReplace(outputFRT, Var = 'var', replaceData = CleanUp,
                         exact = F)

outputFRT <- outputFRT[c(89:90, 87:88, 1:38, 84:83, 86:85, 39:40,
                         46, 42, 50), ]

# Insert blank row for formatting
blank <- c('', '', '', '', '')
row.names(outputFRT) <- 1:nrow(outputFRT)
outputFRT <- InsertRow(outputFRT, New = blank, RowNum = 49)
outputFRT <- InsertRow(outputFRT, New = blank, RowNum = 49)

names(outputFRT) <- c('',
                    '$\\Delta$ Long-term (10-year) bond spread (US 10-year bond, \\%)',
                    '$\\Delta$ Long-term (10-year) bond spread (US 10-year bond, \\%)',
                    '$\\Delta$ Coefficient of variation, LT bond yields (annual, based on monthly data)',
                    '$\\Delta$ Coefficient of variation, LT bond yields (annual, based on monthly data)'
)

# Output
tableFRT_imf <- xtable(outputFRT, dcolumn = TRUE, booktabs = TRUE)
align(tableFRT_imf) <- 'llp{3cm}p{3cm}p{3cm}p{3cm}'
print(tableFRT_imf, include.rownames = FALSE, floating = FALSE, size = 'tiny',
      file = 'tables/frt_bond_results_imf.tex')

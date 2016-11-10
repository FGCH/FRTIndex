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
setwd('/git_repositories/FRTIndex/paper/analysis/Hollyer_et_al_Compare/')

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
             'l_frt_residuals', 'd_frt_residuals',
             'l_frt_residxlpub', 'd_frt_residxdpub',
             'd_hrvxd_pubdebtgdp_gen', 'l_hrvxl_pub_gen',
             'l_hrvxd_pub_gen', 'd_hrvxl_pub_gen',
             'd_frtxd_pub', 'l_frtxl_pub',
             'l_frt_residxdpub', 'd_frt_residxlpub',
             'l_cgdpgrowth', 'd_cgdpgrowth',
             'l_pcgdp2005l', 'd_pcgdp2005l',
             '^d_bond_spread_fred', '^l_bond_spread_fred',
             '^lltrate$', '^lltspreadus$', '^l_lt_ratecov_fred$',
             '^l_frt$', '^d_frt$',
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
           'FRT Residuals$_{t-1}$', '$\\\\Delta$ FRT Residuals',
           'FRT Residuals$_{t-1}$ * Public debt/GDP (\\\\%)$_{t-1}$', '$\\\\Delta$ FRT Residuals * $\\\\Delta$ Public debt/GDP',
           '$\\\\Delta$ HRV * $\\\\Delta$ Public debt/GDP', 'HRV$_{t-1}$ * Public debt/GDP (\\\\%)$_{t-1}$',
           'HRV$_{t-1}$ * $\\\\Delta$ Public debt/GDP', '$\\\\Delta$ HRV * Public debt/GDP (\\\\%)$_{t-1}$',
           '$\\\\Delta$ FRT * $\\\\Delta$ Public debt/GDP', 'FRT$_{t-1}$ * Public debt/GDP (\\\\%)$_{t-1}$',
           'FRT$_{t-1}$ * $\\\\Delta$ Public debt/GDP', '$\\\\Delta$ FRT * Public debt/GDP (\\\\%)$_{t-1}$',
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


outputHRV <- CombineFiles(filesHRV, start = 'HRV_1.dta')
outputHRV <- FindReplace(outputHRV, Var = 'var', replaceData = CleanUp,
                         exact = FALSE)

outputHRV <- outputHRV[c(77:78, 89:90, 79:82, 1:32, 88:85, 84:83, 91:92,
                         95:98, 99:102, 94:93, 33:34, 40, 36, 44
                         ), ]

# Insert blank row for formatting
blank <- c('', '', '', '', '', '')
row.names(outputHRV) <- 1:nrow(outputHRV)
outputHRV <- InsertRow(outputHRV, New = blank, RowNum = 61)
outputHRV <- InsertRow(outputHRV, New = blank, RowNum = 61)

names(outputHRV) <- c('',
                      '$\\Delta$ Long-term (10-year) bond spread (US 10-year bond, \\%)',
                      '$\\Delta$ Long-term (10-year) bond spread (US 10-year bond, \\%)',
                      '$\\Delta$ Coefficient of variation, LT bond yields (annual, based on monthly data)',
                      '$\\Delta$ Coefficient of variation, LT bond yields (annual, based on monthly data)',
                      '$\\Delta$ Long-term (10-year) bond spread (US 10-year bond, \\%)'
)

# Output
tableHRV <- xtable(outputHRV, dcolumn = TRUE, booktabs = TRUE)
align(tableHRV) <- 'llp{3cm}p{3cm}p{3cm}p{3cm}p{3cm}'
print(tableHRV, include.rownames = FALSE, floating = FALSE, size = 'tiny',
      file = 'tables/hrv_bond_results.tex')

#############
# Create indicator description file from http://data.worldbank.org/data-catalog/global-financial-development
# Christopher Gandrud
# 30 September 2014
############

setwd('/git_repositories/FRTIndex/source/IndicatorDescript/')

library(DataCombine)
library(xtable)

Descript <- read.csv(file = 'GFDD_Series.csv', stringsAsFactors = FALSE)
Descript <- Descript[, c('SeriesCode', 'Indicator.Name', 'Source', 'Periodicity')]

# List of indicators included
Indicators <- c('GFDD.DI.01', 'GFDD.DI.03', 'GFDD.DI.04',
                'GFDD.DI.05', 'GFDD.DI.06', 'GFDD.DI.07',
                'GFDD.DI.08', 'GFDD.DI.11',
                'GFDD.DI.14', 'GFDD.EI.02', 'GFDD.EI.08',
                'GFDD.OI.02', 'GFDD.OI.07',
                'GFDD.SI.04')

Descript <- subset(Descript, SeriesCode %in% Indicators)

write.csv(Descript, file = 'IndicatorDescription.csv', row.names = FALSE)

## Create .tex version
from = c('International Financial Statistics \\(IFS\\), International Monetary Fund \\(IMF\\)',
         'World Bank - Non banking financial database',
         'Nonbanking financial database, World Bank',
         'World Development Indicators \\(WDI\\), World Bank')
to = c('IFS', 'World Bank', 'World Bank', 'World Bank')
replacements <- data.frame(from, to)

Descript <- FindReplace(Descript, Var = 'Source', replaceData = replacements)

print.xtable(xtable(Descript), file = 'IndicatorDescription.tex', 
             floating = FALSE, type = 'latex', row.names = FALSE)

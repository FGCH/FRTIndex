#############
# Create indicator description file from http://data.worldbank.org/data-catalog/global-financial-development
# Christopher Gandrud
# 19 February 2014
############

setwd('/git_repositories/FRTIndex/source/IndicatorDescript/')

Descript <- read.csv(file = 'GFDD_Series.csv', stringsAsFactors = FALSE)
Descript <- Descript[, c('SeriesCode', 'Indicator.Name', 'Source', 'Periodicity')]

# List of indicators included 
Indicators <- c('GFDD.AM.03', 'GFDD.DI.01', 'GFDD.DI.02', 'GFDD.DI.03', 'GFDD.DI.04', 'GFDD.DI.05', 'GFDD.DI.06',
                'GFDD.DI.07', 'GFDD.DI.08', 'GFDD.DI.11', 'GFDD.DI.12', 'GFDD.DI.13', 'GFDD.DI.14', 'GFDD.DM.03',
                'GFDD.DM.04', 'GFDD.DM.05', 'GFDD.DM.06', 'GFDD.DM.07', 'GFDD.DM.08', 'GFDD.DM.09', 'GFDD.DM.10',
                'GFDD.EI.02', 'GFDD.EI.08', 'GFDD.OI.02', 'GFDD.OI.07', 'GFDD.OI.08', 'GFDD.OI.09',
                'GFDD.OI.10', 'GFDD.OI.11', 'GFDD.OI.12', 'GFDD.OI.13', 'GFDD.SI.02', 'GFDD.SI.03',
                'GFDD.SI.04', 'GFDD.SI.05', 'GFDD.SI.07')

Descript <- subset(Descript, SeriesCode %in% Indicators)

write.csv(Descript, file = 'IndicatorDescription.csv', row.names = FALSE)

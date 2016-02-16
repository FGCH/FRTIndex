####################################
# Plot results from FRT_Stan version 2
# Christopher Gandrud
# MIT License
####################################

library(rio)
library(dplyr)
library(ggplot2)

setwd('/git_repositories/FRTIndex/')

frt <- import('IndexData/FRTIndex_v2.csv')

frt_13 <- frt %>% filter(year == 2013)

frt_13 <- frt_13 %>% arrange(median)


ggplot(frt_13, aes(median, y = reorder(country, median))) +
    geom_point() +
    geom_segment(aes(x = lower_95, xend = upper_95,
                     yend = reorder(country, median)), size = 0.5,
                 alpha = 0.5) +
    geom_segment(aes(x = lower_90, xend = upper_90,
                     yend = reorder(country, median)), size = 1.5,
                 alpha = 0.5) +
    ylab('') + xlab('\nFRT') + ggtitle('2013\n') +
    theme_bw()

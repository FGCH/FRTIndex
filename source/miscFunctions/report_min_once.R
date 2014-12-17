#' Subset a data set so that it includes only countries that report at least once
#' 
#' @param data the data frame to subset
#' @param logical whether to return the names of the dropped countries or a 
#' data frame of the subsetted data. 

report_min_once <- function(data, drop_names = FALSE){
    library(dplyr)
    binary_vars <- names(data)[grep('^Rep_', names(data))]
    data$sums <- rowSums(data[, binary_vars])
                    report_zero <- group_by(data, country) %>%
                    summarize(added = sum(sums)) %>%
                    subset(., added == 0) %>%
                    as.data.frame()                    
    
    if (!isTRUE(drop_names)){
        # Subset
        data <- subset(data, !(country %in% report_zero[, 1]))
        message('None reported for:\n')
        message(paste(report_zero[, 1], '\n'))
        return(data)
    }
    else if (isTRUE(drop_names)){
        return(report_zero[, 1])
    }
    
}

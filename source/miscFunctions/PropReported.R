#' Find the proportion of indicators reported

PropReported <- function(data){
    vars <- grep('Rep_.*', names(data), value = TRUE)
    data$sum <- rowSums(data[, vars])
    data$FRT_PropReport <- data$sum/length(vars)
    Out <- data[, c('country', 'iso2c', 'year', 'FRT_PropReport')]
    return(Out)
}
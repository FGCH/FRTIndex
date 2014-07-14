#' A function to create catterpillar plots from stanfit objects from rstan
#'
#' @param obj a \code{stanfit} object
#' @param params character string, vector, or regular expression of paramater
#' labels that you would like to plot as declared in \code{model_code} from the
#' \code{\link{stan}} call.
#' @param params_labels vector of parameter labels. Important: they must be in
#' the same order as in the \code{stanfit} object when \code{as.data.frame(obj)}
#' is called.
#' @param order_medians logical. Whether or not to order the points by their
#' medians.
#' @param horizontal logical. Whether or not you would like the lines to be
#' horizontal
#'
#' @details Points plot the simulations' medians, thin lines represent the 95%
#' credibility intervals, and thick lines represent the 90% credibility
#' intervals.

stan_catterpillar <- function(obj, params, params_labels = NULL,
                              order_medians = TRUE, horizontal = TRUE){
    # Load packages
    library(rstan)
    library(reshape2)
    library(dplyr)
    library(ggplot2)

    # Extract all simulations
    sims <- as.data.frame(obj)

    # Extract only desired parameters
    names <- names(sims)
    sims_subset <- sims[, names %in% grep(pattern = params, x = names,
                                          value = TRUE)]

    if (ncol(sims_subset) == 0) {
        stop("No parameters selected. \n", call. = FALSE)
    }
    # Melt for plotting
    sims_subset_molten <- suppressMessages(melt(sims_subset))

    # Add labels
    if (!is.null(params_labels)) {
        message("\nEnsure that your parameter labels are in the same order as the parameters.\n")
        if (length(params_labels) !=
                length(unique(sims_subset_molten$variable))) {
            stop("params_labels must equal the number of plotted parameters.",
                call. = FALSE)
        }
        sims_subset_molten$variable <- factor(sims_subset_molten$variable,
                                              labels = params_labels)
    }

    # Find median and CI
    sims_subset_molten <- group_by(sims_subset_molten, variable)
    medians <- summarise(sims_subset_molten, median(value))
    lower95 <- summarise(sims_subset_molten, quantile(value, 0.025))
    lower90 <- summarise(sims_subset_molten, quantile(value, 0.05))
    upper90 <- summarise(sims_subset_molten, quantile(value, 0.95))
    upper95 <- summarise(sims_subset_molten, quantile(value, 0.975))
    comb <- suppressMessages(inner_join(lower95, lower90))
    comb <- suppressMessages(inner_join(comb, medians))
    comb <- suppressMessages(inner_join(comb, upper90))
    comb <- suppressMessages(inner_join(comb, upper95))
    names(comb) <- c('params', 'lower95', 'lower90', 'medians', 'upper90',
                     'upper95')

    # Plot
    if (isTRUE(order_medians)){
        pp <- ggplot(comb, aes(x = medians, y = reorder(params, medians),
                            xmin = lower95,
                            xmax = upper95)) +
            geom_point(size = 3) +
            geom_segment(aes(x = lower95, xend = upper95,
                            yend = reorder(params, medians)), size = 0.5) +
            geom_segment(aes(x = lower90, xend = upper90,
                            yend = reorder(params, medians)), size = 1.5) +
            xlab('') + ylab('') +
            theme_bw()
    }
    else {
        pp <- ggplot(comb, aes(x = medians, y = params,
                               xmin = lower95,
                               xmax = upper95)) +
            geom_point(size = 3) +
            geom_segment(aes(x = lower95, xend = upper95, yend = params),
                            size = 0.5) +
            geom_segment(aes(x = lower90, xend = upper90,
                            yend = params), size = 1.5) +
            xlab('') + ylab('') +
            theme_bw()
    }

    if (!isTRUE(horizontal)) pp <- pp + coord_flip()

    return(pp)
}

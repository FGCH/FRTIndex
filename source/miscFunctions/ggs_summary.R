#' Extract parameter point estimates
#' 
#' @param D Data frame with the simulations or list of data frame with simulations. If a list of data frames with simulations is passed, the names of the models are the names of the objects in the list.
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc).
#' @param thick_ci Vector of length 2 with the quantiles of the thick band for the credible interval.
#' @param thin_ci Vector of length 2 with the quantiles of the thin band for the credible interval.
#' @param param_label_from Vector of strings (can be regular expressions) for the original paramater labels that you would like to replace in the plot lables using \code{param_label_to}. 
#' @param param_label_to Vector of strings for paramater labels you would like to use. Must be both the same length as \code{param_label_from} and in the same order.
#'
#' @return a data frame summarising the point estimates of the postierior distribution.
#' 
#' @source Based on source from \code{ggs_caterpillar} from the \link{ggmcmc} package. 
#' 
#' @importFrom ggmcmc get_family
#' @importFrom dplyr group_by summarize
#' @importFrom reshape2 dcast
#' @export
 
ggs_summary <- function(D, family = NA, X = NA, thick_ci = c(0.05, 0.95), 
                        thin_ci = c(0.025, 0.975), param_label_from = NULL, 
                        param_label_to = NULL){
    # Remove if part of package
    require(plyr)
    require(reshape2)
    
    # Manage subsetting a family of parameters
    if (!is.na(family)) {
        D <- get_family(D, family=family)
    }
    
    # Passing the "probs" argument to quantile inside ddply inside a function
    # turns out to be really hard.
    # Apparently there is a bug in ddply that even Hadley does not know how to
    # solve
    # http://stackoverflow.com/questions/6955128/object-not-found-error-with-ddply-inside-a-function
    # One of the solutions, not elegant, is to assign qs globally (as well as
    # locally  for further commands in this function
    qs  <- qs <<- c(thin.low = thin_ci[1], thick.low = thick_ci[1], 
                    median = 0.5, thick.high = thick_ci[2], 
                    thin.high = thin_ci[2])
    
    # Multiple models or a single model
    #
    if (!is.data.frame(D)) { # D is a list, and so multiple models are passed
        multi <- TRUE # used later in plot call
        for (i in 1:length(D)) { # iterate over list elements
            dc <p
            dc <- ddply(D[[i]], .(Parameter), summarize,
                        q = quantile(value, probs=qs), qs=qs)
            dc$qs <- factor(dc$qs, labels=names(qs))
            dcm <- dcast(dc, Parameter ~ qs, value.var="q")
            D[[i]] <- dcm # replace list element with transformed list element
        }
        #
        # Get model names, by default the description attribute of the ggs object
        model.names <- lapply(D, function(x) return(attributes(x)$description))
        # But prevalence is for the names of the named list, not for labels or for the description
        if (length(model_labels)==length(D)) model.names <- model_labels       # get model names from labels
        if (length(names(D)!=0)) model.names <- names(D)           # get model names from named list
        # Final data frame to use for plotting
        dcm <- do.call(
            rbind, 
            lapply(1:length(D), 
                   function(i) if (length(D[[i]]) > 1) cbind(D[[i]], Model=model.names[i])))
        
    } else if (is.data.frame(D)) { # D is a data frame, and so a single model is passed
        multi <-  FALSE
        dc <- ddply(D, .(Parameter), summarize, 
                    q=quantile(value, probs=qs), qs=qs,
                    .parallel=attributes(D)$parallel)
        dc$qs <- factor(dc$qs, labels=names(qs))
        dcm <- dcast(dc, Parameter ~ qs, value.var="q")
    }
    
    # Relabel plotted parameters
    if (!is.null(param_label_from) & !is.null(param_label_to)){
        if (length(param_label_from) != length(param_label_to)){
            stop('param_label_from must be the same length as param_label_to', 
                 .call = FALSE)
        }
        for (i in 1:length(param_label_from)){
            dcm[, 'Parameter'] <- gsub(pattern = param_label_from[i], 
                                       replacement = param_label_to[i], 
                                       dcm[, 'Parameter'])  
        }
    }
    return(dcm)
}
 
#' Subset a ggs object to get only the parameters with a given regular expression.
#'
#' Internal function used by the graphical functions to get only some of the parameters that follow a given regular expression.
#'
#' @param D Data frame with the data arranged and ready to be used by the rest of the ggmcmc functions. The dataframe has four columns, namely: Iteration, Parameter, value and Chain, and seven attributes: nChains, nParameters, nIterations, nBurnin, nThin, description and parallel.
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc).
#' @return D Data frame that is a subset of the given D dataset.
#' @source from the \code{\link{ggmcmc}} package. 
 
get_family <- function(D, family=NA) {
    if (!is.character(family) | length(family)!=1) {
        stop("family must be a character vector with a single element")
    }
    # Select only the family paramaters
    family.id.parameters <- grep(family, D$Parameter)
    D.sub <- D[family.id.parameters,]
    D.sub$Parameter <- factor(as.character(D.sub$Parameter))
    # Copy the same attributes to the new object, except the number of
    # parameters
    # Probably there's a cleaner way to do it
    attr(D.sub, "nChains") <- attributes(D)$nChains
    attr(D.sub, "nParameters") <- length(unique(D.sub$Parameter))
    attr(D.sub, "nIterations") <- attributes(D)$nIterations
    attr(D.sub, "nBurnin") <- attributes(D)$nBurnin
    attr(D.sub, "nThin") <- attributes(D)$nThin
    attr(D.sub, "description") <- attributes(D)$description
    attr(D.sub, "parallel") <- attributes(D)$parallel
    return(D=D.sub)
}
#' Wrapper for using Zelig to simulate predicted values from two-way interactions
#'

simZelig <- function(obj, scen = NULL, secondVar = NULL, num = 1000, sig = 0.95){
    require(DataCombine)
    require(simPH)

    if (is.null(scen)) stop('Missing scen argument', call. = FALSE)

    if (class(scen) != 'data.frame') stop('scen needs to be a data frame',
                                          call. = FALSE)

    # Make sure sig is between 0 and 1.
    if (sig <= 0 | sig > 1){
        stop("sig must be greater than 0 and not greater than 1.")
    }

    # Create lower and upper percentile bounds of the confidence interval
    Bottom <- Bottom <<- (1 - sig)/2
    Top <- Bottom <<- 1 - Bottom

    # Create data frame to fill in with simulation summaries
    SimOut <- data.frame()

    # Run simulations
    for (i in 1:nrow(scen)){
        tempScen <- data.frame(scen[i, ])
        SimTemp <- simZeligInner(obj = obj, scen = tempScen, num = num)
        SimOut <- rbind(SimOut, SimTemp)
    }

    if (!is.null(secondVar)){
        # Thin out of sig--this is clearly a terrible hack
        SimOut <- simPH:::IntervalConstrict(Simb = SimOut, SubVar = secondVar,
                            spin = FALSE, ci = sig,
                            qi = 'Marginal Effect')
    }
    else message('All simulations retained.')

    return(SimOut)
}

#' Internal function used to simulate each scenario for simZelig
#'

simZeligInner <- function(obj, scen, num){
    library(Zelig)
    require(DataCombine)

    # Add in dependent variable for setx
    DV <- as.character(obj$formula[2])
    DVmean <- data.frame(mean(obj$data[, DV], na.rm = TRUE))
    names(DVmean) <- DV
    AllVarsScen <- cbind(DVmean, scen)

    # Run simulations
    SetVales <- setx(obj = obj, data = AllVarsScen)
    SimValues <- sim(obj = obj, x = SetVales, num = num)

    # Create summary data frame
    PV <- simulation.matrix(SimValues, "Predicted Values: Y|X")

    # Combine
    suppressWarnings(SimSum <- data.frame(scen, PV))
    names(SimSum) <- c(names(scen), 'QI')
    SimSum$ID <- 1:num

    return(SimSum)
}

#' Smooth values for one simulation

SmoothOneSim <- function(x, y, df = 20){
    TempXY <- cbind(x, y)
    TempOut <- smooth.spline(TempXY, df = df)
    Out <- fitted(TempOut)
    return(Out)
}

#' Smooth values for all simulations

SmoothSimulations <- function(SimIn, xaxis, group){
    # CRAN nonsense
    SimID <- Xj <- QI <- NULL

    names(SimIn)[names(SimIn) == xaxis] <- 'Xj'
    names(SimIn)[names(SimIn) == group] <- 'group'

    # Spline smooth
    Sims <- dplyr::group_by(SimIn, ID, group)
    SimsFitted <- dplyr::mutate(Sims, QI = SmoothOneSim(Xj, QI))
    names(SimsFitted)[names(SimsFitted) == 'Xj'] <- xaxis
    names(SimsFitted)[names(SimsFitted) == 'group'] <- group
    SimsFitted
}
# Simulate median effects from ECM
# Christopher Gandrud
# MIT License

# Load required packages
library(rio)
library(dplyr)
library(MASS)
library(ecmSim) # if not installed use first: devtools::install_github('christophergandrud/ecmSim')
library(ggplot2)
library(gridExtra)
library(grid)
theme_set(theme_bw())


setwd('/git_repositories/FRTIndex/paper/')

# Load parameter estimates and variance-covariance matrix estimated with Stata ----
# Spreads
spreads_coef <- import('analysis/sim_plots/estimates/spreads_coef.txt') %>%
    dplyr::select(-V1, -V27)
spreads_varcov <- import('analysis/sim_plots/estimates/spreads_varcovf.txt') %>%
    dplyr::select(-V1, -V27) %>% rename(intercept_ = `_cons`)

# Volatility
volatility_coef <- import('analysis/sim_plots/estimates/volatility_coef.txt') %>%
    dplyr::select(-V1, -V27)
volatility_varcov <- import('analysis/sim_plots/estimates/volatility_varcovf.txt') %>%
    dplyr::select(-V1, -V27) %>% rename(intercept_ = `_cons`)

# Clean for mvrnorm
spreads_coef <- as.numeric(spreads_coef[1, ])
spreads_varcov <- as.matrix(spreads_varcov[, 1:ncol(spreads_varcov)])
volatility_coef <- as.numeric(volatility_coef[1, ])
volatility_varcov <- as.matrix(volatility_varcov[, 1:ncol(volatility_varcov)])

# Create scenarios
main <- import('analysis/frt10_16_v1.dta')

# Subset to OECD, not Japan
main <- subset(main, oecd_member = 1)
main <- subset(main, !(country %in% c("Russian Federation", "South Africa",
                                      "Japan")))


# ECM simulation Functions-----------
common_shock_FUN <- function(x, only_shocked = TRUE, iv_s = 0, iv2_s = 0,
                             mu, Sigma) {
    sims <- ecm_builder(mu = spreads_coef,
                        Sigma = Sigma,
                        baseline_df = x,
                        lag_iv = 'l_frt_2015', d_iv = 'd_frt_2015',
                        iv_shock = iv_s,
                        lag_iv_2 = 'l_pubdebtgdp_gen', d_iv_2 = 'd_pubdebtgdp_gen',
                        iv_2_shock = iv2_s,
                        shock_duration = 5,
                        lag_iv_lag_iv2_interaction = 'l_frt2015xl_pub_gen',
                        d_iv_d_iv2_interaction = 'd_frt_2015xd_pubdebtgdp_gen',
                        lag_iv_d_iv2_interaction = 'l_frt2015xd_pubdebtgdp_gen',
                        d_iv_lag_iv2_interaction = 'd_frt_2015xl_pub_gen',
                        t_extent = 15, slim = FALSE, ci = 0.9, nsim = 5000,
                        qi_d_dv = FALSE
    )
    if (only_shocked) sims <- subset(sims, is_shocked == TRUE)
    return(sims)
}

sims_plotter <- function(x, main) {
    out_plot <- ggplot(x, aes(time__, dv_)) +
        geom_line(aes(group = sim_id__), alpha = 0.1) +
        scale_y_continuous(limits = c(0, 10)) +
        ggtitle(main) +
        theme_bw()
}

quantile_num <- function(x, prob){
    quantile(x, probs = prob, na.rm = TRUE) %>% as.numeric
}

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1,
                                       position = c("bottom", "right")) {

    plots <- list(...)
    position <- match.arg(position)
    g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x) x + theme(legend.position="none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)

    combined <- switch(position,
                       "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                              legend,
                                              ncol = 1,
                                              heights = unit.c(unit(1, "npc") - lheight, lheight)),
                       "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                             legend,
                                             ncol = 2,
                                             widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
    grid.newpage()
    grid.draw(combined)
}


medians_finder <- function(mu, Sigma, debt_level, iv_s = 0, dv_) {
    colnames(Sigma)[1] <- 'dv_'
    vlow <- data.frame(
        dv_ = median(main[, dv_], na.rm = TRUE),
        l_frt_2015 =  quantile_num(main$l_frt_2015, prob = 0.1),
        l_pubdebtgdp_gen = debt_level
    )

    low <- data.frame(
        dv_ = median(main[, dv_], na.rm = TRUE),
        l_frt_2015 =  quantile_num(main$l_frt_2015, prob = 0.25),
        l_pubdebtgdp_gen = debt_level
    )

    median_frt <- data.frame(
        dv_ = median(main[, dv_], na.rm = TRUE),
        l_frt_2015 =  median(main$l_frt_2015, na.rm = TRUE),
        l_pubdebtgdp_gen = debt_level
    )

    high <- data.frame(
        dv_ = median(main[, dv_], na.rm = TRUE),
        l_frt_2015 =  quantile_num(main$l_frt_2015, prob = 0.75),
        l_pubdebtgdp_gen = debt_level
    )

    vhigh <- data.frame(
        dv_ = median(main[, dv_], na.rm = TRUE),
        l_frt_2015 =  quantile_num(main$l_frt_2015, prob = 0.90),
        l_pubdebtgdp_gen = debt_level
    )

    sims_vlow <- common_shock_FUN(vlow, mu = mu, Sigma = Sigma, iv_s = iv_s)
    sims_vlow$`FRT (lag)` <- '10th Percentile'
    sims_low <- common_shock_FUN(low, mu = mu, Sigma = Sigma, iv_s = iv_s)
    sims_low$`FRT (lag)` <- '25th Percentile'
    sims_median <- common_shock_FUN(median_frt, mu = mu, Sigma = Sigma,
                                    iv_s = iv_s)
    sims_median$`FRT (lag)` <- 'Median'
    sims_high <- common_shock_FUN(high, mu = mu, Sigma = Sigma, iv_s = iv_s)
    sims_high$`FRT (lag)` <- '75th Percentile'
    sims_vhigh <- common_shock_FUN(vhigh, mu = mu, Sigma = Sigma, iv_s = iv_s)
    sims_vhigh$`FRT (lag)` <- '90th Percentile'

    sims_comb <- rbind(sims_vlow, sims_low, sims_median, sims_high,
                           sims_vhigh)

    sims_comb <- sims_comb %>% group_by(`FRT (lag)`, time__) %>%
        summarise(dv_median = median(dv_))

    sims_comb$`FRT (lag)` <- factor(sims_comb$`FRT (lag)`, levels = c(
        '10th Percentile', '25th Percentile',
        'Median', '75th Percentile',
        '90th Percentile'))

    return(sims_comb)
}

# Spreads -----------
frt_levels <- c('Debt/GDP: 30%', 'Debt/GDP: 100%')

# Spreads no FRT Shock
spreads_low_debt <- medians_finder(mu = spreads_coef, Sigma = spreads_varcov,
                                      debt_level = 30,
                                      dv_ = 'l_bond_spread_fred')
spreads_low_debt$main_scenario <- 'Debt/GDP: 30%'

spreads_high_debt <- medians_finder(mu = spreads_coef, Sigma = spreads_varcov,
                                     debt_level = 100,
                                     dv_ = 'l_bond_spread_fred')
spreads_high_debt$main_scenario <- 'Debt/GDP: 100%'

comb_spreads <- rbind(spreads_low_debt, spreads_high_debt)
comb_spreads$main_scenario <- factor(comb_spreads$main_scenario,
                                       levels = frt_levels)

p_spreads <- ggplot(comb_spreads, aes(time__, dv_median,
                                      linetype = `FRT (lag)`)) +
    facet_wrap(~main_scenario) +
    geom_line(alpha = 0.7) +
    ylab('10-yr Bond Spread\n') + xlab('') +
    ggtitle('No FRT Shock')

# Spreads FRT Shock
spreads_low_debt_s <- medians_finder(mu = spreads_coef, Sigma = spreads_varcov,
                                   debt_level = 30, iv_s = -1,
                                   dv_ = 'l_bond_spread_fred')
spreads_low_debt_s$main_scenario <- 'Debt/GDP: 30%'

spreads_high_debt_s <- medians_finder(mu = spreads_coef, Sigma = spreads_varcov,
                                    debt_level = 100, iv_s = -1,
                                    dv_ = 'l_bond_spread_fred')
spreads_high_debt_s$main_scenario <- 'Debt/GDP: 100%'

comb_spreads_s <- rbind(spreads_low_debt_s, spreads_high_debt_s)
comb_spreads_s$main_scenario <- factor(comb_spreads_s$main_scenario,
                                        levels = frt_levels)

p_spreads_s <- ggplot(comb_spreads_s, aes(time__, dv_median,
                                          linetype = `FRT (lag)`)) +
    facet_wrap(~main_scenario) +
    geom_line(alpha = 0.7) +
    ylab('10-yr Bond Spread\n') + xlab('') +
    ggtitle('-1 FRT Shock')

# Need to save by hand 10.6 width 5.77 height
grid_arrange_shared_legend(p_spreads, p_spreads_s, position = 'right')


# Volatility ------------------------
# Spreads no FRT Shock
volatility_low_debt <- medians_finder(mu = volatility_coef,
                                      Sigma = volatility_varcov,
                                      debt_level = 30,
                                   dv_ = 'l_bond_spread_fred')
volatility_low_debt$main_scenario <- 'Debt/GDP: 30%'

volatility_high_debt <- medians_finder(mu = volatility_coef,
                                       Sigma = volatility_varcov,
                                       debt_level = 100,
                                    dv_ = 'l_bond_spread_fred')
volatility_high_debt$main_scenario <- 'Debt/GDP: 100%'

comb_volatility <- rbind(volatility_low_debt, volatility_high_debt)
comb_volatility$main_scenario <- factor(comb_volatility$main_scenario,
                                        levels = frt_levels)

p_volatility <- ggplot(comb_volatility, aes(time__, dv_median,
                                            linetype = `FRT (lag)`)) +
    facet_wrap(~main_scenario) +
    geom_line(alpha = 0.7) +
    ylab('Bond Spread Volatility (coefficient of variation)\n') + xlab('') +
    ggtitle('No FRT Shock')

# Spreads FRT Shock
volatility_low_debt_s <- medians_finder(mu = volatility_coef,
                                        Sigma = volatility_varcov,
                                        debt_level = 30, iv_s = -1,
                                     dv_ = 'l_bond_spread_fred')
volatility_low_debt_s$main_scenario <- 'Debt/GDP: 30%'

volatility_high_debt_s <- medians_finder(mu = volatility_coef,
                                         Sigma = volatility_varcov,
                                         debt_level = 100, iv_s = -1,
                                      dv_ = 'l_bond_spread_fred')
volatility_high_debt_s$main_scenario <- 'Debt/GDP: 100%'

comb_volatility_s <- rbind(volatility_low_debt_s, volatility_high_debt_s)
comb_volatility_s$main_scenario <- factor(comb_volatility_s$main_scenario,
                                        levels = frt_levels)

p_volatility_s <- ggplot(comb_volatility_s, aes(time__, dv_median,
                                                linetype = `FRT (lag)`)) +
    facet_wrap(~main_scenario) +
    geom_line(alpha = 0.7) +
    ylab('Bond Spread Volatility (coefficient of variation)\n') + xlab('') +
    ggtitle('-1 FRT Shock')

# Need to save by hand 10.6 width 5.77 height
grid_arrange_shared_legend(p_volatility, p_volatility_s, position = 'right')



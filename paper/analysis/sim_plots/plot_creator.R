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
theme_set(theme_bw())


setwd('/git_repositories/FRTIndex/paper/analysis/sim_plots/')

# Load parameter estimates and variance-covariance matrix estimated with Stata ----
# Spreads
spreads_coef <- import('estimates/spreads_coef.txt') %>%
    dplyr::select(-V1, -V27)
spreads_varcov <- import('estimates/spreads_varcovf.txt') %>%
    dplyr::select(-V1, -V27) %>% rename(intercept_ = `_cons`)

# Volatility
volatility_coef <- import('estimates/volatility_coef.txt') %>%
    dplyr::select(-V1, -V27)
volatility_varcov <- import('estimates/volatility_varcovf.txt') %>%
    dplyr::select(-V1, -V27) %>% rename(intercept_ = `_cons`)

# Clean for mvrnorm
spreads_coef <- as.numeric(spreads_coef[1, ])
spreads_varcov <- as.matrix(spreads_varcov[, 1:ncol(spreads_varcov)])
volatility_coef <- as.numeric(volatility_coef[1, ])
volatility_varcov <- as.matrix(volatility_varcov[, 1:ncol(volatility_varcov)])

# Create scenarios
main <- import('/git_repositories/FRTIndex/paper/analysis/frt10_16_v1.dta')

# Subset to OECD, not Japan
main <- subset(main, oecd_member = 1)
main <- subset(main, !(country %in% c("Russian Federation", "South Africa",
                                      "Japan")))


# ECM simulations -----------
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
                        t_extent = 15, slim = FALSE, ci = 0.9, nsim = 1000,
                        qi_d_dv = FALSE
    )
    if (only_shocked) sims <- subset(sims, is_shocked == TRUE)
    return(sims)
}

sims_plotter <- function(x, main) {
    out_plot <- ggplot(x, aes(time__, l_bond_spread_fred)) +
        geom_line(aes(group = sim_id__), alpha = 0.1) +
        scale_y_continuous(limits = c(0, 10)) +
        ggtitle(main) +
        theme_bw()
}

quantile_num <- function(x, prob){
    quantile(x, probs = prob, na.rm = TRUE) %>% as.numeric
}


medians_plotter <- function(mu, Sigma, debt_level, change = TRUE) {
    vlow <- data.frame(
        l_bond_spread_fred = mean(main$l_bond_spread_fred, na.rm = TRUE),
        l_frt_2015 =  quantile_num(main$l_frt_2015, prob = 0.1),
        l_pubdebtgdp_gen = debt_level
    )

    low <- data.frame(
        l_bond_spread_fred = mean(main$l_bond_spread_fred, na.rm = TRUE),
        l_frt_2015 =  quantile_num(main$l_frt_2015, prob = 0.25),
        l_pubdebtgdp_gen = debt_level
    )

    median_frt <- data.frame(
        l_bond_spread_fred = mean(main$l_bond_spread_fred, na.rm = TRUE),
        l_frt_2015 =  median(main$l_frt_2015, na.rm = TRUE),
        l_pubdebtgdp_gen = debt_level
    )

    high <- data.frame(
        l_bond_spread_fred = mean(main$l_bond_spread_fred, na.rm = TRUE),
        l_frt_2015 =  quantile_num(main$l_frt_2015, prob = 0.75),
        l_pubdebtgdp_gen = debt_level
    )

    vhigh <- data.frame(
        l_bond_spread_fred = mean(main$l_bond_spread_fred, na.rm = TRUE),
        l_frt_2015 =  quantile_num(main$l_frt_2015, prob = 0.90),
        l_pubdebtgdp_gen = debt_level
    )

    sims_vlow <- common_shock_FUN(vlow, mu = mu, Sigma = Sigma)
    sims_vlow$scenario <- 'FRT (lag) 10% Percentile'
    sims_low <- common_shock_FUN(low, mu = mu, Sigma = Sigma)
    sims_low$scenario <- 'FRT (lag) 25% Percentile'
    sims_median <- common_shock_FUN(median_frt, mu = mu, Sigma = Sigma)
    sims_median$scenario <- 'FRT (lag) Median'
    sims_high <- common_shock_FUN(high, mu = mu, Sigma = Sigma)
    sims_high$scenario <- 'FRT (lag) 75% Percentile'
    sims_vhigh <- common_shock_FUN(vhigh, mu = mu, Sigma = Sigma)
    sims_vhigh$scenario <- 'FRT (lag) 90% Percentile'

    sims_debt_low <- rbind(sims_vlow, sims_low, sims_median, sims_high,
                           sims_vhigh)

    sims_low_sum <- sims_debt_low %>% group_by(scenario, time__) %>%
        summarise(l_spreads_mediam = median(l_bond_spread_fred))

    sims_low_sum$scenario <- factor(sims_low_sum$scenario, levels = c(
        'FRT (lag) 10% Percentile', 'FRT (lag) 25% Percentile',
        'FRT (lag) Median', 'FRT (lag) 75% Percentile',
        'FRT (lag) 90% Percentile'))

    p <- ggplot(sims_low_sum, aes(time__, l_spreads_mediam, linetype = scenario)) +
        geom_line() + xlab('\n Years')

    return(p)
}

p_spreads_low_debt <- medians_plotter(mu = spreads_coef, Sigma = spreads_varcov,
                                      debt_level = 30) +
                                    scale_y_continuous(limits = c(0, 6)) +
                                    ylab('10-yr Bond Spread\n') +
                                    ggtitle('Debt/GDP: 30%')

p_spreads_high_debt <- medians_plotter(mu = spreads_coef, Sigma = spreads_varcov,
                                      debt_level = 100) +
                                    scale_y_continuous(limits = c(0, 6)) +
                                    ylab('10-yr Bond Spread\n') +
                                    ggtitle('Debt/GDP: 100%')

grid.arrange(p_spreads_low_debt, p_spreads_high_debt, nrow = 1)









    # High Debt





baseline_frt_low_debt_high <- data.frame(
        l_bond_spread_fred = mean(main$l_bond_spread_fred, na.rm = TRUE),
        l_frt_2015 =  quantile_num(main$l_frt_2015, prob = 0.25),
        l_pubdebtgdp_gen = 100
    )

    baseline_frt_median_debt_high<- data.frame(
        l_bond_spread_fred = mean(main$l_bond_spread_fred, na.rm = TRUE),
        l_frt_2015 =  median(main$l_frt_2015, na.rm = TRUE),
        l_pubdebtgdp_gen = 100
    )

    baseline_frt_high_debt_high <- data.frame(
        l_bond_spread_fred = mean(main$l_bond_spread_fred, na.rm = TRUE),
        l_frt_2015 = quantile_num(main$l_frt_2015, prob = 0.75),
        l_pubdebtgdp_gen = 100
    )


    sims_low_debt_high <- common_shock_FUN(baseline_frt_low_debt_high, mu = spreads_coef,
                                           Sigma = spreads_varcov)
    sims_low_debt_high$scenario <- 'FRT (lag) 1st Quartile'
    sims_medium_debt_high <- common_shock_FUN(baseline_frt_median_debt_high, mu = spreads_coef,
                                              Sigma = spreads_varcov)
    sims_medium_debt_high$scenario <- 'FRT (lag) Median'
    sims_high_debt_high <- common_shock_FUN(baseline_frt_high_debt_high, mu = spreads_coef,
                                            Sigma = spreads_varcov)
    sims_high_debt_high$scenario <- 'FRT (lag) 3rd Quartile'

    sims_debt_high <- rbind(sims_high_debt_high, sims_medium_debt_high, sims_low_debt_high)

    sims_high_sum <- sims_debt_high %>% group_by(scenario, time__) %>%
        summarise(l_spreads_mediam = median(l_bond_spread_fred))

    sims_high_sum$scenario <- factor(sims_low_sum$scenario, levels = c(
        'FRT (lag) 3rd Quartile', 'FRT (lag) Median', 'FRT (lag) 1st Quartile'

    ))

    ggplot(sims_high_sum, aes(time__, l_spreads_mediam, linetype = scenario)) +
        geom_line()
}







first_diff <- sims_low$l_bond_spread_fred[sims_low$time__ == 2] -
                    sims_high$l_bond_spread_fred[sims_high$time__ == 2]
hist(first_diff)





low_debt_not_shocked <- subset(low_debt, is_shocked == FALSE)
ggplot(low_debt_not_shocked, aes(time__, l_bond_spread_fred)) +
    geom_line(aes(group = sim_id__), alpha = 0.2) +
    theme_bw()


ggplot(low_debt, aes(time__, qi_median, group = is_shocked, fill = is_shocked)) +
    geom_line(aes(colour = is_shocked)) +
    geom_ribbon(aes(ymin = qi_min, ymax = qi_max), alpha = 0.2) +
    ylab('10-year Bond Spread Chagne\n') + xlab('\nSimulation Time') +
    theme_bw()

# High debt ----------
baseline_scen_high <- data.frame(
    l_bond_spread_fred = median(main$l_bond_spread_fred, na.rm = TRUE),
    l_frt_2015 =  median(main$l_frt_2015, na.rm = TRUE),
    l_pubdebtgdp_gen = 100
)

high_debt <- ecm_builder(mu = spreads_coef,
                        Sigma = spreads_varcov,
                        baseline_df = baseline_scen_high,
                        lag_iv = 'l_frt_2015', d_iv = 'd_frt_2015', iv_shock = -1,
                        lag_iv_2 = 'l_pubdebtgdp_gen', d_iv_2 = 'd_pubdebtgdp_gen',
                        lag_iv_lag_iv2_interaction = 'l_frt2015xl_pub_gen',
                        d_iv_d_iv2_interaction = 'd_frt_2015xd_pubdebtgdp_gen',
                        lag_iv_d_iv2_interaction = 'l_frt2015xd_pubdebtgdp_gen',
                        d_iv_lag_iv2_interaction = 'd_frt_2015xl_pub_gen',
                        t_extent = 15, slim = TRUE, ci = 0.95, nsim = 5000
)


ggplot(high_debt, aes(time__, qi_median, group = is_shocked, fill = is_shocked)) +
    geom_line(aes(colour = is_shocked)) +
    geom_ribbon(aes(ymin = qi_min, ymax = qi_max), alpha = 0.2) +
    ylab('10-year Bond Spread Change\n') + xlab('\nSimulation Time') +
    theme_bw()


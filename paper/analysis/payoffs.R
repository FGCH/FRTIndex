# ----------------------------------------------------------------------------
# Transparency Game
# Christopher Gandrud
# MIT LICENSE
# ----------------------------------------------------------------------------

# Set working directory. Change as needed.
setwd('/git_repositories/FRTIndex/paper/')

# Load required package
library(dplyr)

# Payoff matrix for costless transparency
# Create scenarios -------------------------------------------------------------
payoff <- data.frame(
    x = rep(1:3, 2), # 1 = L, 2 = H, 3 = V
    gamma = c(rep(0, 3), rep(1, 3)) # 0 = L, 1 = H
)

# REVEAL
payoff$trans <- 0

# HIDE
payoff2 <- payoff
payoff2$trans <- 1

payoff <- rbind(payoff, payoff2)

# Find real debt level D
payoff$D_actual <- payoff$x + payoff$gamma
payoff$D_actual[payoff$D_actual > 3] <- 3

# Find investors belief about D under an assumption that they don't want to be 
# 'caught out'
payoff$D_I_belief <- NA
for (i in 1:nrow(payoff)) {
    if (payoff[i, 'trans'] == 0) payoff[i, 'D_I_belief'] <- payoff[i, 'D_actual']
    else payoff[i, 'D_I_belief'] <- payoff[i, 'x'] + 1
}

payoff$D_I_belief[payoff$D_I_belief > 3] <- 3

# Interest rate based on Investor's beliefs
payoff$r <- payoff$D_I_belief

# Government's utility
payoff$u_g <- 1 - payoff$r
payoff$u_i <- payoff$r - payoff$D_actual

# ---------------------------------------------------------------------------- #
# Preferred strategies for the government with costly transparency + 
# forced transparency

# Create scenarios ------------------
# Constant \Gamma at a low level
scen1 <- data.frame(
    x1 = rep('L', 12),
    x2 = c(rep('L', 4), rep('H', 4), rep('V', 4)),
    gamma1 = rep('L', 12),
    gamma2 = rep('L', 12),
    trans1 = rep(c('HIDE', 'HIDE', 'REVEAL', 'REVEAL'), 3),
    trans2 = rep(c('HIDE', 'REVEAL', 'HIDE', 'REVEAL'), 3)
)
scen1$r1 <- NA
scen1$r2 <- NA

scen1_h <- scen1
scen1_h$x1 <- 'H'

scen1_v <- scen1
scen1_v$x1 <- 'V'

scen1 <- rbind(scen1, scen1_h, scen1_v)

# Constant \Gamma at a high level
scen2 <- scen1
scen2$gamma1 <- 'H'
scen2$gamma2 <- 'H'

# Increasing \Gamma -----
scen3 <- scen1
scen3$gamma2 <- 'H'

# Decreasing \Gamma -----
scen4 <- scen1
scen4$gamma1 <- 'H'

scen <- rbind(scen1, scen2, scen3, scen4)

# Keep only valid scenarios, where change in debt is caused by change in 
## \Gamma or \Gamma high -----
for (i in 1:4) scen[, i] <- as.character(scen[, i])

# Keep only valid scenarios, where change in debt is one step
# Debt can only go up when \Gamma is high
scen <- scen %>% filter(x1 == x2 |
                    (x1 == 'L' & x2 == 'H' & gamma1 == 'H' & gamma2 == 'H') | 
                    (x1 == 'L' & x2 == 'H' & gamma1 == 'H' & gamma2 == 'L') | 
                    (x1 == 'H' & x2 == 'V' & gamma1 == 'H' & gamma2 == 'H') |
                    (x1 == 'H' & x2 == 'V' & gamma1 == 'H' & gamma2 == 'L') |
                    (x1 == 'V' & x2 == 'H') | 
                    (x1 == 'H' & x2 == 'L')
)

# Find interest rate choice ----------------------------------------------------
for (i in 1:nrow(scen)) {
    for (u in 1:2) {
        # Low explicit debt
        if (scen[i, paste0('x', u)] == 'L' & 
            scen[i, paste0('gamma', u)] == 'L' & 
            scen[i, paste0('trans', u)] == 'HIDE') {
            scen[i, paste0('r', u)] <- 2 
        }
        if (scen[i, paste0('x', u)] == 'L' & 
            scen[i, paste0('gamma', u)] == 'L' & 
            scen[i, paste0('trans', u)] == 'REVEAL') {
            scen[i, paste0('r', u)] <- 1 
        }
    
        if (scen[i, paste0('x', u)] == 'L' & 
            scen[i, paste0('gamma', u)] == 'H' & 
            scen[i, paste0('trans', u)] == 'HIDE') {
            scen[i, paste0('r', u)] <- 2 
        }
        if (scen[i, paste0('x', u)] == 'L' & 
            scen[i, paste0('gamma', u)] == 'H' &
            scen[i, paste0('trans', u)] == 'REVEAL') {
            scen[i, paste0('r', u)] <- 2 
        }
        # High explicit debt
        if (scen[i, paste0('x', u)] == 'H' & 
            scen[i, paste0('gamma', u)] == 'L' & 
            scen[i, paste0('trans', u)] == 'HIDE') {
            scen[i, paste0('r', u)] <- 3 
        }
        if (scen[i, paste0('x', u)] == 'H' & 
            scen[i, paste0('gamma', u)] == 'L' & 
            scen[i, paste0('trans', u)] == 'REVEAL') {
            scen[i, paste0('r', u)] <- 2 
        }
        
        if (scen[i, paste0('x', u)] == 'H' & 
            scen[i, paste0('gamma', u)] == 'H' & 
            scen[i, paste0('trans', u)] == 'HIDE') {
            scen[i, paste0('r', u)] <- 3 
        }
        if (scen[i, paste0('x', u)] == 'H' & 
            scen[i, paste0('gamma', u)] == 'H' & 
            scen[i, paste0('trans', u)] == 'REVEAL') {
            scen[i, paste0('r', u)] <- 3 
        }
        # Very high explicit debt
        if (scen[i, paste0('x', u)] == 'V') {
            scen[i, paste0('r', u)] <- 3 
        }
    }
}

# Find changes from stages 1 to 2 ---------
scen$delta_r = scen$r2 - scen$r1

# Gov. Utilities: Costless transparency change scenario (Stage 1) --------------
scen$u1 = 1 - scen$r1

# Gov. interest rate ceiling cost of 0.5 for x = V and gamma = H (Stage 1) -----
for (i in 1:nrow(scen)) {
    if (scen[i, 'x1'] == 'V' & scen[i, 'gamma1'] == 'H') {
        scen[i, 'u1'] <- scen[i, 'u1'] - 0.5
    }
}

# Gov. Utilities: Constant change cost = -1 (Stage 1)  ------
scen$u1_costly <- scen$u1
for (i in 1:nrow(scen)) scen[i, 'u1_costly'] <- scen[i, 'u1'] + -1

# Gov. Utilities: Constant change benefit = +1 (Stage 1) --- 
for (i in 1:nrow(scen)) scen[i, 'u1_benefit'] <- scen[i, 'u1'] + 1

# Gov. Utilities: Costless scenario (Stage 2) ----------------------------------
scen$u2 = 1 - scen$r2

# Gov. interest rate ceiling cost of 0.5 for x = V and gamma = H (Stage 2) -----
for (i in 1:nrow(scen)) {
    if (scen[i, 'x2'] == 'V' & scen[i, 'gamma2'] == 'H') {
        scen[i, 'u2'] <- scen[i, 'u2'] - 0.5
    }
}

# Gov. Utilities: Constant change cost = -1 (Stage 2)  -------------------------
scen$u2_costly <- scen$u2
for (i in 1:nrow(scen)) {
    if (scen[i, 'trans1'] != scen[i, 'trans2']) {
        scen[i, 'u2_costly'] <- scen[i, 'u2'] + -1
    }
}

# Gov. Utilities: Constant change benefit = +1 (Stage 2) -----------------------
scen$u2_benefit <- scen$u2
for (i in 1:nrow(scen)) {
    if (scen[i, 'trans1'] != scen[i, 'trans2']) {
        scen[i, 'u2_benefit'] <- scen[i, 'u2'] + 1
    }
}

# Find preferred strategies -----------------------
scen$scenario_id_2 <- with(scen, paste(x1, x2, gamma1, gamma2, trans1, 
                                       sep = '_'))

preferred <- function(x, t1, t2) {
    x <- x == max(x)
    # Minimal switching cost, i.e. status quo bias under conditions of indifference
    if (!missing(t2) & length(x[x == TRUE]) > 1) {
        x[t1 != t2] <- FALSE
    }
    x <- as.character(x)
    x[x == 'TRUE'] <- 'P'
    x[x == 'FALSE'] <- ''    
    return(x)
}

# Stage 2 preferred transparency
scen <- scen %>% group_by(scenario_id_2) %>%
    mutate(preferred2_costless = preferred(u2, trans1, trans2),
           preferred2_costly = preferred(u2_costly, trans1, trans2),
           preferred2_benefit = preferred(u2_benefit, trans1, trans2)
           )

# Forced REVEAL under very high interest rates ------------
scen$forced <- ''
scen$forced[scen$trans1 == 'HIDE' & scen$trans2 == 'REVEAL' & 
                scen$r2 == 3 & !(scen$x1 == 'V' & scen$x2 == 'H') &
                !(scen$x1 == 'H' & scen$x2 == 'L')
            ] <- 'F'


# Export as LaTeX table -----------------------
library(xtable)

scen$scenario_id_2 <- scen$scenario_id_2 %>% 
                        factor(levels = unique(scen$scenario_id_2)) %>% 
                        as.numeric

sub <- scen %>% select(scenario_id_2, x1, x2, gamma1, gamma2, trans1, trans2, 
                       r1, r2, u2, u2_costly, u2_benefit, 
                       preferred2_costless, preferred2_costly, preferred2_benefit,
                       forced)

names(sub) <- c('ID', '$X_{1}$', '$X_{2}$', '$\\Gamma_{1}$', '$\\Gamma_{2}$',
                '$Transp._{1}$', '$Transp._{2}$', '$r_{1}$', '$r_{2}$',
                '$U^{G}_{c = 0}$', '$U^{G}_{c = -1}$', 
                '$U^{G}_{c = 1}$', '$P^{G}_{c=0}$', '$P^{G}_{c = -1}$',
                '$P^{G}_{c = 1}$', 'Forced?'
                )
sub$ID <- as.character(sub$ID)

print(xtable(sub, digits = 1, caption = 'Extensive Form Government Payoffs for All Valid Scenarios in 2 Stage Transparency Games',
             label = 'truthtable'), 
      size = 'tiny', 
      sanitize.colnames.function = identity, include.rownames = FALSE,
      caption.placement = 'top', tabular.environment = 'longtable',
      file = 'tables/payoff.tex')


# Examine scenarios - Average Effect of transparency ------------

# Switched to forced opening as the preferred scenario, when applicable -----
forced <- scen %>% as.data.frame

plot_forced <- FALSE # Set to TRUE for a plot of interest rates with forced REVEAL 

if (plot_forced) {
    for (i in c('preferred2_costless', 'preferred2_costly', 'preferred2_benefit')) {
        message(i)
        forced[, i][forced$trans2 == 'HIDE' & forced$r2 == 3 & 
                        !(forced$x1 == 'V' & forced$x2 == 'H') & 
                        !(forced$x1 == 'H' & forced$x2 == 'L') 
                    ] <- ''
        forced[, i][forced$trans1 == 'HIDE' & forced$trans2 == 'REVEAL' 
                    & forced$r2 == 3 &
                        !(forced$x1 == 'V' & forced$x2 == 'H') & 
                        !(forced$x1 == 'H' & forced$x2 == 'L') 
                    ] <- 'P'
    }    
}


# Find interest rates in first stage
library(DataCombine)
no_dups <- FindDups(scen, c('x1', 'gamma1', 'trans1'), NotDups = T)

out_df1 <- data.frame()
for (i in c('L', 'H', 'V')) {
    message(i)
    temp <- no_dups %>% filter(x1 == i & trans1 == 'REVEAL')
    temp_r <- mean(temp$r1)
    
    temp <- no_dups %>% filter(x1 == i & trans1 == 'HIDE')
    temp_r <- c(temp_r, mean(temp$r1))
    
    temp_df <- data.frame(x = i, mean_r = temp_r)
    
    out_df1 <- rbind(out_df1, temp_df)
}

# Data frame with identifiers
ids1 <- data.frame(Transparency = rep(c('REVEAL', 'HIDE'), 3))

ids1 <- cbind(ids1, out_df1)
ids1$cost = 'Stage 1'


# Find interest rates in second stage
out_df2 <- data.frame()
for (i in c('L', 'H', 'V')) {
    l_R_0 <- forced %>% filter(x2 == i & trans2 == 'REVEAL' & preferred2_costless == 'P')
    temp_r <- mean(l_R_0$r2)
    
    l_H_0 <- forced %>% filter(x2 == i & trans2 == 'HIDE' & preferred2_costless == 'P')
    temp_r <- c(temp_r, mean(l_H_0$r2))
    
    
    l_R_1 <- forced %>% filter(x2 == i & trans2 == 'REVEAL' & preferred2_benefit == 'P')
    temp_r <- c(temp_r, mean(l_R_1$r2))
    
    l_H_1 <- forced %>% filter(x2 == i & trans2 == 'HIDE' & preferred2_benefit == 'P')
    temp_r <- c(temp_r, mean(l_H_1$r2))
    
    
    l_R_n1 <- forced %>% filter(x2 == i & trans2 == 'REVEAL' & preferred2_costly == 'P')
    temp_r <- c(temp_r, mean(l_R_n1$r2))
    
    l_H_n1 <- forced %>% filter(x2 == i & trans2 == 'HIDE' & preferred2_costly == 'P')
    temp_r <- c(temp_r, mean(l_H_n1$r2))
    
    temp_df <- data.frame(x = i, mean_r = temp_r)
    
    out_df2 <- rbind(out_df2, temp_df)
}
 
# Data frame with identifiers
ids2 <- data.frame(cost = c('c = 0', 'c = 0', 'c = 1', 'c = 1', 
                           'c = -1', 'c = -1'),
                  Transparency = rep(c('REVEAL', 'HIDE'), 3))

ids2 <- cbind(ids2, out_df2)
comb <- rbind(ids1, ids2)

comb$x <- factor(comb$x, levels = c('L', 'H', 'V'))
comb$cost <- factor(comb$cost, levels = c('Stage 1', 'c = -1', 
                                          'c = 0', 'c = 1'),
                    labels = c('Stage 1', 'Stage 2: c = -1', 
                                          'Stage 2: c = 0', 'Stage 2: c = 1'))
comb$Transparency <- factor(comb$Transparency, levels = c('HIDE', 'REVEAL'))

comb <- subset(comb, !is.na(mean_r))

library(ggplot2)
theme_set(theme_bw())

ggplot(comb, aes(x, mean_r, 
                group = Transparency, linetype = Transparency)) +
    facet_grid(.~cost) +
    geom_line() +
    xlab('\nDebt level (X)') + 
    ylab('Mean interest rate\n across all possible scenarios\n')

ggsave(filename = 'paper_plots/game_interest_levels.pdf', width = 10, 
       height = 6)


# Change in transparency on interest rates -----------------------
forced$x_change <- paste(forced$x1, forced$x2, sep = '_')
forced$trans_change <- paste(forced$trans1, forced$trans2, sep = '_')

out_df <- data.frame()
for (i in unique(forced$x_change)) {
    message(i)
    temp <- forced %>% filter(x_change == i & trans_change == 'HIDE_HIDE' & preferred2_costless == 'P')
    temp_r <- mean(temp$r2)

    temp <- forced %>% filter(x_change == i & trans_change == 'HIDE_REVEAL' & preferred2_costless == 'P')
    temp_r <-  c(temp_r, mean(temp$r2))

    temp <- forced %>% filter(x_change == i & trans_change == 'REVEAL_HIDE' & preferred2_costless == 'P')
    temp_r <- c(temp_r, mean(temp$r2))
    
    temp <- forced %>% filter(x_change == i & trans_change == 'REVEAL_REVEAL' & preferred2_costless == 'P')
    temp_r <- c(temp_r, mean(temp$r2))
    
    
    temp <- forced %>% filter(x_change == i & trans_change == 'HIDE_HIDE' & preferred2_benefit == 'P')
    temp_r <- c(temp_r, mean(temp$r2))
    
    temp <- forced %>% filter(x_change == i & trans_change == 'HIDE_REVEAL' & preferred2_benefit == 'P')
    temp_r <- c(temp_r, mean(temp$r2))
    
    temp <- forced %>% filter(x_change == i & trans_change == 'REVEAL_HIDE' & preferred2_benefit == 'P')
    temp_r <- c(temp_r, mean(temp$r2))
    
    temp <- forced %>% filter(x_change == i & trans_change == 'REVEAL_REVEAL' & preferred2_benefit == 'P')
    temp_r <- c(temp_r, mean(temp$r2))
    
    
    temp <- forced %>% filter(x_change == i & trans_change == 'HIDE_HIDE' & preferred2_costly == 'P')
    temp_r <- c(temp_r, mean(temp$r2))
    
    temp <- forced %>% filter(x_change == i & trans_change == 'HIDE_REVEAL' & preferred2_costly == 'P')
    temp_r <- c(temp_r, mean(temp$r2))
    
    temp <- forced %>% filter(x_change == i & trans_change == 'REVEAL_HIDE' & preferred2_costly == 'P')
    temp_r <- c(temp_r, mean(temp$r2))
    
    temp <- forced %>% filter(x_change == i & trans_change == 'REVEAL_REVEAL' & preferred2_costly == 'P')
    temp_r <- c(temp_r, mean(temp$r2))
    
    temp_df <- data.frame(x_change = i, mean_r = temp_r)
    
    out_df <- rbind(out_df, temp_df)
}

# Data frame with identifiers
costs_temp <- c(rep('c = 0', 4), rep('c = 1', 4), rep('c = -1', 4))
trans_change_temp <- rep(c('HIDE_HIDE', 'HIDE_REVEAL',
                           'REVEAL_HIDE', 'REVEAL_REVEAL'), 3)
ids <- data.frame(cost = rep(costs_temp, 7),
                  Trans_Change = rep(trans_change_temp, 7))

ids <- cbind(ids, out_df)
ids$mean_r[is.nan(ids$mean_r)] <- NA

# Keep only changing debt levels
ids$x_change <- factor(ids$x, levels = c('V_H', 'H_L', 
                                         'L_L','H_H','V_V', 
                                         'L_H',  'H_V'
                                          ),
                       labels = c('V -> H', 'H -> L',  
                                  'L -> L','H -> H','V -> V', 
                                  'L -> H',  'H -> V')
                       )
ids$Trans_Change <- factor(ids$Trans_Change, 
                           levels = c('HIDE_HIDE', 'HIDE_REVEAL', 
                                      'REVEAL_HIDE', 'REVEAL_REVEAL'),
                           labels = c('HIDE -> HIDE', 'HIDE -> REVEAL',
                                      'REVEAL -> HIDE', 'REVEAL -> REVEAL'))

# Only keep debt increases
ids <- subset(ids, x_change %in% c('L -> H',  'H -> V'))
#ids <- subset(ids, x_change %in% c('V -> H',  'H -> L'))

ids <- subset(ids, !is.na(mean_r))

ggplot(ids, aes(x_change, mean_r, 
                group = Trans_Change, linetype = Trans_Change)) +
    facet_grid(.~cost) +
    geom_line(position = position_dodge(width = 0.1)) +
    xlab('\nDebt level Change from Stage 1 to 2') + 
    ylab('Mean interest rates in Stage 2\n across all possible scenarios\n')

ggsave(filename = 'paper_plots/game_interest_levels_trans_change.pdf', 
       width = 10, height = 6)

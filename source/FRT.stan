////////////////////////////////
// FRT Stan Model
// Version 0.2--
// Christopher Gandrud
// 8 January 2015
// MIT License
////////////////////////////////

data {
    int<lower=1> C;                // number of countries
    int<lower=1> T;                // number of years
    int<lower=1> K;                // number of items
    int<lower=1> N;                // number of observations
    int<lower=1> cc[N];            // country for observation n
    int<lower=1> tt[N];            // time for observation n
    int<lower=1,upper=K> kk[N];    // item for observation n
    int<lower=0,upper=1> y[N];     // response for observation n
}

parameters {
    real delta;                // mean transparency
    vector[C] alpha1;          // initial alpha for t = 1 before recentering
    matrix[C,T] alpha;         // transparency for c,t - mean
    vector[K] beta;            // difficulty of item k
    vector[K] log_gamma;       // discrimination of k

    //// all scale parameters have an implicit half Cauchy prior ////
    real<lower=0> sigma_alpha[C];     // scale of abilities, per country
    real<lower=0> sigma_beta;         // scale of difficulties
    real<lower=0> sigma_gamma;        // scale of log discrimination
}

transformed parameters {
    //// re-centers transparency for t = 1 ////
    vector[C] recentered_alpha1;
    real mean_alpha1;
    real<lower=0> sd_alpha1;

    mean_alpha1 <- mean(alpha1);
    sd_alpha1 <- sd(alpha1);
    for (c in 1:C)
        recentered_alpha1[c] <- ( alpha1[c] - mean_alpha1 ) / sd_alpha1;
}

model {
    alpha1 ~ normal(0,1);   // informed constraints on the ability
                            // numerical issues with larger sd
    for (c in 1:C) {
        alpha[c,1] ~ normal(recentered_alpha1[c],0.001);
            // addresses current Stan limitation
        for (t in 2:T)
            alpha[c,t] ~ normal(alpha[c,t-1], sigma_alpha[c]);
    }

    beta ~ normal(0,sigma_beta);
    log_gamma ~ normal(0,sigma_gamma);
    delta ~ cauchy(0,0.05);

    sigma_alpha ~ cauchy(0,0.05);
    sigma_beta ~ cauchy(0,0.25);
    sigma_gamma ~ cauchy(0,0.25);

        for (n in 1:N)
            y[n] ~ bernoulli_logit(
            exp(log_gamma[kk[n]])
            * (alpha[cc[n],tt[n]] - beta[kk[n]] + delta) );
}

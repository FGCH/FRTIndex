# Read in Stan Output created by Julia/CMDStan

library(rstan)

setwd('~/git_repositories/FRTIndex/tmp/')

csv_from_julia <- sprintf('frt_model_samples_%s.csv', 1:4)

fit <- read_stan_csv(csv_from_julia)

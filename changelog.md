# Changelog

## Version 0.2.5 (To-do)

- Dropped Liquid liabilities to GDP (%) (GFDD.DI.05) as it is very similar to
Liquid liabilities in millions USD (2000 constant).

- Modified Stan code so that gamma is drawn from a Log-Normal distribution,
rather than exponentiating gamma in the logistic model. Gamma is also explicitly
constrained to be lower bounded by 0.

## Version 0.2.4.1

Same as v0.2.4, but highest probability density intervals reported rather than
central intervals.

## Version 0.2.4

- Run with 10,000 iterations.

## Version 0.2.3

- Run with 6,000 iterations.

## Version 0.2.2

- Use cauchy(0,0.05) priors for transparency scale parameter and mean
transparency. A smaller scale parameter (previously it was 0.25) is  a more
informative prior used to limit the range of the transparency parameter.

- Run with 3,000 iterations.

## Version 0.2.1

- Fill in data missing from the World Bank's version of the GFDD
with data from FRED's version of the GFDD.

- No longer include jurisdictions that never report items.
So, now there are 50 countries in the sample.

## Version 0.2

Updated beta model FRT Index using the No-U-Turn Sampler in Stan.

- 14 items

- 1990-2011.

- Model run with 4 chains with 2,000 iterations each (1000 or which are
burn-in).

## Version 0.1

Initial beta model FRT Index with 60 countries, 21 items, and 12 yeas
(1998-2011). The model was run with 2 chains, 6,000 iterations with a 5,000
iteration burn-in period using JAGS.

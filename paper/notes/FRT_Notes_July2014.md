---
output: pdf_document
---
Financial Transparency Index Estimation Notes July 2014
=======================================================

We used a hierarchical Bayesian Item Response Theory (IRT) approach To estimate the FRT Index. Specifically, we use a two parameter model with a logistic link function. It was influenced by previous work including Hollyer et. al. (2014), Bafumi et al. (2005), and Stan Development Team (2014a).

In our model we have 60 countries $j$ and 14 years $t$. We use binary information on whether or not each country reports each of 19 items $k$ in each year.[^1] So our two parameter IRT model for estimating the probability that each response $i$ will be made is given by:

$$\mathrm{Pr}(y_{i}=1) = \mathrm{logit^{-1}}(\gamma_{k(i)}(\alpha_{j,\:t(i)} - \beta_{k(i)} + \delta)) $$

The key parameters have the following meanings:

- $\gamma_{k}$: discrimination of item $k$,
- $\alpha_{j,\:t}$: transparency of country $k$ at year $t$,
- $\beta_{k}$: difficulty of item $k$,
- $\delta$: mean transparency.

[MORE from previous]

Including $\delta$ normalizes the parameters (see Bafumi et. al. 2005, 176).

The parameters are given the following priors:

$$\begin{array}{ll}
\gamma_{k} & \sim \mathrm{N}(0,\:\sigma_{\gamma}) \\
\alpha_{j,\:t = 1} & \sim \mathrm{N}(0,\: \sigma_{\alpha}) \\
\alpha_{j,\:t>1} & \sim \mathrm{N}(\alpha_{j,\:t-1},\: \sigma_{\alpha}) \\
\beta_{k} & \sim \mathrm{N}(0,\: \sigma_{\beta}) \\
\delta & \sim \mathrm{Cauchy}(0,\: 5) \\
\sigma_{\gamma} & \sim \mathrm{Cauchy}(0,\:5) \\
\sigma_{\alpha} & \sim \mathrm{N}(0,\:1) \\
\sigma_{\beta} & \sim \mathrm{Cauchy}(0,\:5) \\
\end{array}$$

The discrimination, difficulty parameters are given an semi-informative normal priors with mean 0. This is also true of the transparency parameter for the first year in the sample (1998). Using a mean of 0 helps identify the parameters (Bafumi et al. 2005, 173). Rather than assuming that transparency for a given country is independent across years, model each country's transparency as a random walk process. A country's previous year's estimated transparency is treated as the mean for the following year's normal prior distribution in all years after 1998. This effectively works to smooth the FRT for within countries across time (see Martin and Quinn 2002, 140).

As the mean transparency $\delta$ is not modeled hierarchically, we give it a weakly informative Cauchy prior with mean 0 and variance 5 (Stan Development Team 2014a, 35).

Furthermore, we constrained the discrimination parameter $\gamma_{k}$ to be positive. In standard IRT theory an item with a negative discrimination would indicate that less transparent countries are more likely to report the item. Since we consider the reporting of any item to indicate more transparency than not reporting an item, negative $\gamma$ would not be substantively meaningful.

We estimated the model using the No U-Turn Sampler Markov Chain Monte Carlo Algorithm (Hoffman and Gelman 2014) from Stan version 2.3.0 (Stan Development Team 2014b). Previous IRT models in political science have tended to use the Gibbs sampler, typically from BUGS [CITE] or JAGS [CITE]. The No U-Turn Sample is more computationally efficient with IRT data that is often fairly highly correlated (e.g. countries tend to not report groups of financial system indicators to the World Bank). Using the No U-Turn Sampler allowed us to reach convergence [GIVE STATS] much more quickly than a similar earlier model using the Gibbs sampler with JAGS.

[^1]: We use notation based on Bafumi et al. (2005)
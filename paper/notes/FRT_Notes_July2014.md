Financial Transparency Index Estimation Notes July 2014
=======================================================

We used a hierarchical Bayesian Item Response Theory (IRT) approach To estimate the FRT Index. Specifically, we use a two parameter model with a logistic link function. It was influenced by previous work including Hollyer et. al. (2014), Bafumi et al. (2005), and Stan Development Team (2014).

In our model we have 60 countries $j$ and 14 years $t$. We use binary information on whether or not each country reports each of 19 items $k$ in each year. So our two parameter IRT model is given by:

$$\mathrm{Pr}(y*{i}=1) = \mathrm{logit^{-1}}(\gamma*{k(i)}(\alpha*{jt(i)} - \beta*{k(i)} + \delta)) $$

The key parameters have the following meanings:

- $\gamma_{k}$: discrimination of item $k$,
- $\alpha_{jt}$: transparency of country $k$ at year $t$,
- $\beta_{k}$: difficulty of item $k$,
- $\delta$: mean transparency.

[MORE from previous]

Including $\delta$ normalizes the parameters (see Stan Development Team 33 and Bafumi et. al. 2005, 176).

The parameters are given the following priors:

$$\begin{array}{l}
\gamma_{k} \sim N(0,\sigma_{\gamma}) \\
\alpha_{jt} \sim N(0, \sigma_{\alpha}) \\
\beta_{k} \sim N(0, \sigma_{\beta}) \\
\delta \sim \mathrm{Cauchy}(0, 5) \\
\sigma_{\gama} \sim \mathrm{Cauchy}(0,5) \\
\sigma_{\alpha} \sim N(0,1) \\
\sigma_{\beta} \sim \mathrm{Cauchy}(0,5) \\

\end{array}$$

We constrained $\gamma_{k}$ to be positive. In standard IRT theory an item with a negative discrimination would indicate that less transparent countries are more likely to report the item. Since we consider the reporting of any item to indicate more transparency than not reporting an item, negative $\gamma$ would not be substantively meaningful.

Financial Regulatory Transparency Index
========

**Version:** 0.2

**Maintainer:** [Christopher Gandrud](http://christophergandrud.blogspot.de/p/biocontact.html)

Funding generously provided by the [Deutsche Forschungsgemeinschaft](http://www.dfg.de/en/).

**Work In Progress**

---

## Motivation

Why do countries release data on their financial systems to international
organizations, such as the IMF and World Bank? What are the consequences of
releasing this data for the stability of their financial systems?

We are developing a **Financial Regulatory Transparency Index**. The new
Index we make it possible to compare the willingness of governments to credibly
reveal the structure of their financial systems and their regulatory quality, so
that they can be scrutinized by market participants and citizens.

## The FRT Index

The current draft version of the Index is located in the *IndexData* directory.
The current version is in a CSV formatted file called: [FRTIndex_v0_2.csv](https://raw.githubusercontent.com/FGCH/FRTIndex/master/IndexData/FRTIndex_v0_2.csv).

It covers the **60 countries** classified by the World Bank as
'**High Income**' for the years **1990 through 2011**.

#### FRT Index Scores for 2011

![FRT_2011](FRT_2011.png)

The file `FRTIndex_v0_1.csv` contains the following variables:

| Variable Name | Short Description                              |
| ------------- | ---------------------------------------------- |
| country       | country name                                   |
| iso2c         | [ISO 2 letter country code](http://en.wikipedia.org/wiki/ISO_3166-1_alpha-2) |
| year          | year of the FRT score                          |
| lower_95      | lower bound of the 95% credibility interval    |
| lower_90      | lower bound of the 90% credibility interval    |
| median        | median of the FRT index posterior distribution |
| upper_90      | upper bound of the 90% credibility interval    |
| upper_95      | upper bound of the 95% credibility interval    |

## Download into R

To download the **working version** of the Index directly into
[R](http://www.r-project.org/) as a data frame use the
[repmis](http://cran.r-project.org/web/packages/repmis/index.html) package:

```{S}
URL <- 'https://raw.githubusercontent.com/FGCH/FRTIndex/master/IndexData/FRTIndex_v0_2.csv'

frt_index <- repmis::source_data(URL)
```

## Estimation Model

The FRT Index is created using a
[Bayesian Item Response Theory](http://en.wikipedia.org/wiki/Item_response_theory)
model of high income countries's reporting of [financial industry indicators](https://github.com/FGCH/FRTIndex/blob/master/source/IndicatorDescript/IncludedIndicators.csv)
to the World Bank's
[Global Financial Development Database](http://data.worldbank.org/data-catalog/global-financial-development).

A full write up of our model is in the works. The most recent (incomplete) draft
is available for download as a
[PDF](https://github.com/FGCH/FRTIndex/blob/master/paper/FRTIndexPaper.pdf?raw=true).

Our estimation model is based on:

> Hollyer, James R., B. Peter Rosendorff, and James Raymond Vreeland. 2014.
"Replication data for: Measuring Transparency".
[http://dx.doi.org/10.7910/DVN/24274](http://dx.doi.org/10.7910/DVN/24274)

We have built on this model using the
[No-U-Turn Sampler](http://arxiv.org/abs/1111.4246) implemented in
[Stan](http://mc-stan.org/). This provides computational efficiency improvements.

---

<a href="http://www.dfg.de/en/"><img src="http://fgch.github.io/amc-site/img/dfg.png"/></a> <a href="http://nadrosia.tumblr.com/post/53520500877/made-in-berlin-badge-update"><img alt="image" src="http://media.tumblr.com/023c285c14ef01953d3b67ffe789004d/tumblr_inline_mor1uu2OOZ1qz4rgp.png" height = "50"></a> <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/"><img alt="Creative Commons Licence" style="border-width:0" src="http://i.creativecommons.org/l/by-sa/4.0/88x31.png" height = "40" /></a>

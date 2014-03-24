Financial Regulatory Transparency Index
========

Financial Regulatory Transparency (FRT) Index as well as documentation and source code to create the Index

Version 0.1

---

> **This is a work in progress.**

## Index Data

The current draft version of the index is located in the *IndexData* directory. The current version is in a CSV formatted file called: `FRTIndex_v0_1.csv`

It covers the **60 countries** classified by the World Bank as 'High Income' and the years **1998 through 2011**.

The file `FRTIndex_v0_1.csv` contains the following variables:

| Variable Name | Short Description                              |
| ------------- | ---------------------------------------------- |
| country       | country name                                   |
| iso2c         | [ISO 2 letter country code](http://en.wikipedia.org/wiki/ISO_3166-1_alpha-2) |
| year          | year of the FRT score                          |
| lower_95        | lower bound of the 95% credibility interval    |
| lower_90        | lower bound of the 90% credibility interval    |
| median        | median of the FRT index posterior distribution |
| upper_90      | upper bound of the 90% credibility interval    |
| upper_95      | upper bound of the 95% credibility interval    |

#### FRT Index Scores for 2011

![FRT_2011](FRT_2011.png)

## Model

The estimation model is based on:

Hollyer, James R. ; Rosendorff, B. Peter; Vreeland, James Raymond, 2014, 
"Replication data for: Measuring Transparency", 
[http://dx.doi.org/10.7910/DVN/24274](http://dx.doi.org/10.7910/DVN/24274)

A full right up of our model is being written. The most recent draft is available for download as a [PDF](https://github.com/FGCH/FRTIndex/blob/master/paper/FRTIndexPaper.pdf).

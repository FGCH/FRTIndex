# Load stan_catterpillar function
SourceURL <- 'https://gist.githubusercontent.com/christophergandrud/9b6caf8fa6ed0cbb33c4/raw/211f98590903e96e87686b02e4436a207f49f2ad/stan_caterpillar.R'
devtools::source_url(SourceURL)

countries <- unique(BaseSub$country)

fit <- fit_NonIndp

# 1993

## 1998

## 2007

## 2011
stan_catterpillar(fit, params = 'alpha\\[.*,22\\]', params_labels = countries) +
    ylab('') + xlab('\nFRT Index (HPD)') + theme_bw()




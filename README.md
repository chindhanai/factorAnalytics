
# Google Summer of Code 2017: Advancing factorAnalytics
This repository is created as a part of Google Summer of Code (GSoC) 2017, to add new functionalities to the **factorAnalytics** package forked from R-forge.

Installation
------------

To get started, you can install the package from github using `devtools`.

``` r
library(devtools)
install_github("chindhanai/factorAnalyticsAddons")
library(factorAnalyticsAddons)
```

factorAnalyticsAddons Key Features
----------------------------------

The factorAnalyticsAddons package contains the same functionalities as the factorAnalytics package, i.e. fitting and analysis methods for the three main types of factor models used in conjunction with portfolio construction, optimization and risk management, namely fundamental factor models, time series factor models and statistical factor models. The sole purpose of advancing factorAnalytics is to add key improvements to the package - offering users not only the basic features and capabilities close to those of commercial portfolio optimization and risk management products, but also the advanced features and capabilities to perform more challenging analyses. The key deliverables are

> Active Manager Performance Assessment: Newly added function

> Robust Risk Budgeting: Newly added function

> The Fundamental Law of Active Management: Extending the functionalities of fitFfm



Detailed information on newly added functionalities and use of the functions can be found at **[ffm vignette](https://github.com/AvinashAcharya/factorAnalytics/blob/master/vignettes/ffmVignette-GSoC-2016.pdf)**

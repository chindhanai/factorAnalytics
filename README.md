
# Google Summer of Code 2017: Advancing factorAnalytics
This repository is created as a part of Google Summer of Code (GSoC) 2017, to add new functionalities to the `factorAnalytics` package forked from R-forge.

Installation
------------

To get started, you can install the package from github using `devtools`.

``` r
library(devtools)
install_github("chindhanai/factorAnalyticsAddons")
library(factorAnalyticsAddons)
```

factorAnalyticsAddons 
---------------------

The `factorAnalyticsAddons` package contains the same functionalities as the `factorAnalytics` package, i.e. fitting and analysis methods for the three main types of factor models used in conjunction with portfolio construction, optimization and risk management, namely fundamental factor models, time series factor models and statistical factor models. The sole purpose of advancing factorAnalytics is to add key improvements to the package - offering users not only the basic features and capabilities close to those of commercial portfolio optimization and risk management products, but also the advanced features and capabilities to perform more challenging analyses.

> Proposed Key Deliverables

1. Fundamental Factor Models - Hybrid Factor Model
2. Fundamental Factor Models - The Multi-Factor Model method (Ding, Z. and Martin, R. D. (2016))
3. Fundamental Factor Models - The Multi-dimensional outlier detection
4. Fundamental Factor Models - EWMA and GARCH models
5. Time Series Factor Models - Portfolio level returns and risk reports
6. Time Series Factor Models - Multi-dimensional outlier detection and visualization
7. Time Series Factor Models - Quadratic and Interaction model terms, Lagged risk factors and lagged returns models, and models with ARIMA errors
8. Time Series Factor Models - Implement the CUSUM procedure in active management
9. Fundamental Factor Models - Multi-currency risk modeling
10. Risk Models and Risk Budgeting - Risk parity portfolios for volatility risk
11. Risk Models and Risk Budgeting - Simple and robust risk budgeting with expected shortfall
12. Risk Models and Risk Budgeting - Russian Doll risk models

> Completed Tasks

Key deliverables No. 2, 4, 8, 11, which are deemed the most challenging among all key deliverables, have been completed. The implementation of key deliverables 1 and 3 are at their beginning. I plan to finish this by mid September, and the rest are to be completed by the end of December. 

Detailed information on new functionalities and use of the functions can be found in the vignette folder.

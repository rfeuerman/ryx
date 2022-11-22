
# ryx

![](ryx.jpg)

<!-- badges: start -->
<!-- badges: end -->

The goal of ryx is to easily create, summarize and plot correlations. 

## Installation

You can install the development version of ryx like so:

``` r
remotes::install_github("rfeuerman/ryx")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ryx)
library(MASS)
x <- ryx(Boston, y="medv")
plot(x)

```


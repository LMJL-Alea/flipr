
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Overview <a href='https://astamm.github.io/flipr/'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![check-standard](https://github.com/astamm/flipr/workflows/R-CMD-check/badge.svg)](https://github.com/astamm/flipr/actions)
[![test-coverage](https://github.com/astamm/flipr/workflows/test-coverage/badge.svg)](https://github.com/astamm/flipr/actions)
[![Codecov test
coverage](https://codecov.io/gh/astamm/flipr/branch/master/graph/badge.svg)](https://codecov.io/gh/astamm/flipr?branch=master)
[![pkgdown](https://github.com/astamm/flipr/workflows/pkgdown/badge.svg)](https://github.com/astamm/flipr/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/flipr)](https://CRAN.R-project.org/package=flipr)
<!-- badges: end -->

The goal of the **flipr** package is to provide a flexible framework for
making inference via permutation. The idea is to promote the permutation
framework as an incredibly well-suited tool for hypothesis testing on
complex data. You supply your data, as complex as it might be, in the
form of lists in which each entry stores one data point in a
representation that suits you and **flipr** takes care of the
permutation magic and provides you with the result of the permutation
test. Permutation tests are especially appealing because they are exact
no matter how small or big your sample sizes are. You can also use the
so-called *non-parametric combination* approach in this setting to
combine several statistics to better target the alternative hypothesis
you are testing against. Asymptotic consistency is also guaranteed under
mild conditions on the statistic you use. Currently, you can do
two-sample tests. We plan on adding very soon one-sample tests, ANOVA
and regression to the list as well.

## Installation

You can install the latest stable version of **flipr** on CRAN with:

``` r
install.packages("flipr")
```

Or you can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("astamm/flipr")
```

## Example

We hereby use the very simple t-test for comparing the means of two
univariate samples to show how easy it is to carry out a permutation
test with **flipr**.

Let us first generate a first sample of size 10 governed by a Gaussian
distribution of mean 0 and unit variance:

``` r
set.seed(1234)
x1 <- rnorm(n = 10, mean = 0, sd = 1)
```

Let us then generate a second sample of size 10 governed by a Gaussian
distribution of mean 3 and unit variance:

``` r
set.seed(1234)
x2 <- rnorm(n = 10, mean = 3, sd = 1)
```

We can implement the squared *t*-statistic as a function that plays well
with **flipr** as follows:

``` r
stat_t2 <- function(data, indices) {
  n <- length(data)
  n1 <- length(indices)
  n2 <- n - n1
  indices2 <- seq_len(n)[-indices]
  x1 <- unlist(data[indices])
  x2 <- unlist(data[indices2])
  stats::t.test(x = x1, y = x2, var.equal = TRUE)$statistic^2
}
```

Note that the square is needed as permutation tests look for large
values of the test statistic to find evidence against the null
hypothesis.

Now we can simply use the function `flipr::two_sample_test()` to get the
result of the test:

``` r
test_t2 <- flipr::two_sample_test(
  x = x1, 
  y = x2, 
  statistic = stat_t2, 
  B = 10000
)
test_t2$pvalue
#> [1] 0.0001945701
```

We can compare the resulting p-value with the one obtained using the
more classic parametric test:

``` r
test_student <- t.test(x = x1, y = x2, var.equal = TRUE)
test_student$p.value
#> [1] 2.584312e-06
```


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

The goal of the [**flipr**](https://astamm.github.io/flipr/) package is
to provide a flexible framework for making inference via permutation.
The idea is to promote the permutation framework as an incredibly
well-suited tool for inference on complex data. You supply your data, as
complex as it might be, in the form of lists in which each entry stores
one data point in a representation that suits you and
[**flipr**](https://astamm.github.io/flipr/) takes care of the
permutation magic and provides you with either point estimates or
confidence regions or p-value of hypothesis tests. Permutation tests are
especially appealing because they are exact no matter how small or big
your sample sizes are. You can also use the so-called *non-parametric
combination* approach in this setting to combine several statistics to
better target the alternative hypothesis you are testing against.
Asymptotic consistency is also guaranteed under mild conditions on the
statistic you use. The [**flipr**](https://astamm.github.io/flipr/)
package provides a flexible permutation framework for making inference
such as point estimation, confidence intervals or hypothesis testing, on
any kind of data, be it univariate, multivariate, or more complex such
as network-valued data, topological data, functional data or
density-valued data.

## Installation

You can install the latest stable version of
[**flipr**](https://astamm.github.io/flipr/) on CRAN with:

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

``` r
library(flipr)
```

We hereby use the very simple t-test for comparing the means of two
univariate samples to show how easy it is to carry out a permutation
test with [**flipr**](https://astamm.github.io/flipr/).

Let us first generate two samples of size 15 each governed by Gaussian
distributions with equal variance but different means:

``` r
set.seed(123)
n <- 15
x1 <- rnorm(n = n, mean = 0, sd = 1)
x2 <- rnorm(n = n, mean = 1, sd = 1)
```

Let *δ* = *μ*<sub>2</sub> − *μ*<sub>1</sub>. In order for the `x2`
sample to be exchangeable with the `x1` sample, one can subtract *δ* to
each observation of the `x2` sample. To do that, let us define the
following null specification function:

``` r
null_spec <- function(y, parameters) {
  purrr::map(y, ~ .x - parameters)
}
```

Next, we need to decide which test statistic(s) we are going to use for
performing the test. Here, we are only interested in one parameter,
namely the mean difference *δ*. Since the two samples share the same
variance we can use the *t*-statistic with a pooled estimated of the
common variance. We need to implement it for easy computation on a
permuted version of the data. Hence, the skeleton function to implement
test statistics compatible with **flipr** is:

``` r
my_stat_fun <- function(data, indices) {
  # data: A list of n1 + n2 element in which the first n1 elements are the
  # observations of the first sample and the last n2 elements contain the
  # observations of the second sample
  
  # indices: An integer vector of indices of observations that belong to the
  # first sample in the current permuted version of the data
}
```

We can therefore implement the *t*-statistic as a function that plays
well with [**flipr**](https://astamm.github.io/flipr/) as follows:

``` r
my_t_stat <- function(data, indices) {
  n <- length(data)
  n1 <- length(indices)
  n2 <- n - n1
  indices2 <- seq_len(n)[-indices]
  x1 <- unlist(data[indices])
  x2 <- unlist(data[indices2])
  stats::t.test(x = x1, y = x2, var.equal = TRUE)$statistic
}
```

We then declare a list of test statistics function:

``` r
stat_functions <- list(my_t_stat)
```

Finally we need to define a named list that tells **flipr** which test
statistics among the ones declared in the `stat_functions` list above
should be used for each inferred parameter. This is used to determine
bounds on each parameter for the plausibility function. This list, often
termed `stat_assignments`, should therefore have as many elements as
there are parameters to be inferred. Each element should be named after
the name of a parameter to be inferred and should list the indices
corresponding to the test statistics that should be used for that
parameter in `stat_functions`. In our example, it means that:

``` r
stat_assignments <- list(delta = 1)
```

Now we can instantiate a plausibility function as follows:

``` r
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x1, x2
)
#> ! Setting the seed for sampling permutations is mandatory for obtaining a continuous p-value function. Using `seed = 1234`.
```

Now assume we want to test the following hypotheses:

*H*<sub>0</sub> : *δ* = 0  v.s.  *H*<sub>1</sub> : *δ* ≠ 0.

We just have to evaluate the plausbility function in 0 to get the
corresponding p-value as follows:

``` r
pf$get_value(0)
#> [1] 0.1068931
```

We can compare the resulting p-value with the one obtained using the
more classic parametric test:

``` r
t.test(x = x1, y = x2, var.equal = TRUE)$p.value
#> [1] 0.1030946
```

The permutation p-value does not quite match the parametric one. This is
because of two reasons:

1.  The resolution of a permutation p-value is of the order of
    1/(*B* + 1), where *B* is the number of sampled permutations. By
    default, the plausibility function is instantiated with *B* = 1000:

``` r
pf$nperms
#> [1] 1000
```

2.  We randomly sample *B* permutations out of the
    $\\binom{n\_1+n\_2}{n\_1}$ possible permutations and therefore
    introduce extra variability in the p-value.

If we were to ask for more permutations, say *B* = 1, 000, 000, we would
be much closer to the parametric *p*-value:

``` r
pf$set_nperms(1000000)
pf$get_value(0)
#> [1] 0.1029993
```

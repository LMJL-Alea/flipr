
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Overview <a href='https://lmjl-alea.github.io/flipr/'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![check-standard](https://github.com/LMJL-Alea/flipr/workflows/R-CMD-check/badge.svg)](https://github.com/LMJL-Alea/flipr/actions)
[![test-coverage](https://github.com/LMJL-Alea/flipr/workflows/test-coverage/badge.svg)](https://github.com/LMJL-Alea/flipr/actions)
[![Codecov test
coverage](https://codecov.io/gh/LMJL-Alea/flipr/branch/master/graph/badge.svg)](https://codecov.io/gh/LMJL-Alea/flipr?branch=master)
[![pkgdown](https://github.com/LMJL-Alea/flipr/workflows/pkgdown/badge.svg)](https://github.com/LMJL-Alea/flipr/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/flipr)](https://CRAN.R-project.org/package=flipr)
<!-- badges: end -->

The goal of the [**flipr**](https://lmjl-alea.github.io/flipr/) package
is to provide a flexible framework for making inference via permutation.
The idea is to promote the permutation framework as an incredibly
well-suited tool for inference on complex data. You supply your data, as
complex as it might be, in the form of lists in which each entry stores
one data point in a representation that suits you and
[**flipr**](https://lmjl-alea.github.io/flipr/) takes care of the
permutation magic and provides you with either point estimates or
confidence regions or
![p](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;p "p")-value
of hypothesis tests. Permutation tests are especially appealing because
they are exact no matter how small or big your sample sizes are. You can
also use the so-called *non-parametric combination* approach in this
setting to combine several statistics to better target the alternative
hypothesis you are testing against. Asymptotic consistency is also
guaranteed under mild conditions on the statistic you use. The
[**flipr**](https://lmjl-alea.github.io/flipr/) package provides a
flexible permutation framework for making inference such as point
estimation, confidence intervals or hypothesis testing, on any kind of
data, be it univariate, multivariate, or more complex such as
network-valued data, topological data, functional data or density-valued
data.

## Installation

You can install the latest stable version of
[**flipr**](https://lmjl-alea.github.io/flipr/) on CRAN with:

``` r
install.packages("flipr")
```

Or you can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("LMJL-Alea/flipr")
```

## Example

``` r
library(flipr)
```

We hereby use the very simple t-test for comparing the means of two
univariate samples to show how easy it is to carry out a permutation
test with [**flipr**](https://lmjl-alea.github.io/flipr/).

### Data generation

Let us first generate two samples of size
![15](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;15 "15")
governed by Gaussian distributions with equal variance but different
means:

``` r
set.seed(123)
n <- 15
x <- rnorm(n = n, mean = 0, sd = 1)
y <- rnorm(n = n, mean = 1, sd = 1)
```

Given the data we simulated, the parameter of interest here is the
difference between the means of the distributions, say
![\delta = \mu_y - \mu_x](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cdelta%20%3D%20%5Cmu_y%20-%20%5Cmu_x "\delta = \mu_y - \mu_x").

### Make the two samples exchangeable under ![H_0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;H_0 "H_0")

In the context of null hypothesis testing, we consider the null
hypothesis
![H_0: \mu_y - \mu_x = \delta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;H_0%3A%20%5Cmu_y%20-%20%5Cmu_x%20%3D%20%5Cdelta "H_0: \mu_y - \mu_x = \delta").
We can use a permutation scheme to approach the
![p](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;p "p")-value
if the two samples are *exchangeable* under
![H_0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;H_0 "H_0").
This means that we need to transform for example the second sample to
*make* it exchangeable with the first sample under
![H_0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;H_0 "H_0").
In this simple example, this can be achieved as follows. Let
![X_1, \dots, X\_{n_x} \sim \mathcal{N}(\mu_x, 1)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X_1%2C%20%5Cdots%2C%20X_%7Bn_x%7D%20%5Csim%20%5Cmathcal%7BN%7D%28%5Cmu_x%2C%201%29 "X_1, \dots, X_{n_x} \sim \mathcal{N}(\mu_x, 1)")
and
![Y_1, \dots, Y\_{n_y} \sim \mathcal{N}(\mu_y, 1)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y_1%2C%20%5Cdots%2C%20Y_%7Bn_y%7D%20%5Csim%20%5Cmathcal%7BN%7D%28%5Cmu_y%2C%201%29 "Y_1, \dots, Y_{n_y} \sim \mathcal{N}(\mu_y, 1)").
We can then transform the second sample as
![Y_i \longleftarrow Y_i - \delta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y_i%20%5Clongleftarrow%20Y_i%20-%20%5Cdelta "Y_i \longleftarrow Y_i - \delta").

We can define a proper function to do this, termed the *null
specification* function, which takes two input arguments:

-   `y` which is a list storing the data points in the second sample;
-   `parameters` which is a numeric vector of values for the parameters
    under investigation (here only
    ![\delta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cdelta "\delta")
    and thus `parameters` is of length
    ![1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;1 "1")
    with `parameters[1] = delta`).

In our simple example, it boils down to:

``` r
null_spec <- function(y, parameters) {
  purrr::map(y, ~ .x - parameters[1])
}
```

### Choose suitable test statistics

Next, we need to decide which test statistic(s) we are going to use for
performing the test. Here, we are only interested in one parameter,
namely the mean difference
![\delta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cdelta "\delta").
Since the two samples share the same variance, we can use for example
the
![t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;t "t")-statistic
with a pooled estimate of the common variance.

This statistic can be easily computed using
`stats::t.test(x, y, var.equal = TRUE)$statistic`. However, we want to
extend its evaluation to any permuted version of the data. Test
statistic functions compatible with
[**flipr**](https://lmjl-alea.github.io/flipr/) should have at least two
mandatory input arguments:

-   `data` which is either a concatenated list of size
    ![n_x + n_y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;n_x%20%2B%20n_y "n_x + n_y")
    regrouping the data points of both samples or a distance matrix of
    size
    ![(n_x + n_y) \times (n_x + n_y)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%28n_x%20%2B%20n_y%29%20%5Ctimes%20%28n_x%20%2B%20n_y%29 "(n_x + n_y) \times (n_x + n_y)")
    stored as an object of class `dist`.
-   `indices1` which is an integer vector of size
    ![n_x](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;n_x "n_x")
    storing the indices of the data points belonging to the first sample
    in the current permuted version of the data.

Some test statistics are already implemented in
[**flipr**](https://lmjl-alea.github.io/flipr/) and ready to use.
User-defined test statistics can be used as well, with the use of the
helper function `use_stat(nsamples = 2, stat_name = )`. This function
creates and saves an `.R` file in the `R/` folder of the current working
directory and populates it with the following template:

``` r
#' Test Statistic for the Two-Sample Problem
#'
#' This function computes the test statistic...
#'
#' @param data A list storing the concatenation of the two samples from which
#'   the user wants to make inference. Alternatively, a distance matrix stored
#'   in an object of class \code{\link[stats]{dist}} of pairwise distances
#'   between data points.
#' @param indices1 An integer vector that contains the indices of the data
#'   points belong to the first sample in the current permuted version of the
#'   data.
#'
#' @return A numeric value evaluating the desired test statistic.
#' @export
#'
#' @examples
#' # TO BE DONE BY THE DEVELOPER OF THE PACKAGE
stat_{{{name}}} <- function(data, indices1) {
  n <- if (inherits(data, "dist"))
    attr(data, "Size")
  else if (inherits(data, "list"))
    length(data)
  else
    stop("The `data` input should be of class either list or dist.")

  indices2 <- seq_len(n)[-indices1]

  x <- data[indices1]
  y <- data[indices2]

  # Here comes the code that computes the desired test
  # statistic from input samples stored in lists x and y

}
```

For instance, a
[**flipr**](https://lmjl-alea.github.io/flipr/)-compatible version of
the
![t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;t "t")-statistic
with pooled variance will look like:

``` r
my_t_stat <- function(data, indices1) {
  n <- if (inherits(data, "dist"))
    attr(data, "Size")
  else if (inherits(data, "list"))
    length(data)
  else
    stop("The `data` input should be of class either list or dist.")

  indices2 <- seq_len(n)[-indices1]

  x <- data[indices1]
  y <- data[indices2]
  
  # Here comes the code that computes the desired test
  # statistic from input samples stored in lists x and y
  x <- unlist(x)
  y <- unlist(y)
  
  stats::t.test(x, y, var.equal = TRUE)$statistic
}
```

Here, we are only going to use the
![t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;t "t")-statistic
for this example, but we might be willing to use more than one statistic
for a parameter or we might have several parameters under investigation,
each one of them requiring a different test statistic. We therefore
group all the test statistics that we need into a single list:

``` r
stat_functions <- list(my_t_stat)
```

### Assign test statistics to parameters

Finally we need to define a named list that tells
[**flipr**](https://lmjl-alea.github.io/flipr/) which test statistics
among the ones declared in the `stat_functions` list should be used for
each parameter under investigation. This is used to determine bounds on
each parameter for the plausibility function. This list, often termed
`stat_assignments`, should therefore have as many elements as there are
parameters under investigation. Each element should be named after a
parameter under investigation and should list the indices corresponding
to the test statistics that should be used for that parameter in
`stat_functions`. In our example, it boils down to:

``` r
stat_assignments <- list(delta = 1)
```

### Use the plausibility function

Now we can instantiate a plausibility function as follows:

``` r
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
#> ! Setting the seed for sampling permutations is mandatory for obtaining a continuous p-value function. Using `seed = 1234`.
```

Now, assume we want to test the following hypotheses:

![H_0: \delta = 0 \quad \mbox{v.s.} \quad H_1: \delta \ne 0.](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;H_0%3A%20%5Cdelta%20%3D%200%20%5Cquad%20%5Cmbox%7Bv.s.%7D%20%5Cquad%20H_1%3A%20%5Cdelta%20%5Cne%200. "H_0: \delta = 0 \quad \mbox{v.s.} \quad H_1: \delta \ne 0.")

We use the `$get_value()` method for this purpose, which essentially
evaluates the permutation
![p](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;p "p")-value
of a two-sided test by default:

``` r
pf$get_value(0)
#> [1] 0.1078921
```

We can compare the resulting
![p](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;p "p")-value
with the one obtained using the more classic parametric test:

``` r
t.test(x, y, var.equal = TRUE)$p.value
#> [1] 0.1030946
```

The permutation
![p](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;p "p")-value
does not quite match the parametric one. This is because of two reasons:

1.  The resolution of a permutation
    ![p](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;p "p")-value
    is of the order of
    ![1/(B+1)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;1%2F%28B%2B1%29 "1/(B+1)"),
    where
    ![B](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;B "B")
    is the number of sampled permutations. By default, the plausibility
    function is instantiated with
    ![B = 1000](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;B%20%3D%201000 "B = 1000"):

``` r
pf$nperms
#> [1] 1000
```

2.  We randomly sample
    ![B](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;B "B")
    permutations out of the
    ![\binom{n_x+n_y}{n_x}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbinom%7Bn_x%2Bn_y%7D%7Bn_x%7D "\binom{n_x+n_y}{n_x}")
    possible permutations and therefore introduce extra variability in
    the
    ![p](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;p "p")-value.

If we were to ask for more permutations, say
![B = 1,000,000](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;B%20%3D%201%2C000%2C000 "B = 1,000,000"),
we would be much closer to the parametric
![p](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;p "p")-value:

``` r
pf$set_nperms(1000000)
pf$get_value(0)
#> [1] 0.1029879
```

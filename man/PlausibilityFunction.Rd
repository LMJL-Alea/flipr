% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plausibility-class.R
\name{PlausibilityFunction}
\alias{PlausibilityFunction}
\title{R6 Class representing a plausibility function}
\description{
A plausibility function is...
}
\examples{

## ------------------------------------------------
## Method `PlausibilityFunction$set_nperms`
## ------------------------------------------------

x <- rnorm(10)
y <- rnorm(10, mean = 2)
null_spec <- function(y, parameters) {purrr::map(y, ~ .x - parameters[1])}
stat_functions <- list(stat_t)
stat_assignments <- list(mean = 1)
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
pf$nperms
pf$set_nperms(10000)
pf$nperms

## ------------------------------------------------
## Method `PlausibilityFunction$set_nperms_max`
## ------------------------------------------------

x <- rnorm(10)
y <- rnorm(10, mean = 2)
null_spec <- function(y, parameters) {purrr::map(y, ~ .x - parameters[1])}
stat_functions <- list(stat_t)
stat_assignments <- list(mean = 1)
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
pf$nperms_max
pf$set_nperms_max(10000)
pf$nperms_max

## ------------------------------------------------
## Method `PlausibilityFunction$set_alternative`
## ------------------------------------------------

x <- rnorm(10)
y <- rnorm(10, mean = 2)
null_spec <- function(y, parameters) {purrr::map(y, ~ .x - parameters[1])}
stat_functions <- list(stat_t)
stat_assignments <- list(mean = 1)
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
pf$alternative
pf$set_alternative("right_tail")
pf$alternative

## ------------------------------------------------
## Method `PlausibilityFunction$set_aggregator`
## ------------------------------------------------

x <- rnorm(10)
y <- rnorm(10, mean = 2)
null_spec <- function(y, parameters) {purrr::map(y, ~ .x - parameters[1])}
stat_functions <- list(stat_t)
stat_assignments <- list(mean = 1)
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
pf$aggregator
pf$set_aggregator("fisher")
pf$aggregator

## ------------------------------------------------
## Method `PlausibilityFunction$set_pvalue_formula`
## ------------------------------------------------

x <- rnorm(10)
y <- rnorm(10, mean = 2)
null_spec <- function(y, parameters) {
  purrr::map(y, ~ .x - parameters[1])
}
stat_functions <- list(stat_t)
stat_assignments <- list(mean = 1)
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
pf$pvalue_formula
pf$set_pvalue_formula("estimate")
pf$pvalue_formula

## ------------------------------------------------
## Method `PlausibilityFunction$get_value`
## ------------------------------------------------

x <- rnorm(10)
y <- rnorm(10, mean = 2)
null_spec <- function(y, parameters) {purrr::map(y, ~ .x - parameters[1])}
stat_functions <- list(stat_t)
stat_assignments <- list(mean = 1)
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
pf$set_nperms(50)
pf$get_value(2)

## ------------------------------------------------
## Method `PlausibilityFunction$set_max_conf_level`
## ------------------------------------------------

x <- rnorm(10)
y <- rnorm(10, mean = 2)
null_spec <- function(y, parameters) {
  purrr::map(y, ~ .x - parameters[1])
}
stat_functions <- list(stat_t)
stat_assignments <- list(mean = 1)
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
pf$max_conf_level
pf$set_max_conf_level(0.999)
pf$max_conf_level

## ------------------------------------------------
## Method `PlausibilityFunction$set_point_estimate`
## ------------------------------------------------

x <- rnorm(10)
y <- rnorm(10, mean = 2)
null_spec <- function(y, parameters) {
  purrr::map(y, ~ .x - parameters[1])
}
stat_functions <- list(stat_t)
stat_assignments <- list(mean = 1)
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
pf$point_estimate
pf$set_point_estimate(mean(y) - mean(x))
pf$point_estimate

## ------------------------------------------------
## Method `PlausibilityFunction$set_parameter_bounds`
## ------------------------------------------------

x <- rnorm(10)
y <- rnorm(10, mean = 2)
null_spec <- function(y, parameters) {
  purrr::map(y, ~ .x - parameters[1])
}
stat_functions <- list(stat_t)
stat_assignments <- list(mean = 1)
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
pf$set_nperms(50)
pf$set_point_estimate(point_estimate = mean(y) - mean(x))
pf$parameters
pf$set_parameter_bounds(
  point_estimate = pf$point_estimate,
  conf_level = 0.8
)
pf$parameters

## ------------------------------------------------
## Method `PlausibilityFunction$set_grid`
## ------------------------------------------------

x <- rnorm(10)
y <- rnorm(10, mean = 2)
null_spec <- function(y, parameters) {
  purrr::map(y, ~ .x - parameters[1])
}
stat_functions <- list(stat_t)
stat_assignments <- list(mean = 1)
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
pf$set_nperms(50)
pf$set_point_estimate(mean(y) - mean(x))
pf$set_parameter_bounds(
  point_estimate = pf$point_estimate,
  conf_level = 0.8
)
pf$set_grid(
  parameters = pf$parameters,
  npoints = 2L
)

## ------------------------------------------------
## Method `PlausibilityFunction$evaluate_grid`
## ------------------------------------------------

x <- rnorm(10)
y <- rnorm(10, mean = 2)
null_spec <- function(y, parameters) {
  purrr::map(y, ~ .x - parameters[1])
}
stat_functions <- list(stat_t)
stat_assignments <- list(mean = 1)
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
pf$set_nperms(50)
pf$set_point_estimate(mean(y) - mean(x))
pf$set_parameter_bounds(
  point_estimate = pf$point_estimate,
  conf_level = 0.8
)
pf$set_grid(
  parameters = pf$parameters,
  npoints = 2L
)
pf$evaluate_grid(grid = pf$grid)
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{nparams}}{An integer specifying the number of parameters to be
inferred. Default is \code{1L}.}

\item{\code{nperms}}{An integer specifying the number of permutations to be
sampled. Default is \code{1000L}.}

\item{\code{nperms_max}}{An integer specifying the total number of distinct
permutations that can be made given the sample sizes.}

\item{\code{alternative}}{A string specifying the type of alternative
hypothesis. Choices are \code{"two_tail"}, \code{"left_tail"} and \verb{"right_tail}.
Defaults to \code{"two_tail"}.}

\item{\code{aggregator}}{A string specifying which function should be used to
aggregate test statistic values when non-parametric combination is used
(i.e. when multiple test statistics are used). Choices are \code{"tippett"}
and \verb{"fisher} for now. Defaults to \code{"tippett"}.}

\item{\code{pvalue_formula}}{A string specifying which formula to use for
computing the permutation p-value. Choices are either \code{probability}
(default) or \code{estimator}. The former provides p-values that lead to
exact hypothesis tests while the latter provides an unbiased estimate
of the traditional p-value.}

\item{\code{max_conf_level}}{A numeric value specifying the maximum confidence
level that we aim to achieve for the confidence regions. This is used
to compute bounds on each parameter of interest in order to fit a
Kriging model that approximates the expensive plausibility function on
a hypercube. Defaults to \code{0.99}.}

\item{\code{point_estimate}}{A numeric vector providing point estimates for the
parameters of interest.}

\item{\code{parameters}}{A list of functions of class \code{param} produced via
\code{\link[dials]{new_quant_param}} that stores the parameters to be
inferred along with important properties such as their name, range,
etc. Defaults to \code{NULL}.}

\item{\code{grid}}{A tibble storing evaluations of the plausibility function on
a regular centered grid of the parameter space. Defaults to \code{NULL}.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PlausibilityFunction-new}{\code{PlausibilityFunction$new()}}
\item \href{#method-PlausibilityFunction-set_nperms}{\code{PlausibilityFunction$set_nperms()}}
\item \href{#method-PlausibilityFunction-set_nperms_max}{\code{PlausibilityFunction$set_nperms_max()}}
\item \href{#method-PlausibilityFunction-set_alternative}{\code{PlausibilityFunction$set_alternative()}}
\item \href{#method-PlausibilityFunction-set_aggregator}{\code{PlausibilityFunction$set_aggregator()}}
\item \href{#method-PlausibilityFunction-set_pvalue_formula}{\code{PlausibilityFunction$set_pvalue_formula()}}
\item \href{#method-PlausibilityFunction-get_value}{\code{PlausibilityFunction$get_value()}}
\item \href{#method-PlausibilityFunction-set_max_conf_level}{\code{PlausibilityFunction$set_max_conf_level()}}
\item \href{#method-PlausibilityFunction-set_point_estimate}{\code{PlausibilityFunction$set_point_estimate()}}
\item \href{#method-PlausibilityFunction-set_parameter_bounds}{\code{PlausibilityFunction$set_parameter_bounds()}}
\item \href{#method-PlausibilityFunction-set_grid}{\code{PlausibilityFunction$set_grid()}}
\item \href{#method-PlausibilityFunction-evaluate_grid}{\code{PlausibilityFunction$evaluate_grid()}}
\item \href{#method-PlausibilityFunction-clone}{\code{PlausibilityFunction$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PlausibilityFunction-new"></a>}}
\if{latex}{\out{\hypertarget{method-PlausibilityFunction-new}{}}}
\subsection{Method \code{new()}}{
Create a new plausibility function object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PlausibilityFunction$new(
  null_spec,
  stat_functions,
  stat_assignments,
  ...,
  seed = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{null_spec}}{A function or an R object coercible into a function (via
\code{rlang::as_function()}). For one-sample problems, it should transform
the \code{x} sample (provided as first argument) using the parameters (as
second argument) to make its distribution centered symmetric. For
two-sample problems, it should transform the \code{y} sample (provided as
first argument) using the parameters (as second argument) to make it
exchangeable with the \code{x} sample under a null hypothesis.}

\item{\code{stat_functions}}{A vector or list of functions (or R objects
coercible into functions via \code{rlang::as_function()}) specifying the
whole set of test statistics that should be used.}

\item{\code{stat_assignments}}{A named list of integer vectors specifying which
test statistic should be associated with each parameter. The length of
this list should match the number of parameters under investigation and
is thus used to set it. Each element of the list should be named after
the parameter it identifies.}

\item{\code{...}}{Vectors, matrices or lists providing the observed samples.}

\item{\code{seed}}{A numeric value specifying the seed to be used. Defaults to
\code{NULL} in which case \code{seed = 1234} is used and the user is informed of
this setting.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{PlausibilityFunction} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PlausibilityFunction-set_nperms"></a>}}
\if{latex}{\out{\hypertarget{method-PlausibilityFunction-set_nperms}{}}}
\subsection{Method \code{set_nperms()}}{
Change the value of the \code{nperms} field.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PlausibilityFunction$set_nperms(val)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{val}}{New value for the number of permutations to be sampled.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{x <- rnorm(10)
y <- rnorm(10, mean = 2)
null_spec <- function(y, parameters) {purrr::map(y, ~ .x - parameters[1])}
stat_functions <- list(stat_t)
stat_assignments <- list(mean = 1)
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
pf$nperms
pf$set_nperms(10000)
pf$nperms
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PlausibilityFunction-set_nperms_max"></a>}}
\if{latex}{\out{\hypertarget{method-PlausibilityFunction-set_nperms_max}{}}}
\subsection{Method \code{set_nperms_max()}}{
Change the value of the \code{nperms_max} field.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PlausibilityFunction$set_nperms_max(val)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{val}}{New value for the total number of of possible distinct
permutations.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{x <- rnorm(10)
y <- rnorm(10, mean = 2)
null_spec <- function(y, parameters) {purrr::map(y, ~ .x - parameters[1])}
stat_functions <- list(stat_t)
stat_assignments <- list(mean = 1)
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
pf$nperms_max
pf$set_nperms_max(10000)
pf$nperms_max
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PlausibilityFunction-set_alternative"></a>}}
\if{latex}{\out{\hypertarget{method-PlausibilityFunction-set_alternative}{}}}
\subsection{Method \code{set_alternative()}}{
Change the value of the \code{alternative} field.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PlausibilityFunction$set_alternative(val)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{val}}{New value for the type of alternative hypothesis.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{x <- rnorm(10)
y <- rnorm(10, mean = 2)
null_spec <- function(y, parameters) {purrr::map(y, ~ .x - parameters[1])}
stat_functions <- list(stat_t)
stat_assignments <- list(mean = 1)
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
pf$alternative
pf$set_alternative("right_tail")
pf$alternative
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PlausibilityFunction-set_aggregator"></a>}}
\if{latex}{\out{\hypertarget{method-PlausibilityFunction-set_aggregator}{}}}
\subsection{Method \code{set_aggregator()}}{
Change the value of the \code{aggregator} field.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PlausibilityFunction$set_aggregator(val)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{val}}{New value for the string specifying which function should be
used to aggregate test statistic values when non-parametric combination
is used (i.e. when multiple test statistics are used).}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{x <- rnorm(10)
y <- rnorm(10, mean = 2)
null_spec <- function(y, parameters) {purrr::map(y, ~ .x - parameters[1])}
stat_functions <- list(stat_t)
stat_assignments <- list(mean = 1)
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
pf$aggregator
pf$set_aggregator("fisher")
pf$aggregator
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PlausibilityFunction-set_pvalue_formula"></a>}}
\if{latex}{\out{\hypertarget{method-PlausibilityFunction-set_pvalue_formula}{}}}
\subsection{Method \code{set_pvalue_formula()}}{
Change the value of the \code{pvalue_formula} field.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PlausibilityFunction$set_pvalue_formula(val)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{val}}{New value for the string specifying which formula should be
used to compute the permutation p-value.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{x <- rnorm(10)
y <- rnorm(10, mean = 2)
null_spec <- function(y, parameters) {
  purrr::map(y, ~ .x - parameters[1])
}
stat_functions <- list(stat_t)
stat_assignments <- list(mean = 1)
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
pf$pvalue_formula
pf$set_pvalue_formula("estimate")
pf$pvalue_formula
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PlausibilityFunction-get_value"></a>}}
\if{latex}{\out{\hypertarget{method-PlausibilityFunction-get_value}{}}}
\subsection{Method \code{get_value()}}{
Computes an indicator of the plausibility of specific values
for the parameters of interest in the form of a p-value of an
hypothesis test against these values.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PlausibilityFunction$get_value(
  parameters,
  keep_null_distribution = FALSE,
  keep_permutations = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{parameters}}{A vector whose length should match the \code{nparams} field
providing specific values of the parameters of interest for assessment
of their plausibility in the form of a p-value of the corresponding
hypothesis test.}

\item{\code{keep_null_distribution}}{A boolean specifying whether the empirical
permutation null distribution should be returned as well. Defaults to
\code{FALSE}.}

\item{\code{keep_permutations}}{A boolean specifying whether the list of sampled
permutations used to compute the empirical permutation null
distribution should be returned as well. Defaults to \code{FALSE}.}

\item{\code{...}}{Extra parameters specific to some statistics.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{x <- rnorm(10)
y <- rnorm(10, mean = 2)
null_spec <- function(y, parameters) {purrr::map(y, ~ .x - parameters[1])}
stat_functions <- list(stat_t)
stat_assignments <- list(mean = 1)
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
pf$set_nperms(50)
pf$get_value(2)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PlausibilityFunction-set_max_conf_level"></a>}}
\if{latex}{\out{\hypertarget{method-PlausibilityFunction-set_max_conf_level}{}}}
\subsection{Method \code{set_max_conf_level()}}{
Change the value of the \code{max_conf_level} field.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PlausibilityFunction$set_max_conf_level(val)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{val}}{New value for the maximum confidence level that we aim to
achieve for the confidence regions.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{x <- rnorm(10)
y <- rnorm(10, mean = 2)
null_spec <- function(y, parameters) {
  purrr::map(y, ~ .x - parameters[1])
}
stat_functions <- list(stat_t)
stat_assignments <- list(mean = 1)
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
pf$max_conf_level
pf$set_max_conf_level(0.999)
pf$max_conf_level
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PlausibilityFunction-set_point_estimate"></a>}}
\if{latex}{\out{\hypertarget{method-PlausibilityFunction-set_point_estimate}{}}}
\subsection{Method \code{set_point_estimate()}}{
Change the value of the \code{point_estimate} field.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PlausibilityFunction$set_point_estimate(
  point_estimate = NULL,
  lower_bound = -10,
  upper_bound = 10,
  ncores = 1L,
  estimate = FALSE,
  overwrite = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{point_estimate}}{A numeric vector providing rough point estimates for the
parameters under investigation.}

\item{\code{lower_bound}}{A scalar or numeric vector specifying the lower bounds
for each parameter under investigation. If it is a scalar, the value is
used as lower bound for all parameters. Defaults to \code{-10}.}

\item{\code{upper_bound}}{A scalar or numeric vector specifying the lower bounds
for each parameter under investigation. If it is a scalar, the value is
used as lower bound for all parameters. Defaults to \code{10}.}

\item{\code{ncores}}{An integer specifying the number of cores to use for
maximizing the plausibility function to get a point estimate of the
parameters. Defaults to \code{1L}.}

\item{\code{estimate}}{A boolean specifying whether the rough point estimate
provided by \code{val} should serve as initial point for maximizing the
plausibility function (\code{estimate = TRUE}) or as final point estimate
for the parameters (\code{estimate = FALSE}). Defaults to \code{FALSE}.}

\item{\code{overwrite}}{A boolean specifying whether to force the computation if
it has already been set. Defaults to \code{FALSE}.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{x <- rnorm(10)
y <- rnorm(10, mean = 2)
null_spec <- function(y, parameters) {
  purrr::map(y, ~ .x - parameters[1])
}
stat_functions <- list(stat_t)
stat_assignments <- list(mean = 1)
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
pf$point_estimate
pf$set_point_estimate(mean(y) - mean(x))
pf$point_estimate
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PlausibilityFunction-set_parameter_bounds"></a>}}
\if{latex}{\out{\hypertarget{method-PlausibilityFunction-set_parameter_bounds}{}}}
\subsection{Method \code{set_parameter_bounds()}}{
Change the value of the \code{parameters} field.

Updates the range of the parameters under investigation.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PlausibilityFunction$set_parameter_bounds(point_estimate, conf_level)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{point_estimate}}{A numeric vector providing a point estimate for
each parameter under investigation. If no estimator is known by the
user, (s)he can resort to the \verb{$set_point_estimate()} method to get a
point estimate by maximizing the plausibility function.}

\item{\code{conf_level}}{A numeric value specifying the confidence level to be
used for setting parameter bounds. It should be in (0,1).}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{x <- rnorm(10)
y <- rnorm(10, mean = 2)
null_spec <- function(y, parameters) {
  purrr::map(y, ~ .x - parameters[1])
}
stat_functions <- list(stat_t)
stat_assignments <- list(mean = 1)
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
pf$set_nperms(50)
pf$set_point_estimate(point_estimate = mean(y) - mean(x))
pf$parameters
pf$set_parameter_bounds(
  point_estimate = pf$point_estimate,
  conf_level = 0.8
)
pf$parameters
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PlausibilityFunction-set_grid"></a>}}
\if{latex}{\out{\hypertarget{method-PlausibilityFunction-set_grid}{}}}
\subsection{Method \code{set_grid()}}{
Computes a tibble storing a regular centered grid of the
parameter space.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PlausibilityFunction$set_grid(parameters, npoints = 20L)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{parameters}}{A list of \code{\link[dials]{new_quant_param}} objects
containing information about the parameters under investigation. It
should contain the fields \code{point_estimate} and \code{range}.}

\item{\code{npoints}}{An integer specifying the number of points to discretize
each dimension. Defaults to \code{20L}.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{x <- rnorm(10)
y <- rnorm(10, mean = 2)
null_spec <- function(y, parameters) {
  purrr::map(y, ~ .x - parameters[1])
}
stat_functions <- list(stat_t)
stat_assignments <- list(mean = 1)
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
pf$set_nperms(50)
pf$set_point_estimate(mean(y) - mean(x))
pf$set_parameter_bounds(
  point_estimate = pf$point_estimate,
  conf_level = 0.8
)
pf$set_grid(
  parameters = pf$parameters,
  npoints = 2L
)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PlausibilityFunction-evaluate_grid"></a>}}
\if{latex}{\out{\hypertarget{method-PlausibilityFunction-evaluate_grid}{}}}
\subsection{Method \code{evaluate_grid()}}{
Updates the \code{grid} field with a \code{pvalue} column storing
evaluations of the plausibility function on the regular centered grid
of the parameter space.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PlausibilityFunction$evaluate_grid(grid, ncores = 1L)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{grid}}{A \code{\link[tibble]{tibble}} storing a grid that spans the
space of parameters under investigation.}

\item{\code{ncores}}{An integer specifying the number of cores to run
evaluations in parallel. Defaults to \code{1L}.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{x <- rnorm(10)
y <- rnorm(10, mean = 2)
null_spec <- function(y, parameters) {
  purrr::map(y, ~ .x - parameters[1])
}
stat_functions <- list(stat_t)
stat_assignments <- list(mean = 1)
pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
pf$set_nperms(50)
pf$set_point_estimate(mean(y) - mean(x))
pf$set_parameter_bounds(
  point_estimate = pf$point_estimate,
  conf_level = 0.8
)
pf$set_grid(
  parameters = pf$parameters,
  npoints = 2L
)
pf$evaluate_grid(grid = pf$grid)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PlausibilityFunction-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PlausibilityFunction-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PlausibilityFunction$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}

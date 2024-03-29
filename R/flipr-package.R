#' flipr: Flexible inference via permutations in R
#'
#' The flipr package provides a flexible permutation framework for making
#' inference such as point estimation, confidence intervals or hypothesis
#' testing, on any kind of data, be it univariate, multivariate, or more complex
#' such as network-valued data, topological data, functional data or
#' density-valued data.
#'
#' @useDynLib flipr, .registration = TRUE
#' @import ggplot2
#' @importFrom pbapply pblapply pbsapply
#' @importFrom R6 R6Class
#' @importFrom Rcpp sourceCpp
#' @import rlang
#' @importFrom tibble tibble
#' @keywords internal
"_PACKAGE"

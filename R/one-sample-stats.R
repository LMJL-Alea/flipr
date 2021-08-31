#' Test Statistics for the One-Sample Problem
#'
#' This is a collection of functions that provide test statistics to be used
#' into the permutation scheme for performing one-sample testing.
#'
#' @param data A list storing the sample from which the user wants to make
#'   inference.
#' @param flips A numeric vectors of `-1`s and `1`s to be used to randomly flip
#'   some data points around the center of symmetric of the distribution of the
#'   sample.
#'
#' @return A numeric value evaluating the desired test statistic.
#' @name one-sample-stats
#'
#' @examples
#' n <- 10
#' x <- as.list(rnorm(n))
#' flips <- sample(c(-1, 1), n, replace = TRUE)
#' stat_max(x, flips)
NULL

#' @rdname one-sample-stats
#' @export
stat_max <- function(data, flips) {
  stopifnot(inherits(data, "list"))
  n <- length(data)

  data <- one_sample_prep(data, flips)
  Xbar <- purrr::map_dbl(data, mean)
  max(abs(Xbar))
}

one_sample_prep <- function(data, flips) {
  data %>%
    purrr::transpose() %>%
    purrr::simplify_all() %>%
    purrr::map(~ .x * flips)
}

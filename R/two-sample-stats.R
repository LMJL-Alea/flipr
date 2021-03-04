#' Test Statistics for the Two-Sample Problem
#'
#' This function implements the original Hotelling's $T^2$ statistic which is
#' defined for multivariate data when the number $n$ of observations is greater
#' than the number $p$ of variables.
#'
#' @param data A list of the `n1 + n2` concatenated observations with the
#'   original `n1` observations from the first sample on top and the original
#'   `n2` observations from the second sample below.
#' @param indices An integer vector giving the indices in `data` that are
#'   considered to belong to the first sample.
#' @param ... Extra parameters (might be useful for other user-supplied
#'   `stat_*()` functions). Not used here.
#'
#' @return A real scalar giving the value of Hotelling's $T^2$ statistic.
#' @name test-statistic
#'
#' @examples
#' n <- 10L
#' mx <- 0
#' sigma <- 1
#'
#' # Two different models for the two populations
#' x <- rnorm(n = n, mean = mx, sd = sigma)
#' x <- as.list(x)
#' delta <- 10
#' my <- mx + delta
#' y <- rnorm(n = n, mean = my, sd = sigma)
#' y <- as.list(y)
#' stat_hotelling(c(x, y), 1:n)
#' stat_t(c(x, y), 1:n)
#' stat_mean(c(x, y), 1:n)
NULL

#' @rdname test-statistic
#' @export
stat_hotelling <- function(data, indices, ...) {
  n <- length(data)
  nx <- length(indices)
  ny <- n - nx
  indices2 <- seq_len(n)[-indices]
  X <- purrr::reduce(data[indices], rbind)
  Y <- purrr::reduce(data[indices2], rbind)
  Xbar <- colMeans(X)
  Ybar <- colMeans(Y)
  Sx <- stats::cov(X)
  Sy <- stats::cov(Y)
  Spooled <- ((nx - 1) * Sx + (ny - 1) * Sy) / (nx + ny - 2)
  Sinv <- solve(Spooled)
  D <- Xbar - Ybar
  as.numeric(t(D) %*% Sinv %*% D)
}

#' @rdname test-statistic
#' @export
stat_t <- function(data, indices, ...) {
  n <- length(data)
  n1 <- length(indices)
  n2 <- n - n1
  indices2 <- seq_len(n)[-indices]
  x1 <- unlist(data[indices])
  x2 <- unlist(data[indices2])
  stats::t.test(x1, x2, var.equal = TRUE)$statistic
}

#' @rdname test-statistic
#' @export
stat_f <- function(data, indices, ...) {
  n <- length(data)
  n1 <- length(indices)
  n2 <- n - n1
  indices2 <- seq_len(n)[-indices]
  x1 <- unlist(data[indices])
  x2 <- unlist(data[indices2])
  stats::var.test(x1, x2)$statistic
}

#' @rdname test-statistic
#' @export
stat_mean <- function(data, indices, ...) {
  n <- length(data)
  n1 <- length(indices)
  n2 <- n - n1
  indices2 <- seq_len(n)[-indices]
  x1 <- unlist(data[indices])
  x2 <- unlist(data[indices2])
  mean(x1) - mean(x2)
}

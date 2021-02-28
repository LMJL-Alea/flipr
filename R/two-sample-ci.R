#' Two-Sample Permutation Confidence Interval
#'
#' This function calculates permutation confidence intervals for two-sample
#' problems. This is done through the specification a point estimate of the
#' parameter to be estimated and of a set of null hypotheses of the form `F_X =
#' F_{g(Y, parameters)}` where `g` is a user-supplied function.
#'
#' @param point_estimate A scalar value providing a point estimate of the
#'   parameter for which we want to compute a confidence interval.
#' @param alpha A scalar specifying the desired significance level.
#' @inheritParams two_sample_pf
#'
#' @return A length-2 vector providing the confidence interval at the required
#'   level.
#' @export
#'
#' @examples
#' x1 <- rnorm(10)
#' x2 <- rnorm(10, 3)
#' null_spec <- function(y, parameters) {y - parameters[1]}
#' two_sample_ci(
#'   point_estimate = mean(x2) - mean(x1),
#'   alpha = 0.05,
#'   null_specification = null_spec,
#'   x = x1,
#'   y = x2,
#'   statistic = stat_t,
#'   seed = 1234,
#'   B = 1000,
#'   alternative = "two_tail"
#' )
two_sample_ci <- function(point_estimate, alpha,
                          null_specification,
                          x, y,
                          statistic = stat_hotelling,
                          B = 1000L,
                          type = "exact",
                          combine_with = "tippett",
                          seed = NULL,
                          alternative = "right_tail") {
  stopifnot(length(point_estimate) == 1)

  cost <- function(.x) {
    pvalues <- two_sample_pf(
      parameters = .x,
      null_specification = null_specification,
      x = x,
      y = y,
      statistic = statistic,
      B = B,
      type = type,
      combine_with = combine_with,
      seed = seed,
      alternative = alternative
    )
    pvalues - alpha
  }

  lb <- stats::uniroot(
    f = cost,
    interval = c(point_estimate / 10, point_estimate),
    extendInt = "upX"
  )$root
  ub <- stats::uniroot(
    f = cost,
    interval = c(point_estimate, point_estimate * 10),
    extendInt = "downX"
  )$root

  c(lb, ub)
}

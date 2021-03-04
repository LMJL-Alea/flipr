#' Two-Sample Permutation Confidence Interval
#'
#' This function calculates permutation confidence intervals for two-sample
#' problems. This is done through the specification a point estimate of the
#' parameter to be estimated and of a set of null hypotheses of the form `F_X =
#' F_{g(Y, parameters)}` where `g` is a user-supplied function.
#'
#' @inheritParams two_sample_pe
#' @param alpha A scalar specifying the desired significance level. Default is
#'   `0.05`.
#' @param point_estimate A scalar providing a point estimate of the parameter
#'   under investigation. Default is `NULL`, in which case the point estimate is
#'   computed by maximizing the p-value function via the
#'   \code{\link{two_sample_pe}} function.
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
#'   null_specification = null_spec,
#'   x = x1,
#'   y = x2,
#'   statistic = stat_t,
#'   alternative = "two_tail",
#'   lower_bound = 2,
#'   upper_bound = 4,
#'   B = 100L
#' )
two_sample_ci <- function(null_specification,
                          x, y,
                          alpha = 0.05,
                          statistic = stat_hotelling,
                          B = 1000L,
                          alternative = "right_tail",
                          type = "exact",
                          point_estimate = NULL,
                          lower_bound = 0,
                          upper_bound = 1,
                          seed = NULL) {
  stopifnot(length(statistic) == 1)

  if (is.null(seed)) {
    seed <- 1234
    cli::cli_alert_info(
      "Setting the seed for sampling permutations is mandatory for obtaining a continuous p-value function. Using `seed = 1234`."
    )
  }

  if (is.null(point_estimate))
  {
    point_estimate <- two_sample_pe(
      null_specification = null_specification,
      x = x, y = y,
      statistic = statistic,
      B = B,
      alternative = alternative,
      type = type,
      lower_bound = lower_bound,
      upper_bound = upper_bound,
      seed = seed
    )
  }

  cost <- function(.x) {
    pvalues <- two_sample_pf(
      parameters = .x,
      null_specification = null_specification,
      x = x,
      y = y,
      statistic = statistic,
      B = B,
      alternative = alternative,
      type = type,
      seed = seed
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

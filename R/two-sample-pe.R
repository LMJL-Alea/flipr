#' Two-Sample Permutation Point Estimate
#'
#' This function produces a point estimate of the parameters under investigation
#' using the permutation framework.
#'
#' @inheritParams two_sample_pf
#' @param lower_bound A scalar value specifying a rough lower bound of the
#'   interval over which searching the maximum of the p-value function. Default
#'   is `0`.
#' @param upper_bound A scalar value specifying a rough upper bound of the
#'   interval over which searching the maximum of the p-value function. Default
#'   is `1`.
#' @param verbose A boolean specifying whether to display some information about
#'   the resolution of the maximization problem. Default is `FALSE`.
#'
#' @return A numerical vector providing a point estimate of the parameters.
#' @export
#'
#' @examples
#' x1 <- rnorm(10)
#' x2 <- rnorm(10, mean = 3)
#' null_spec <- function(y, parameters) {y - parameters[1]}
#' two_sample_pe(
#'   null_specification = null_spec,
#'   x = x1, y = x2,
#'   statistic = stat_t,
#'   lower_bound = 2,
#'   upper_bound = 4
#' )
two_sample_pe <- function(null_specification,
                          x, y,
                          statistic = stat_hotelling,
                          B = 1000,
                          alternative = "two_tail",
                          type = "exact",
                          lower_bound = 0,
                          upper_bound = 1,
                          seed = NULL,
                          verbose = FALSE) {
  stopifnot(length(statistic) == 1)

  if (is.null(seed)) {
    seed <- 1234
    cli::cli_alert_info(
      "Setting the seed for sampling permutations
      is mandatory for obtaining a continuous p-value
      function. Using `seed = 1234`."
    )
  }

  continue_loop <- TRUE
  while (continue_loop) {
    if (verbose) {
      writeLines(paste0(
        "Searching point estimate on the interval [",
        lower_bound,
        ", ",
        upper_bound,
        "]..."
      ))
    }
    opt <- stats::optimise(
      f = two_sample_pf,
      interval = c(lower_bound, upper_bound),
      null_specification = null_specification,
      x = x,
      y = y,
      statistic = statistic,
      B = B,
      alternative = alternative,
      type = type,
      seed = seed,
      maximum = TRUE
    )
    x0 <- opt$maximum
    fval <- opt$objective
    if (verbose) {
      writeLines(paste0(
        "Local maximum ",
        round(fval, 3),
        " reached at x0 = ",
        round(x0, 3),
        "."
      ))
    }
    condition_lb <- abs(x0 - lower_bound) / abs(lower_bound) < 0.01
    condition_ub <- abs(x0 - upper_bound) / abs(upper_bound) < 0.01
    condition_pv <- abs(fval - 1) > 0.01
    continue_loop <- condition_lb || condition_ub || condition_pv
    if (condition_lb)
      lower_bound <- lower_bound - 1.5 * (upper_bound - lower_bound)
    if (condition_ub)
      upper_bound <- upper_bound + 1.5 * (upper_bound - lower_bound)
    if (condition_pv) {
      if (!condition_lb)
        lower_bound <- lower_bound - 0.5 * (upper_bound - lower_bound)
      if (!condition_ub)
      upper_bound <- upper_bound + 0.5 * (upper_bound - lower_bound)
    }
  }
  x0
}

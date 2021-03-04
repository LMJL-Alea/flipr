#' Two-Sample Permutation P-Value Function
#'
#' This function calculates the permutation p-value function for two-sample
#' problems. This is done through the specification of a set of null hypotheses
#' of the form `F_X = F_{g(Y, parameters)}` where `g` is a user-supplied
#' function.
#'
#' @param parameters A list of vectors specifying a set of candidate parameter
#'   values under the null hypothesis.
#' @param null_specification A function with two arguments `y` and `parameters`
#'   such that `F_X = F_{null_specification(Y, parameters)}` under the null
#'   hypothesis.
#' @inheritParams two_sample_test
#'
#' @return A vector of p-values of the permutation test for each parameter
#'   values specified through the first argument.
#' @export
#'
#' @examples
#' x1 <- rnorm(10)
#' x2 <- rnorm(10, 3)
#' null_spec <- function(y, parameters) {y - parameters[1]}
#' two_sample_pf(
#'   parameters = 3,
#'   null_specification = null_spec,
#'   x = x1,
#'   y = x2,
#'   statistic = stat_t,
#'   seed = 1234,
#'   B = 1000,
#'   alternative = "two_tail"
#' )
two_sample_pf <- function(parameters,
                          null_specification,
                          x, y,
                          statistic = stat_hotelling,
                          B = 1000L,
                          alternative = "right_tail",
                          combine_with = "tippett",
                          type = "exact",
                          seed = NULL) {
  if (!is.null(seed)) withr::local_seed(seed)
  null_spec <- rlang::as_function(null_specification)
  parameters %>%
    purrr::map_dbl(~ {
      y <- null_spec(y, .x)
      two_sample_test(
        x = x,
        y = y,
        statistic = statistic,
        B = B,
        type = type,
        alternative = alternative,
        combine_with = combine_with
      )$pvalue
    })
}

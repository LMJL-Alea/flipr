#' Two-Sample Permutation P-Value Function
#'
#' This function calculates the permutation p-value function for two-sample
#' problems. This is done through the specification of a set of null hypotheses
#' of the form $F_X = F_{g(Y)}$ where $g$ is a user-supplied function.
#'
#' @param null_specification A function such that `F_X = F_{null_specification(Y)}`.
#' @inheritParams two_sample_test
#' @param alternative A string specifying whether the p-value is right-tailed,
#'   left-tailed or two-tailed. Choices are `"right_tail"`, `"left_tail"` and
#'   `"two_tail"`. Default is `"right_tail"`. Obviously, if the test statistic
#'   used in argument `statistic` is positive, all alternatives will lead to the
#'   two-tailed p-value.
#'
#' @return A scalar value giving the p-value of the permutation test using the
#'   null hypothesis specified through the first argument.
#' @export
#'
#' @examples
#' x1 <- rnorm(10)
#' x2 <- rnorm(10, 3)
#' two_sample_pf(
#'   null_spec = function(y) {y - 3},
#'   x = x1,
#'   y = x2,
#'   statistic = stat_t,
#'   seed = 1234,
#'   B = 1000,
#'   alternative = "two_tail"
#' )
two_sample_pf <- function(null_specification,
                          x, y,
                          statistic = stat_hotelling,
                          B = 1000L,
                          type = "exact",
                          combine_with = "tippett",
                          seed = NULL,
                          alternative = "right_tail") {
  if (!is.null(seed)) set.seed(seed)
  y <- rlang::as_function(null_specification)(y)
  two_sample_test(
    x = x,
    y = y,
    statistic = statistic,
    B = B,
    type = type,
    alternative = alternative,
    combine_with = combine_with
  )$pvalue
}

#' One-Sample Permutation Test
#'
#' This function carries out an hypothesis test where the null hypothesis is
#' that the sample is governed by a generative probability distribution which is
#' centered and symmetric against the alternative hypothesis that they are
#' governed by a probability distribution that is either not centered or not
#' symmetric.
#'
#' @section User-supplied statistic function:
#' A user-specified function should have at least two arguments:
#'
#' - the first argument is `data` which should be a list of the `n`
#' observations from the sample;
#' - the second argument is `flips` which should be an integer vector giving
#' the signs by which each observation in `data` should be multiplied.
#'
#' It is possible to use the \code{\link{use_stat}} function with `nsamples = 1`
#' to have **flipr** automatically generate a template file for writing down
#' your own test statistics in a way that makes it compatible with the **flipr**
#' framework.
#'
#' See the \code{\link{stat_max}} function for an example.
#'
#' @param x A numeric vector or a numeric matrix or a list representing the
#'   sample from which the user wants to make inference.
#' @inheritParams two_sample_test
#'
#' @return A \code{\link[base]{list}} with three components: the value of the
#'   statistic for the original two samples, the p-value of the resulting
#'   permutation test and a numeric vector storing the values of the permuted
#'   statistics.
#' @export
#'
#' @examples
#' n <- 10L
#' mu <- 3
#' sigma <- 1
#'
#' # Sample under the null distribution
#' x1 <- rnorm(n = n, mean = 0, sd = sigma)
#' t1 <- one_sample_test(x1)
#' t1$pvalue
#'
#' # Sample under some alternative distribution
#' x2 <- rnorm(n = n, mean = mu, sd = sigma)
#' t2 <- one_sample_test(x2)
#' t2$pvalue
one_sample_test <- function(x,
                            stats = list(stat_max),
                            B = 1000L,
                            M = NULL,
                            alternative = "two_tail",
                            combine_with = "tippett",
                            type = "exact",
                            seed = NULL) {
  if (!is.numeric(x) && !is.matrix(x) && !is.list(x))
    abort("The input data should be of class numeric, matrix or list.")

  if (!is.null(seed)) withr::local_seed(seed)

  l <- convert_to_list(x)
  stat_data <- l[[1]]
  n <- length(stat_data)

  # Compute total number of permutations yielding to distinct
  # values of the test statistic
  if (is.null(M))
    M <- 2^n - 1

  # Generate permutation data
  if (M <= B) {
    B <- M
    perm_data <- flipn(n)[, 1:B]
  } else {
    perm_data <- replicate(B, stats::rbinom(n, 1, 0.5) * 2 - 1)
  }
  perm_data <- cbind(
    rep(1, nrow(perm_data)),
    perm_data
  )

  run_permutation_scheme(
    type = type,
    alternative = alternative,
    stats = stats,
    B = B,
    perm_data = perm_data,
    stat_data = stat_data,
    M = M,
    combine_with = combine_with
  )
}

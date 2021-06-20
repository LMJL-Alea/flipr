#' Two-Sample Permutation Test
#'
#' This function carries out an hypothesis test in which the null hypothesis is
#' that the two samples are governed by the same underlying generative
#' probability distribution against the alternative hypothesis that they are
#' governed by two different generative probability distributions.
#'
#' @section User-supplied statistic function:
#' A user-specified function should have at least two arguments:
#'
#' - the first argument is `data` which should be a list of the `n1 + n2`
#' concatenated observations with the original `n1` observations from the first
#' sample on top and the original `n2` observations from the second sample
#' below;
#' - the second argument is `perm_data` which should be an integer vector giving
#' the indices in `data` that are considered to belong to the first sample.
#'
#' See the \code{\link{stat_t}} function for an example.
#'
#' @param x A numeric vector or a numeric matrix or a list representing the 1st
#'   sample.
#' @param y A numeric vector or a numeric matrix or a list representing the 2nd
#'   sample.
#' @param stats A list of functions produced by \code{\link[rlang]{as_function}}
#'   specifying the chosen test statistic(s). A number of test statistic
#'   functions are implemented in the package and can be used as such.
#'   Alternatively, one can provide its own implementation of test statistics
#'   that (s)he deems relevant for the problem at hand. See the section
#'   *User-supplied statistic function* for more information on how these
#'   user-supplied functions should be structured for compatibility with the
#'   **flipr** framwork. Default is \code{\link{stat_t}}.
#' @param B The number of sampled permutations. Default is `1000L`.
#' @param M The total number of possible permutations. Defaults to `NULL`, which
#'   means that it is automatically computed from the given sample size(s).
#' @param alternative A single string or a character vector specifying whether
#'   the p-value is right-tailed, left-tailed or two-tailed. Choices are
#'   `"right_tail"`, `"left_tail"` and `"two_tail"`. Default is `"two_tail"`. If
#'   a single string is provided, it is assumed that it should be applied to all
#'   test statistics provided by the user. Alternative, the length of
#'   `alternative` should match the length of the `stats` parameter and it is
#'   assumed that there is a one-to-one correspondence.
#' @param combine_with A string specifying the combining function to be used to
#'   compute the single test statistic value from the set of p-value estimates
#'   obtained during the non-parametric combination testing procedure. For now,
#'   choices are either `"tippett"` or `"fisher"`. Default is `"tippett"`, which
#'   picks Tippett's function.
#' @param type A string specifying if performing an exact test through the use
#'   of Phipson-Smyth estimate of the p-value or an approximate test through a
#'   Monte-Carlo estimate of the p-value. Default is `"exact"`.
#' @param seed An integer specifying the seed of the random generator useful for
#'   result reproducibility or method comparisons. Default is `NULL`.
#'
#' @return A \code{\link[base]{list}} with three components: the value of the
#'   statistic for the original two samples, the p-value of the resulting
#'   permutation test and a numeric vector storing the values of the permuted
#'   statistics.
#' @export
#'
#' @examples
#' n <- 10L
#' mx <- 0
#' sigma <- 1
#'
#' # Two different models for the two populations
#' x <- rnorm(n = n, mean = mx, sd = sigma)
#' delta <- 10
#' my <- mx + delta
#' y <- rnorm(n = n, mean = my, sd = sigma)
#' t1 <- two_sample_test(x, y)
#' t1$pvalue
#'
#' # Same model for the two populations
#' x <- rnorm(n = n, mean = mx, sd = sigma)
#' delta <- 0
#' my <- mx + delta
#' y <- rnorm(n = n, mean = my, sd = sigma)
#' t2 <- two_sample_test(x, y)
#' t2$pvalue
two_sample_test <- function(x, y = NULL,
                            stats = stat_t,
                            B = 1000L,
                            M = NULL,
                            alternative = "two_tail",
                            combine_with = "tippett",
                            type = "exact",
                            seed = NULL) {

  if (is.null(y) && !inherits(x, "dist"))
    abort("The x argument should be a dist object if the y argument is not supplied.")

  if (!is.null(seed)) withr::local_seed(seed)

  l <- convert_to_list(x, y)
  x <- l[[1]]
  y <- l[[2]]
  n1 <- length(x)
  n2 <- length(y)
  n <- n1 + n2
  stat_data <- c(x, y)

  # Compute total number of permutations yielding to distinct values of the test
  # statistic
  if (is.null(M)) M <- choose(n, n1) - 1

  # Generate permutation data
  if (M <= B) {
    B <- M
    perm_data <- utils::combn(n, n1)[, 1:B]
  } else {
    perm_data <- replicate(B, sample.int(n))[1:n1, ]
  }
  perm_data <- cbind(
    seq_len(nrow(perm_data)),
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

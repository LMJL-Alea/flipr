#' Two-Sample Permutation Test
#'
#' This function carries out an hypothesis test where the null hypothesis is
#' that the two samples are ruled by the same underlying generative probability
#' distribution against the alternative hypothesis that they are ruled by two
#' separate generative probability distributions.
#'
#' @section User-supplied statistic function:
#' A user-specified function should have at least two arguments:
#'
#' - the first argument is `data` which should be a list of the `n1 + n2`
#' concatenated observations with the original `n1` observations from the first
#' sample on top and the original `n2` observations from the second sample
#' below;
#' - the second argument is `indices` which should be an integer vector giving
#' the indices in `data` that are considered to belong to the first sample.
#'
#' See the \code{\link{stat_hotelling}} function for an example.
#'
#' @param x A list or matrix representing the 1st sample.
#' @param y A list or matrix representing the 2nd sample.
#' @param statistic A character vector specifying the chosen test statistic(s).
#'   These can be \code{\link{stat_hotelling}} or user-specified functions that
#'   define desired statistics. See the section *User-supplied statistic
#'   function* for more information on how these user-supplied functions should
#'   be structured for compatibility with the **flipr** framwork. Default is
#'   \code{\link{stat_hotelling}}.
#' @param B The number of sampled permutation. Default is `1000L`.
#' @param test A string specifying if performing an exact test through the use
#'   of Phipson-Smyth estimate of the p-value or an approximate test through a
#'   Monte-Carlo estimate of the p-value. Default is `"exact"`.
#' @param combining_function A string specifying the combining function to be
#'   used to compute the single test statistic value from the set of p-value
#'   estimates obtained during the non-parametric combination testing procedure.
#'   Default is `"tippett"`, which picks Tippett's function.
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
two_sample_test <- function(x, y,
                            statistic = stat_hotelling,
                            B = 1000L,
                            test = "exact",
                            combining_function = "tippett",
                            seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  l <- convert_to_list(x, y)
  x <- l[[1]]
  y <- l[[2]]

  n1 <- length(x)
  n2 <- length(y)
  n <- n1 + n2
  stat_data <- c(x, y)

  npc <- length(statistic) > 1

  M <- choose(n, n1)
  if (n1 == n2)
    M <- M / 2

  test <- match.arg(test, c("approximate", "exact"))
  if (test == "approximate" & M <= B) {
    B <- M
    group1_perm <- utils::combn(n, n1)[, 1:B]
  } else
    group1_perm <- replicate(B, sample.int(n))[1:n1, ]

  if (!npc)
    Tp <- sapply(
      X = 0:B,
      FUN = get_permuted_statistic,
      indices1 = group1_perm,
      stat_data = stat_data,
      statistic = statistic
    )
  else {
    Tp <- statistic %>%
      purrr::map(~ sapply(
        X = 0:B,
        FUN = get_permuted_statistic,
        indices1 = group1_perm,
        stat_data = stat_data,
        statistic = .
      )) %>%
      purrr::map(~ sapply(
        X = 1:(B+1),
        FUN = stats2pvalue,
        Tp = .,
        test = "approximate",
        B = B,
        M = M
      )) %>%
      purrr::transpose() %>%
      purrr::simplify_all() %>%
      purrr::map_dbl(combine_pvalues, method = combining_function)
  }

  list(
    statistic = Tp[1],
    pvalue = stats2pvalue(1, Tp, test, B, M),
    permuted_statistics = Tp[-1]
  )
}

get_permuted_statistic <- function(i, indices1, stat_data, statistic) {
  if (i == 0)
    indices <- seq_len(nrow(indices1))
  else
    indices <- indices1[, i]

  rlang::as_function(statistic)(stat_data, indices)
}

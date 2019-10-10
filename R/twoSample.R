#' Two-Sample Permutation Test
#'
#' This function carries out an hypothesis test where the null hypothesis is
#' that the two samples are ruled by the same underyling generative probability
#' distributions against the alternative hypothesis that they are ruled by two
#' separate generative probability distributions.
#'
#' @param x A list or matrix representing the 1st sample.
#' @param y A list or matrix representing the 2nd sample.
#' @param statistic A string specifying the chosen test statistic(s), among:
#'   \code{"hotelling"} [default].
#' @param B The number of sampled permutation (default: \code{1000L}).
#' @param alpha The significance level (default: \code{0.05}).
#' @param test A character string specifying if performing an exact test through
#'   the use of Phipson-Smyth estimate of the p-value or an approximate test
#'   through a Monte-Carlo estimate of the p-value (default: \code{"exact"}).
#' @param seed An integer for specifying the seed of the random generator for
#'   result reproducibility or method comparisons (default: \code{NULL}).
#'
#' @return A \code{\link[base]{list}} with three components: the value of the
#'   statistic for the original two samples, the p-value of the resulting
#'   permutation test and a numeric vector storing the values of the permuted
#'   statistics.
#' @export
#'
#' @examples
#' n <- 10L
#' p <- 3L
#' mx <- rep(0, p)
#' sigma <- diag(1, p)
#'
#' # Two different models for the two populations
#' x <- mvnfast::rmvn(n = n, mu = mx, sigma = sigma)
#' delta <- 10
#' my <- mx + delta
#' y <- mvnfast::rmvn(n = n, mu = my, sigma = sigma)
#' t1 <- two_sample_test(x, y)
#' t1$pvalue
#'
#' # Same model for the two populations
#' x <- mvnfast::rmvn(n = n, mu = mx, sigma = sigma)
#' delta <- 0
#' my <- mx + delta
#' y <- mvnfast::rmvn(n = n, mu = my, sigma = sigma)
#' t2 <- two_sample_test(x, y)
#' t2$pvalue
two_sample_test <- function(x, y,
                            statistic = "hotelling",
                            B = 1000L,
                            alpha = 0.05,
                            test = "exact",
                            seed = NULL) {

  set.seed(seed)

  if (is.list(x)) {
    stopifnot(is.list(y))
    n1 <- length(x)
    n2 <- length(y)
    d <- 0 # TO BE DONE
  } else if (is.matrix(x)) {
    stopifnot(is.matrix(y))
    n1 <- nrow(x)
    n2 <- nrow(y)
    d <- rbind(x, y)
  }

  n <- n1 + n2

  npc <- length(statistic) > 1

  M <- choose(n, n1)
  if (n1 == n2)
    M <- M / 2

  test <- match.arg(test, c("approximate", "exact"))
  if (test == "approximate" & M <= B) {
    B <- M
    group1.perm <- utils::combn(n, n1)[, 1:B]
  } else
    group1.perm <- replicate(B, sample.int(n))[1:n1, ]

  if (!npc)
    Tp <- sapply(0:B, get_permuted_statistic, indices1 = group1.perm, d = d, statistic = statistic)
  else {
    Tp <- statistic %>%
      purrr::map(~ sapply(0:B, get_permuted_statistic, indices1 = group1.perm, d = d, statistic = .)) %>%
      purrr::map(~ sapply(1:(B+1), stats2pvalue, Tp = ., test = "approximate", B = B, M = M)) %>%
      purrr::transpose() %>%
      purrr::simplify_all() %>%
      purrr::map_dbl(combine_pvalues)
  }

  list(
    statistic = Tp[1],
    pvalue = stats2pvalue(1, Tp, test, B, M),
    permuted_statistics = Tp[-1]
  )
}

stats2pvalue <- function(i, Tp, test = "exact", B, M) {
  T0 <- Tp[i]
  b <- sum(Tp >= T0) - 1
  if (test == "approximate") return(b / B)
  phipson_smyth_pvalue(b, B, M)
}

combine_pvalues <- function(p, method = "tippett") {
  switch (
    method,
    tippett = 1 - min(p),
    fisher = - 2 * sum(log(p))
  )
}

phipson_smyth_pvalue <- function(b, B, M) {
  if (M <= 10000) {
    pt <- seq_len(M) / M
    return(mean(stats::pbinom(q = b, size = B, prob = pt)))
  }

  corr <- stats::integrate(stats::pbinom, 0, 0.5 / M, q = b, size = B)$value
  (b + 1) / (B + 1) - corr
}

get_permuted_statistic <- function(i, indices1, d, statistic) {
  if (i == 0)
    indices <- seq_len(nrow(indices1))
  else
    indices <- indices1[, i]

  rlang::as_function(statistic)(d, indices)
}

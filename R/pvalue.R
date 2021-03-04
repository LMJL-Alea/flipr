stats2pvalue <- function(i, Tp, B, M, type = "exact", alternative = "right_tail") {
  available_types <- c("approximate", "exact")
  type <- match.arg(type, available_types)
  available_alternatives <- c("left_tail", "right_tail", "two_tail")
  alternative <- match.arg(alternative, available_alternatives)
  T0 <- Tp[i]
  b <- switch (alternative,
    right_tail = {
      sum(Tp >= T0) - 1
    },
    left_tail = {
      sum(Tp <= T0) - 1
    },
    two_tail = {
      2 * (min(sum(Tp >= T0), sum(Tp <= T0)) - 1)
    }
  )
  if (type == "approximate") return(b / B)
  phipson_smyth_pvalue(b, B, M)
}

phipson_smyth_pvalue <- function(b, B, M) {
  if (M <= 10000) {
    pt <- seq_len(M + 1) / (M + 1)
    return(mean(stats::pbinom(q = b, size = B, prob = pt)))
  }

  corr <- stats::integrate(stats::pbinom, 0, 0.5 / (M + 1), q = b, size = B)$value
  (b + 1) / (B + 1) - corr
}

combine_pvalues <- function(p, combine_with = "tippett") {
  available_combine_withs <- c("fisher", "tippett")
  combine_with <- match.arg(combine_with, available_combine_withs)
  switch (combine_with,
          tippett = 1 - min(p),
          fisher = - 2 * sum(log(p))
  )
}

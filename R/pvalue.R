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
    # pt <- seq_len(M) / M
    pt <- seq_len(M + 1) / (M + 1)
    return(mean(stats::pbinom(q = b, size = B, prob = pt)))
  }

  # corr <- stats::integrate(stats::pbinom, 0, 0.5 / M, q = b, size = B)$value
  corr <- stats::integrate(stats::pbinom, 0, 0.5 / (M + 1), q = b, size = B)$value
  (b + 1) / (B + 1) - corr
}

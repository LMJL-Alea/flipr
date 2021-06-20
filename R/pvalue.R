stats2pvalue <- function(i, Tp, M, type = "exact", alternative = "right_tail") {
  available_types <- c("approximate", "exact")
  type <- match.arg(type, available_types)
  available_alternatives <- c("left_tail", "right_tail", "two_tail")
  alternative <- match.arg(alternative, available_alternatives)
  T0 <- Tp[i]
  Tp <- unique(Tp)
  B <- length(Tp) - 1
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
  if (type == "approximate") return((b + 1) / (B + 1))
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

run_permutation_scheme <- function(type,
                                   alternative,
                                   stats,
                                   B,
                                   perm_data,
                                   stat_data,
                                   M,
                                   combine_with) {
  type <- match.arg(type, c("approximate", "exact"))
  alternative <- match.arg(alternative, c("left_tail", "right_tail", "two_tail"))
  nstats <- length(stats)
  npc <- nstats > 1
  altern <- if (npc) "right_tail" else alternative
  if (length(alternative) == 1)
    alternative <- rep(alternative, nstats)

  if (!npc) {
    Tp <- sapply(
      X = 0:B,
      FUN = get_permuted_statistic,
      perm_data = perm_data,
      stat_data = stat_data,
      stat_fun = stats[[1]]
    )
  }
  else {
    Tp <- stats %>%
      purrr::map(~ sapply(
        X = 0:B,
        FUN = get_permuted_statistic,
        perm_data = perm_data,
        stat_data = stat_data,
        stat_fun = .
      )) %>%
      purrr::map2(alternative, ~ sapply(
        X = 1:(B+1),
        FUN = stats2pvalue,
        Tp = .x,
        M = M,
        type = "approximate",
        alternative = .y
      )) %>%
      purrr::transpose() %>%
      purrr::simplify_all() %>%
      purrr::map_dbl(combine_pvalues, combine_with = combine_with)
  }

  list(
    statistic = Tp[1],
    pvalue = stats2pvalue(1, Tp, M, type = type, alternative = altern),
    permuted_statistics = Tp[-1]
  )
}

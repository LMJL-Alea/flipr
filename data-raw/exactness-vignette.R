## code to prepare `flipr-vignette` dataset goes here

library(tidyverse)
library(parallel)

# General setup
nreps <- 1e4
n1 <- 5
n2 <- 5
set.seed(12345)
sim <- map(sample.int(.Machine$integer.max, nreps, replace = TRUE), ~ {
    list(
      x = rnorm(n = n1, mean = 0, sd = 1),
      y = rnorm(n = n2, mean = 0, sd = 1),
      s = .x
    )
  })

# Cluster setup
cl <- makeCluster(detectCores(logical = FALSE))
clusterEvalQ(cl, {
  library(tidyverse)
  library(flipr)
  null_spec <- function(y, parameters) {
    map(y, ~ .x - parameters)
  }
  stat_functions <- list(stat_t)
  stat_assignments <- list(delta = 1)
  nperms <- 20
  alpha <- 0.05
})

alpha_estimates <- pbapply::pblapply(sim, function(.l) {
    pf <- PlausibilityFunction$new(
      null_spec = null_spec,
      stat_functions = stat_functions,
      stat_assignments = stat_assignments,
      .l$x, .l$y,
      seed = .l$s
    )
    pf$set_nperms(nperms)
    pf$set_alternative("left_tail")
    pf$set_pvalue_formula("exact")
    pv_exact <- pf$get_value(0)
    pf$set_pvalue_formula("upper_bound")
    pv_upper_bound <- pf$get_value(0)
    pf$set_pvalue_formula("estimate")
    pv_estimate <- pf$get_value(0)
    c(
      exact       = pv_exact       <= alpha,
      upper_bound = pv_upper_bound <= alpha,
      estimate    = pv_estimate    <= alpha
    )
  }, cl = cl) %>%
  transpose() %>%
  simplify_all() %>%
  map(mean)
stopCluster(cl)
alpha_estimates

saveRDS(alpha_estimates, "data-raw/alpha.rds")

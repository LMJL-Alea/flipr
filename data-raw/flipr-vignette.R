## code to prepare `flipr-vignette` dataset goes here

# Setup -------------------------------------------------------------------

library(tidyverse)
library(flipr)

generate_grid <- function(center_value, min_value, max_value, n) {
  stopifnot(center_value > min_value && center_value < max_value)
  c(
    seq(min_value, center_value, len = n / 2 + 1)[1:(n / 2)],
    center_value,
    seq(center_value, max_value, len = n / 2 + 1)[-1]
  )
}

n1 <- 10
n2 <- 10
mu1 <- 1
mu2 <- 4
sd1 <- 1
sd2 <- 1
B <- 100000
null_spec <- function(y, parameters) {y - parameters[1]}

# Scenario A --------------------------------------------------------------

set.seed(1234)
a1 <- rnorm(n1, mean = mu1, sd = sd1)
a2 <- rnorm(n2, mean = mu2, sd = sd2)
delta_pe <- mean(a2) - mean(a1)

dfa <- tibble(
  delta = generate_grid(delta_pe, delta_pe - 2, delta_pe + 2, 20),
  pvalue = delta %>%
    two_sample_pf(
      null_specification = null_spec,
      x = a1,
      y = a2,
      statistic = stat_t,
      B = B,
      seed = 1234,
      alternative = "two_tail"
    )
) %>%
  mutate(
    pvalue_alt = delta %>%
      map_dbl(~ {
        t.test(
          x = a2,
          y = a1,
          alternative = "two.sided",
          mu = .x,
          var.equal = TRUE
        )$p.value
      })
  ) %>%
  select(
    delta,
    `Parametric Approach` = pvalue_alt,
    `Permutation Approach` = pvalue
  ) %>%
  pivot_longer(-delta)

saveRDS(dfa, "data-raw/dfa.rds")

# Scenario B --------------------------------------------------------------

set.seed(1234)
b1 <- rgamma(n1, shape = 1, rate = 1)
b2 <- rgamma(n2, shape = 16, rate = 4)
deltb_pe <- mean(b2) - mean(b1)

dfb <- tibble(
  delta = generate_grid(deltb_pe, deltb_pe - 2, deltb_pe + 2, 20),
  pvalue = delta %>%
    two_sample_pf(
      null_specification = null_spec,
      x = b1,
      y = b2,
      statistic = stat_t,
      B = B,
      seed = 1234,
      alternative = "two_tail"
    )
) %>%
  mutate(
    pvalue_alt = delta %>%
      map_dbl(~ {
        t.test(
          x = b2,
          y = b1,
          alternative = "two.sided",
          mu = .x,
          var.equal = TRUE
        )$p.value
      })
  ) %>%
  select(
    delta,
    Parametric = pvalue_alt,
    Permutation = pvalue
  ) %>%
  pivot_longer(-delta)

saveRDS(dfb, "data-raw/dfb.rds")

# Scenario C --------------------------------------------------------------

set.seed(1234)
c1 <- rnorm(n1, mean = mu1, sd = sd1)
c2 <- rgamma(n2, shape = 16, rate = 4)
deltc_pe <- mean(c2) - mean(c1)

dfc <- tibble(
  delta = generate_grid(deltc_pe, deltc_pe - 2, deltc_pe + 2, 20),
  pvalue = delta %>%
    two_sample_pf(
      null_specification = null_spec,
      x = c1,
      y = c2,
      statistic = stat_t,
      B = B,
      seed = 1234,
      alternative = "two_tail"
    )
) %>%
  mutate(
    pvalue_alt = delta %>%
      map_dbl(~ {
        t.test(
          x = c2,
          y = c1,
          alternative = "two.sided",
          mu = .x,
          var.equal = TRUE
        )$p.value
      })
  ) %>%
  select(
    delta,
    Parametric = pvalue_alt,
    Permutation = pvalue
  ) %>%
  pivot_longer(-delta)

saveRDS(dfc, "data-raw/dfc.rds")

dfa <- readRDS("data-raw/dfa.rds")
dfb <- readRDS("data-raw/dfb.rds")
dfc <- readRDS("data-raw/dfc.rds")
df_mean <- readRDS("data-raw/df_mean.rds")
df_sd <- readRDS("data-raw/df_sd.rds")
df_fisher <- readRDS("data-raw/df_fisher.rds")
df_tippett <- readRDS("data-raw/df_tippett.rds")
usethis::use_data(
  dfa, dfb, dfc,
  df_mean, df_sd,
  df_fisher, df_tippett,
  overwrite = TRUE,
  internal = TRUE
)

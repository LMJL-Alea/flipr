alpha_estimates <- readRDS("data-raw/alpha.rds")
pfa <- readRDS("data-raw/pfa.rds")
pfb <- readRDS("data-raw/pfb.rds")
pfc <- readRDS("data-raw/pfc.rds")
df_mean <- readRDS("data-raw/df_mean.rds")
df_sd <- readRDS("data-raw/df_sd.rds")
df_fisher <- readRDS("data-raw/df_fisher.rds")
df_tippett <- readRDS("data-raw/df_tippett.rds")
usethis::use_data(
  alpha_estimates,
  pfa, pfb, pfc,
  df_mean, df_sd,
  df_fisher, df_tippett,
  overwrite = TRUE,
  internal = TRUE
)

dfa <- readRDS("data-raw/dfa.rds")
dfb <- readRDS("data-raw/dfb.rds")
dfc <- readRDS("data-raw/dfc.rds")
df_mean <- readRDS("data-raw/df_mean.rds")
df_sd <- readRDS("data-raw/df_sd.rds")
pvalue_fisher <- readRDS("data-raw/pvalue_fisher.rds")
pvalue_tippett <- readRDS("data-raw/pvalue_tippett.rds")
usethis::use_data(
  dfa, dfb, dfc,
  df_mean, df_sd, pvalue_fisher, pvalue_tippett,
  overwrite = TRUE, internal = TRUE
)

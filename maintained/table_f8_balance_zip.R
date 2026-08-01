# coppock_green_porter_2022/maintained/table_f8_balance_zip.R
# Output: output/table_f8_balance_zip.csv, output/table_f8_balance_zip.tex,
#   output/table_f8_balance_zip_ftest.csv
# Depends on: CGP_2022_zip_code_level.rds, helpers.R
# Description: Reproduces appendix Table F.8, the ZIP-level balance regression, and the
#   joint F test the appendix text reports alongside it.
#
# As in Table E.7, the published row labels read "Two Party Vote Share" where the model
# uses the two-party vote margin. The labels below name the variables in the model.

source(here::here("maintained", "helpers.R"))

zip_code_level <- read_rds(
  here::here("original", "replication_archive", "CGP_2022_zip_code_level.rds")
)

fit_balance <- lm_robust(
  Z ~ dem_two_party_vote_margin_2016_nona +
    dem_two_party_vote_margin_2014_nona +
    dem_two_party_vote_margin_2012_nona,
  data = zip_code_level
)

results <- tidy(fit_balance)

write_csv(results, here::here("maintained", "output", "table_f8_balance_zip.csv"))

f_test <- glance(fit_balance) |>
  select(r.squared, statistic, p.value, df.residual, nobs)

write_csv(f_test, here::here("maintained", "output", "table_f8_balance_zip_ftest.csv"))

coef_labels <- c(
  "(Intercept)" = "Intercept",
  "dem_two_party_vote_margin_2016_nona" = "Two Party Vote Margin (2016)",
  "dem_two_party_vote_margin_2014_nona" = "Two Party Vote Margin (2014)",
  "dem_two_party_vote_margin_2012_nona" = "Two Party Vote Margin (2012)"
)

modelsummary(
  list("Model 1" = fit_balance),
  output = here::here("maintained", "output", "table_f8_balance_zip.tex"),
  coef_map = coef_labels,
  gof_map = gof_map_lm(3),
  fmt = 3,
  stars = c("*" = 0.05),
  title = "Table F.8: Experimental balance (ZIP code level)",
  notes = "HC2 robust standard errors are in parentheses."
)

print(results)
print(f_test)

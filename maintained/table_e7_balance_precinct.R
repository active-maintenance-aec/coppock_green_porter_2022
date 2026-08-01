# coppock_green_porter_2022/maintained/table_e7_balance_precinct.R
# Output: output/table_e7_balance_precinct.csv, output/table_e7_balance_precinct.tex,
#   output/table_e7_balance_precinct_ftest.csv
# Depends on: CGP_2022_precinct_level.rds, helpers.R
# Description: Reproduces appendix Table E.7, the precinct-level balance regression of
#   the treatment indicator on pre-treatment covariates, and the joint F test the
#   appendix text reports alongside it.
#
# The published table labels these regressors "Two Party Vote Share"; the model in the
# archive uses the two-party vote margin. The labels below name the variables that are
# actually in the model. See the errata section of the report.

source(here::here("maintained", "helpers.R"))

precinct_level <- read_rds(
  here::here("original", "replication_archive", "CGP_2022_precinct_level.rds")
)

fit_balance <- lm_robust(
  Z ~ dem_two_party_vote_margin_2016_nona + vote_2016_missing +
    dem_two_party_vote_margin_2014_nona + vote_2014_missing +
    dem_two_party_vote_margin_2012_nona + vote_2012_missing,
  clusters = vb_tsmart_zip,
  data = precinct_level
)

results <- tidy(fit_balance)

write_csv(results, here::here("maintained", "output", "table_e7_balance_precinct.csv"))

f_test <- glance(fit_balance) |>
  select(r.squared, statistic, p.value, df.residual, nobs) |>
  mutate(nclusters = fit_balance$nclusters)

write_csv(f_test, here::here("maintained", "output", "table_e7_balance_precinct_ftest.csv"))

coef_labels <- c(
  "(Intercept)" = "Intercept",
  "dem_two_party_vote_margin_2016_nona" = "Two Party Vote Margin (2016)",
  "vote_2016_missingTRUE" = "Missingness Indicator (2016)",
  "dem_two_party_vote_margin_2014_nona" = "Two Party Vote Margin (2014)",
  "vote_2014_missingTRUE" = "Missingness Indicator (2014)",
  "dem_two_party_vote_margin_2012_nona" = "Two Party Vote Margin (2012)",
  "vote_2012_missingTRUE" = "Missingness Indicator (2012)"
)

modelsummary(
  list("Model 1" = fit_balance),
  output = here::here("maintained", "output", "table_e7_balance_precinct.tex"),
  coef_map = coef_labels,
  gof_map = gof_map_lm(3),
  fmt = 3,
  stars = c("*" = 0.05),
  title = "Table E.7: Experimental balance",
  notes = "CR2 cluster-robust standard errors, clustered by ZIP code, are in parentheses."
)

print(results)
print(f_test)

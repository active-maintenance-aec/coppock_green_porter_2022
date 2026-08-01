# coppock_green_porter_2022/maintained/table_a2_turnout_precinct.R
# Output: output/table_a2_turnout_precinct.csv, output/table_a2_turnout_precinct.tex
# Depends on: CGP_2022_precinct_level.rds, helpers.R
# Description: Reproduces appendix Table A.2, the effects of the advertisements on
#   precinct-level turnout.

source(here::here("maintained", "helpers.R"))

precinct_level <- read_rds(
  here::here("original", "replication_archive", "CGP_2022_precinct_level.rds")
)

fit_1 <- lm_robust(
  vote_total_2018 ~ Z,
  clusters = vb_tsmart_zip,
  data = precinct_level
)

fit_2 <- lm_robust(
  vote_total_2018 ~ Z +
    vote_total_2016_nona + vote_2016_missing +
    vote_total_2014_nona + vote_2014_missing +
    vote_total_2012_nona + vote_2012_missing,
  clusters = vb_tsmart_zip,
  data = precinct_level
)

fit_3 <- lm_robust(
  vote_total_2018 ~ treatment,
  clusters = vb_tsmart_zip,
  data = precinct_level
)

fit_4 <- lm_robust(
  vote_total_2018 ~ treatment +
    vote_total_2016_nona + vote_2016_missing +
    vote_total_2014_nona + vote_2014_missing +
    vote_total_2012_nona + vote_2012_missing,
  clusters = vb_tsmart_zip,
  data = precinct_level
)

fits <- list(
  "Model 1" = fit_1, "Model 2" = fit_2, "Model 3" = fit_3, "Model 4" = fit_4
)

results <- fits |>
  map(tidy) |>
  list_rbind(names_to = "model")

write_csv(results, here::here("maintained", "output", "table_a2_turnout_precinct.csv"))

coef_labels <- c(
  "Z" = "Any Treatment Video",
  "treatmentvideo_1" = "Treatment Video 1",
  "treatmentvideo_2" = "Treatment Video 2",
  "vote_total_2016_nona" = "Two Party Vote Total (2016)",
  "vote_2016_missingTRUE" = "Missingness Indicator (2016)",
  "vote_total_2014_nona" = "Two Party Vote Total (2014)",
  "vote_2014_missingTRUE" = "Missingness Indicator (2014)",
  "vote_total_2012_nona" = "Two Party Vote Total (2012)",
  "vote_2012_missingTRUE" = "Missingness Indicator (2012)",
  "(Intercept)" = "Intercept"
)

modelsummary(
  fits,
  output = here::here("maintained", "output", "table_a2_turnout_precinct.tex"),
  coef_map = coef_labels,
  gof_map = gof_map_lm(2),
  fmt = 2,
  stars = c("*" = 0.05),
  title = "Table A.2: Effects on turnout",
  notes = "CR2 cluster-robust standard errors, clustered by ZIP code, are in parentheses."
)

print(results |> filter(term %in% c("Z", "treatmentvideo_1", "treatmentvideo_2")))

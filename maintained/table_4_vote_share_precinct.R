# coppock_green_porter_2022/maintained/table_4_vote_share_precinct.R
# Output: output/table_4_vote_share_precinct.csv, output/table_4_vote_share_precinct.tex
# Depends on: CGP_2022_precinct_level.rds, helpers.R
# Description: Reproduces Table 4, the effects of the advertisements on Democratic
#   two-party vote share at the precinct level, with CR2 standard errors clustered
#   by ZIP code.

source(here::here("maintained", "helpers.R"))

precinct_level <- read_rds(
  here::here("original", "replication_archive", "CGP_2022_precinct_level.rds")
)

# Model 1: unadjusted, binary treatment
fit_1 <- lm_robust(
  dem_two_party_share_2018 ~ Z,
  clusters = vb_tsmart_zip,
  data = precinct_level
)

# Model 2: covariate adjusted, binary treatment
fit_2 <- lm_robust(
  dem_two_party_share_2018 ~ Z +
    dem_two_party_share_2016_nona + vote_2016_missing +
    dem_two_party_share_2014_nona + vote_2014_missing +
    dem_two_party_share_2012_nona + vote_2012_missing,
  clusters = vb_tsmart_zip,
  data = precinct_level
)

# Model 3: unadjusted, one coefficient per video
fit_3 <- lm_robust(
  dem_two_party_share_2018 ~ treatment,
  clusters = vb_tsmart_zip,
  data = precinct_level
)

# Model 4: covariate adjusted, one coefficient per video
fit_4 <- lm_robust(
  dem_two_party_share_2018 ~ treatment +
    dem_two_party_share_2016_nona + vote_2016_missing +
    dem_two_party_share_2014_nona + vote_2014_missing +
    dem_two_party_share_2012_nona + vote_2012_missing,
  clusters = vb_tsmart_zip,
  data = precinct_level
)

fits <- list(
  "Model 1" = fit_1, "Model 2" = fit_2, "Model 3" = fit_3, "Model 4" = fit_4
)

results <- fits |>
  map(tidy) |>
  list_rbind(names_to = "model")

write_csv(results, here::here("maintained", "output", "table_4_vote_share_precinct.csv"))

coef_labels <- c(
  "Z" = "Any Treatment Video",
  "treatmentvideo_1" = "Treatment Video 1",
  "treatmentvideo_2" = "Treatment Video 2",
  "dem_two_party_share_2016_nona" = "Two Party Vote Share (2016)",
  "vote_2016_missingTRUE" = "Missingness Indicator (2016)",
  "dem_two_party_share_2014_nona" = "Two Party Vote Share (2014)",
  "vote_2014_missingTRUE" = "Missingness Indicator (2014)",
  "dem_two_party_share_2012_nona" = "Two Party Vote Share (2012)",
  "vote_2012_missingTRUE" = "Missingness Indicator (2012)",
  "(Intercept)" = "Intercept"
)

modelsummary(
  fits,
  output = here::here("maintained", "output", "table_4_vote_share_precinct.tex"),
  coef_map = coef_labels,
  gof_map = gof_map_lm(4),
  fmt = 4,
  stars = c("*" = 0.05),
  title = "Table 4: Effects on vote share",
  notes = "CR2 cluster-robust standard errors, clustered by ZIP code, are in parentheses."
)

print(results |> filter(term %in% c("Z", "treatmentvideo_1", "treatmentvideo_2")))

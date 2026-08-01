# coppock_green_porter_2022/maintained/table_f9_vote_share_zip.R
# Output: output/table_f9_vote_share_zip.csv, output/table_f9_vote_share_zip.tex
# Depends on: CGP_2022_zip_code_level.rds, helpers.R
# Description: Reproduces appendix Table F.9, the vote share models estimated on all 210
#   randomized ZIP codes after apportioning precinct votes across ZIP boundaries.
#
# The published table's first covariate row is labelled "Missingness Indicator (2016)"
# but carries the coefficient on the 2016 two-party vote share; the archive passed
# texreg a coefficient-name vector that is one label short. The labels below are correct.

source(here::here("maintained", "helpers.R"))

zip_code_level <- read_rds(
  here::here("original", "replication_archive", "CGP_2022_zip_code_level.rds")
)

fit_1 <- lm_robust(dem_two_party_share_2018 ~ Z, data = zip_code_level)

fit_2 <- lm_robust(
  dem_two_party_share_2018 ~ Z +
    dem_two_party_share_2016_nona +
    dem_two_party_share_2014_nona + vote_2014_missing +
    dem_two_party_share_2012_nona + vote_2012_missing,
  data = zip_code_level
)

fit_3 <- lm_robust(dem_two_party_share_2018 ~ treatment, data = zip_code_level)

fit_4 <- lm_robust(
  dem_two_party_share_2018 ~ treatment +
    dem_two_party_share_2016_nona +
    dem_two_party_share_2014_nona + vote_2014_missing +
    dem_two_party_share_2012_nona + vote_2012_missing,
  data = zip_code_level
)

fits <- list(
  "Model 1" = fit_1, "Model 2" = fit_2, "Model 3" = fit_3, "Model 4" = fit_4
)

results <- fits |>
  map(tidy) |>
  list_rbind(names_to = "model")

write_csv(results, here::here("maintained", "output", "table_f9_vote_share_zip.csv"))

coef_labels <- c(
  "Z" = "Any Treatment Video",
  "treatmentvideo_1" = "Treatment Video 1",
  "treatmentvideo_2" = "Treatment Video 2",
  "dem_two_party_share_2016_nona" = "Two Party Vote Share (2016)",
  "dem_two_party_share_2014_nona" = "Two Party Vote Share (2014)",
  "vote_2014_missingTRUE" = "Missingness Indicator (2014)",
  "dem_two_party_share_2012_nona" = "Two Party Vote Share (2012)",
  "vote_2012_missingTRUE" = "Missingness Indicator (2012)",
  "(Intercept)" = "Intercept"
)

modelsummary(
  fits,
  output = here::here("maintained", "output", "table_f9_vote_share_zip.tex"),
  coef_map = coef_labels,
  gof_map = gof_map_lm(4),
  fmt = 4,
  stars = c("*" = 0.05),
  title = "Table F.9: Effects on vote share (ZIP code level)",
  notes = "HC2 robust standard errors are in parentheses."
)

print(results |> filter(term %in% c("Z", "treatmentvideo_1", "treatmentvideo_2")))

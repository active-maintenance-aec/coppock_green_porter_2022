# coppock_green_porter_2022/maintained/table_b4_vote_margin_precinct_cd.R
# Output: output/table_b4_vote_margin_precinct_cd.csv, output/table_b4_vote_margin_precinct_cd.tex
# Depends on: CGP_2022_precinct_level.rds, helpers.R
# Description: Reproduces appendix Table B.4, the vote margin models with fixed effects
#   for congressional district.

source(here::here("maintained", "helpers.R"))

precinct_level <- read_rds(
  here::here("original", "replication_archive", "CGP_2022_precinct_level.rds")
)

fit_1 <- lm_robust(
  dem_two_party_vote_margin_2018 ~ Z + cd_jumble,
  clusters = vb_tsmart_zip,
  data = precinct_level
)

fit_2 <- lm_robust(
  dem_two_party_vote_margin_2018 ~ Z +
    dem_two_party_vote_margin_2016_nona + vote_2016_missing +
    dem_two_party_vote_margin_2014_nona + vote_2014_missing +
    dem_two_party_vote_margin_2012_nona + vote_2012_missing +
    cd_jumble,
  clusters = vb_tsmart_zip,
  data = precinct_level
)

fit_3 <- lm_robust(
  dem_two_party_vote_margin_2018 ~ treatment + cd_jumble,
  clusters = vb_tsmart_zip,
  data = precinct_level
)

fit_4 <- lm_robust(
  dem_two_party_vote_margin_2018 ~ treatment +
    dem_two_party_vote_margin_2016_nona + vote_2016_missing +
    dem_two_party_vote_margin_2014_nona + vote_2014_missing +
    dem_two_party_vote_margin_2012_nona + vote_2012_missing +
    cd_jumble,
  clusters = vb_tsmart_zip,
  data = precinct_level
)

fits <- list(
  "Model 1" = fit_1, "Model 2" = fit_2, "Model 3" = fit_3, "Model 4" = fit_4
)

results <- fits |>
  map(tidy) |>
  list_rbind(names_to = "model")

write_csv(results, here::here("maintained", "output", "table_b4_vote_margin_precinct_cd.csv"))

coef_labels <- c(
  "Z" = "Any Treatment Video",
  "treatmentvideo_1" = "Treatment Video 1",
  "treatmentvideo_2" = "Treatment Video 2",
  "dem_two_party_vote_margin_2016_nona" = "Two Party Vote Margin (2016)",
  "vote_2016_missingTRUE" = "Missingness Indicator (2016)",
  "dem_two_party_vote_margin_2014_nona" = "Two Party Vote Margin (2014)",
  "vote_2014_missingTRUE" = "Missingness Indicator (2014)",
  "dem_two_party_vote_margin_2012_nona" = "Two Party Vote Margin (2012)",
  "vote_2012_missingTRUE" = "Missingness Indicator (2012)",
  "(Intercept)" = "Intercept"
)

modelsummary(
  fits,
  output = here::here("maintained", "output", "table_b4_vote_margin_precinct_cd.tex"),
  coef_map = coef_labels,
  gof_map = gof_map_lm(2),
  fmt = 2,
  stars = c("*" = 0.05),
  title = "Table B.4: Effects on vote margin (CD fixed effects)",
  notes = c("CR2 cluster-robust standard errors are in parentheses.",
            "All models include fixed effects for congressional district.")
)

print(results |> filter(term %in% c("Z", "treatmentvideo_1", "treatmentvideo_2")))

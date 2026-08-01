# coppock_green_porter_2022/maintained/table_c6_equivalence_tests.R
# Output: output/table_c6_equivalence_tests.csv, output/table_c6_equivalence_tests.tex
# Depends on: CGP_2022_precinct_level.rds, helpers.R
# Description: Reproduces appendix Table C.6, two one-sided equivalence tests for the
#   difference between the two treatment videos on each of the three outcomes.
#
# The archive computes the video_1 minus video_2 contrast with lh_robust(), which
# current estimatr refuses when clusters are supplied because it has no CR2 path for
# linear hypotheses. The contrast and its standard error are recovered here directly
# from the fitted model's coefficient vector and CR2 variance-covariance matrix, which
# is the same arithmetic lh_robust() performed.

source(here::here("maintained", "helpers.R"))

precinct_level <- read_rds(
  here::here("original", "replication_archive", "CGP_2022_precinct_level.rds")
)

# Two one-sided tests: the p-value is the larger of the two one-sided tail
# probabilities against the equivalence bounds.
eq_test <- function(estimate, std_error, tolerance) {
  p_lower <- pnorm((estimate + tolerance) / std_error, lower.tail = FALSE)
  p_upper <- pnorm((estimate - tolerance) / std_error, lower.tail = TRUE)
  max(p_lower, p_upper)
}

video_contrast <- function(fit) {
  b <- coef(fit)
  v <- vcov(fit)
  tibble(
    estimate = unname(b["treatmentvideo_1"] - b["treatmentvideo_2"]),
    std.error = sqrt(
      v["treatmentvideo_1", "treatmentvideo_1"] +
        v["treatmentvideo_2", "treatmentvideo_2"] -
        2 * v["treatmentvideo_1", "treatmentvideo_2"]
    )
  )
}

sds <- precinct_level |>
  pivot_longer(
    c(vote_total_2018, dem_two_party_share_2018, dem_two_party_vote_margin_2018),
    names_to = "outcome"
  ) |>
  group_by(outcome) |>
  summarise(sd_1.0 = sd(value, na.rm = TRUE), .groups = "drop") |>
  mutate(sd_0.2 = sd_1.0 * 0.2, sd_0.1 = sd_1.0 * 0.1)

fit_share_dim <- lm_robust(
  dem_two_party_share_2018 ~ treatment,
  clusters = vb_tsmart_zip, data = precinct_level
)

fit_share_ols <- lm_robust(
  dem_two_party_share_2018 ~ treatment +
    dem_two_party_share_2016_nona + vote_2016_missing +
    dem_two_party_share_2014_nona + vote_2014_missing +
    dem_two_party_share_2012_nona + vote_2012_missing,
  clusters = vb_tsmart_zip, data = precinct_level
)

fit_margin_dim <- lm_robust(
  dem_two_party_vote_margin_2018 ~ treatment,
  clusters = vb_tsmart_zip, data = precinct_level
)

fit_margin_ols <- lm_robust(
  dem_two_party_vote_margin_2018 ~ treatment +
    dem_two_party_vote_margin_2016_nona + vote_2016_missing +
    dem_two_party_vote_margin_2014_nona + vote_2014_missing +
    dem_two_party_vote_margin_2012_nona + vote_2012_missing,
  clusters = vb_tsmart_zip, data = precinct_level
)

fit_total_dim <- lm_robust(
  vote_total_2018 ~ treatment,
  clusters = vb_tsmart_zip, data = precinct_level
)

fit_total_ols <- lm_robust(
  vote_total_2018 ~ treatment +
    vote_total_2016_nona + vote_2016_missing +
    vote_total_2014_nona + vote_2014_missing +
    vote_total_2012_nona + vote_2012_missing,
  clusters = vb_tsmart_zip, data = precinct_level
)

results <- bind_rows(
  video_contrast(fit_share_dim) |>
    mutate(outcome = "dem_two_party_share_2018", estimator = "DIM"),
  video_contrast(fit_share_ols) |>
    mutate(outcome = "dem_two_party_share_2018", estimator = "OLS"),
  video_contrast(fit_margin_dim) |>
    mutate(outcome = "dem_two_party_vote_margin_2018", estimator = "DIM"),
  video_contrast(fit_margin_ols) |>
    mutate(outcome = "dem_two_party_vote_margin_2018", estimator = "OLS"),
  video_contrast(fit_total_dim) |>
    mutate(outcome = "vote_total_2018", estimator = "DIM"),
  video_contrast(fit_total_ols) |>
    mutate(outcome = "vote_total_2018", estimator = "OLS")
) |>
  left_join(sds, by = "outcome") |>
  rowwise() |>
  mutate(
    p_eq_0.2 = eq_test(estimate, std.error, tolerance = sd_0.2),
    p_eq_0.1 = eq_test(estimate, std.error, tolerance = sd_0.1)
  ) |>
  ungroup() |>
  mutate(
    outcome_name = recode_values(
      outcome,
      "dem_two_party_share_2018" ~ "Vote Share",
      "dem_two_party_vote_margin_2018" ~ "Vote Margin",
      "vote_total_2018" ~ "Vote Total"
    )
  ) |>
  select(outcome_name, sd_1.0, estimator, estimate, std.error, p_eq_0.2, p_eq_0.1)

write_csv(results, here::here("maintained", "output", "table_c6_equivalence_tests.csv"))

results |>
  kable("latex", booktabs = TRUE, digits = 3,
        col.names = c("Outcome", "SD", "Estimator", "Estimate", "SE",
                      "p (0.2SD)", "p (0.1SD)"),
        caption = "Table C.6: Equivalence tests") |>
  kable_styling(latex_options = "hold_position") |>
  write_lines(here::here("maintained", "output", "table_c6_equivalence_tests.tex"))

print(results)

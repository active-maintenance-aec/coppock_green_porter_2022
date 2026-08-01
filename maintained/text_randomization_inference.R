# coppock_green_porter_2022/maintained/text_randomization_inference.R
# Output: output/text_randomization_inference.csv
# Depends on: CGP_2022_precinct_level.rds, CGP_2022_zip_code_level.rds, helpers.R
# Description: Reproduces the one-tailed randomization inference p-values the main text
#   reports for Table 4, by re-running the study's blocked, clustered assignment under
#   the sharp null of no effect.
#
# Two departures from the archive. It shipped with sims = 500 and the line it was
# derived from, sims = 2000, commented out just above; 2000 draws take about half a
# minute here, so the published number of draws is used. And it set no seed, which is
# why the archive cannot return its own published p-values twice; a seed is set here.

source(here::here("maintained", "helpers.R"))

set.seed(20181106)

sims <- 2000

precinct_level <- read_rds(
  here::here("original", "replication_archive", "CGP_2022_precinct_level.rds")
)
zip_code_level <- read_rds(
  here::here("original", "replication_archive", "CGP_2022_zip_code_level.rds")
)

blocked_zips <- zip_code_level |>
  transmute(vb_tsmart_zip, block_id)

# One ZIP in each matched trio is assigned to treatment, and treated ZIPs are then split
# between the two videos within block.
assign_zips <- function(data) {
  new_randomization <- blocked_zips |>
    mutate(
      Zsim = block_ra(block_id),
      treatmentsim = as.character(block_ra(Zsim, conditions = c("video_1", "video_2"))),
      treatmentsim = if_else(Zsim == 1, treatmentsim, "control")
    )
  left_join(
    data,
    select(new_randomization, vb_tsmart_zip, Zsim, treatmentsim),
    by = "vb_tsmart_zip"
  )
}

design <-
  declare_model(data = precinct_level) +
  declare_assignment(handler = assign_zips) +
  declare_estimator(
    dem_two_party_share_2018 ~ Zsim,
    clusters = vb_tsmart_zip,
    label = "DIM binary"
  ) +
  declare_estimator(
    dem_two_party_share_2018 ~ Zsim +
      dem_two_party_share_2016_nona + vote_2016_missing +
      dem_two_party_share_2014_nona + vote_2014_missing +
      dem_two_party_share_2012_nona + vote_2012_missing,
    clusters = vb_tsmart_zip,
    label = "OLS binary"
  ) +
  declare_estimator(
    dem_two_party_share_2018 ~ treatmentsim,
    clusters = vb_tsmart_zip,
    term = c("treatmentsimvideo_1", "treatmentsimvideo_2"),
    label = "DIM three arm"
  ) +
  declare_estimator(
    dem_two_party_share_2018 ~ treatmentsim +
      dem_two_party_share_2016_nona + vote_2016_missing +
      dem_two_party_share_2014_nona + vote_2014_missing +
      dem_two_party_share_2012_nona + vote_2012_missing,
    clusters = vb_tsmart_zip,
    term = c("treatmentsimvideo_1", "treatmentsimvideo_2"),
    label = "OLS three arm"
  )

simulations <- simulate_design(design, sims = sims)

observed_estimates <- design |>
  get_estimates(
    data = rename(precinct_level, Zsim = Z, treatmentsim = treatment)
  ) |>
  transmute(estimator, term, estimate_obs = estimate)

results <- simulations |>
  left_join(observed_estimates, by = c("estimator", "term")) |>
  summarise(
    estimate_obs = unique(estimate_obs),
    p_upper = mean(estimate >= estimate_obs),
    p_two_tailed = mean(abs(estimate) >= abs(estimate_obs)),
    .by = c(estimator, term)
  ) |>
  mutate(sims = sims) |>
  arrange(estimator, term)

write_csv(results, here::here("maintained", "output", "text_randomization_inference.csv"))

print(results)

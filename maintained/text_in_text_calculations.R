# coppock_green_porter_2022/maintained/text_in_text_calculations.R
# Output: output/text_in_text_calculations.csv
# Depends on: CGP_2022_zip_code_level.rds, output/figure_1_bayesian_learning.csv, helpers.R
# Description: Computes the in-text quantities: the number of two-party votes cast in
#   treated ZIP codes, the two posterior means the discussion quotes, and the implied
#   cost per vote.
#
# The campaign's budget is a design fact the paper states in its description of the
# intervention ($30,000 of airtime per advertisement plus $40,000 of production), not an
# estimate, so it enters here as a constant. Everything else is read from the pipeline.

source(here::here("maintained", "helpers.R"))

zip_code_level <- read_rds(
  here::here("original", "replication_archive", "CGP_2022_zip_code_level.rds")
)

bayes_df <- read_csv(
  here::here("maintained", "output", "figure_1_bayesian_learning.csv"),
  show_col_types = FALSE
)

production_cost <- 40000
airtime_cost_per_ad <- 30000
n_ads <- 2
total_cost <- production_cost + airtime_cost_per_ad * n_ads

total_votes_treated <- zip_code_level |>
  filter(Z == 1) |>
  summarise(total = sum(vote_total_2018, na.rm = TRUE)) |>
  pull(total)

posterior_three_studies <- bayes_df |>
  filter(time == "Update 3") |>
  pull(mu)

posterior_final <- bayes_df |>
  filter(time == "Update 4") |>
  pull(mu)

posterior_final_sd <- bayes_df |>
  filter(time == "Update 4") |>
  pull(sigma)

# A shift of p in two-party vote share moves the margin by 2p, since every vote gained
# is also a vote the other side does not get, so the votes swung are the electorate
# times the posterior mean times two.
cost_per_vote <- function(votes, effect) total_cost / (votes * effect * 2)

results <- tibble(
  quantity = c(
    "total_two_party_votes_treated_zips",
    "posterior_mean_after_three_studies",
    "posterior_mean_all_four_studies",
    "posterior_sd_all_four_studies",
    "cost_per_vote_rounded_inputs",
    "cost_per_vote_unrounded_inputs"
  ),
  value = c(
    total_votes_treated,
    posterior_three_studies,
    posterior_final,
    posterior_final_sd,
    cost_per_vote(round(total_votes_treated), round(posterior_final, 3)),
    cost_per_vote(total_votes_treated, posterior_final)
  ),
  note = c(
    "Sum of vote_total_2018 across treated ZIP codes",
    "Reported in the text as 1.0 percentage points",
    "Reported in the text as 0.7 percentage points",
    "Reported in the text as 0.4 percentage points",
    "Reproduces the published $8.68 using the rounded quantities the footnote quotes",
    "Same calculation carried out at full precision"
  )
)

write_csv(results, here::here("maintained", "output", "text_in_text_calculations.csv"))

print(results)

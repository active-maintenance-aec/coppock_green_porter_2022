# coppock_green_porter_2022/maintained/figure_g2_design_diagnosis.R
# Output: output/figure_g2_design_diagnosis.pdf, output/figure_g2_design_diagnosis.png,
#   output/figure_g2_design_diagnosis.csv
# Depends on: CGP_2022_precinct_level.rds, CGP_2022_zip_code_level.rds, helpers.R
# Description: Reproduces appendix Figure G.2, the design diagnosis comparing the power
#   of the covariate-adjusted OLS estimator with the power of the posterior that combines
#   that estimate with the prior implied by the earlier literature.
#
# As with the randomization inference, the archive shipped sims = 500 with the line it
# came from, sims = 5000, commented out above it, and set no seed. The published number
# of draws costs about a minute here, so it is used, and a seed is set.
#
# The published figure labels its panels "Power when PATE = 0.1". The simulated effects
# are drawn from a normal centred at 0.01, which is also what the appendix text says, so
# the label reads 0.01 here. See the errata section of the report.

source(here::here("maintained", "helpers.R"))

set.seed(20181106)

sims <- 5000

prior_estimate <- 0.010
prior_std_error <- 0.005

precinct_level <- read_rds(
  here::here("original", "replication_archive", "CGP_2022_precinct_level.rds")
)
zip_code_level <- read_rds(
  here::here("original", "replication_archive", "CGP_2022_zip_code_level.rds")
)

blocked_zips <- zip_code_level |>
  transmute(vb_tsmart_zip, block_id)

assign_zips <- function(data) {
  new_randomization <- blocked_zips |>
    mutate(Zsim = block_ra(block_id))
  left_join(
    data,
    select(new_randomization, vb_tsmart_zip, Zsim),
    by = "vb_tsmart_zip"
  )
}

design <-
  declare_model(data = precinct_level) +
  declare_model(
    potential_outcomes(
      Y ~ rnorm(n = 1, mean = prior_estimate, sd = prior_std_error) * Zsim +
        dem_two_party_share_2018,
      conditions = list(Zsim = c(0, 1))
    )
  ) +
  declare_assignment(handler = assign_zips) +
  declare_measurement(Y = reveal_outcomes(Y ~ Zsim)) +
  declare_estimator(
    Y ~ Zsim +
      dem_two_party_vote_margin_2016_nona + vote_2016_missing +
      dem_two_party_vote_margin_2014_nona + vote_2014_missing +
      dem_two_party_vote_margin_2012_nona + vote_2012_missing,
    clusters = vb_tsmart_zip
  )

simulations <- simulate_design(design, sims = sims)

prior_weight <- 1 / prior_std_error^2

simulations <- simulations |>
  mutate(
    data_estimate = estimate,
    data_std.error = std.error,
    data_conf.low = data_estimate - 1.96 * data_std.error,
    data_conf.high = data_estimate + 1.96 * data_std.error,
    data_weight = 1 / data_std.error^2,
    data_p.upper = pnorm(statistic, lower.tail = FALSE),
    data_significant = as.numeric(data_p.upper <= 0.05),
    posterior_estimate =
      (data_estimate * data_weight + prior_estimate * prior_weight) /
        (data_weight + prior_weight),
    posterior_std.error = sqrt(1 / (data_weight + prior_weight)),
    posterior_conf.low = posterior_estimate - 1.96 * posterior_std.error,
    posterior_conf.high = posterior_estimate + 1.96 * posterior_std.error,
    posterior_p.upper = pnorm(posterior_estimate / posterior_std.error, lower.tail = FALSE),
    posterior_significant = as.numeric(posterior_p.upper <= 0.05)
  )

diagnosands <- simulations |>
  summarise(
    sims = n(),
    mean_data_estimate = mean(data_estimate),
    sd_data_estimate = sd(data_estimate),
    mean_posterior_estimate = mean(posterior_estimate),
    sd_posterior_estimate = sd(posterior_estimate),
    power_data = mean(data_significant),
    power_posterior = mean(posterior_significant)
  )

write_csv(diagnosands, here::here("maintained", "output", "figure_g2_design_diagnosis.csv"))

gg_df <- simulations |>
  mutate(sim_ID = fct_reorder(factor(sim_ID), data_estimate)) |>
  pivot_longer(cols = starts_with(c("data_", "posterior_"))) |>
  select(sim_ID, name, value) |>
  separate_wider_delim(name, delim = "_", names = c("estimator", "statistic")) |>
  pivot_wider(
    id_cols = c(sim_ID, estimator),
    names_from = statistic,
    values_from = value
  ) |>
  mutate(
    significant = factor(significant),
    facet = if_else(
      estimator == "data",
      "Simulated OLS estimate",
      "Simulated posterior estimate"
    )
  )

label_df <- gg_df |>
  summarise(power = mean(significant == 1), .by = facet) |>
  mutate(label = str_glue("Power when\nPATE = 0.01\n{sprintf('%.3f', power)}"))

g <- ggplot(gg_df) +
  aes(estimate, sim_ID, color = significant) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), alpha = 0.4) +
  geom_point(alpha = 0.5, stroke = 0, size = 1) +
  geom_text(
    data = label_df,
    aes(x = 0.035, y = sims, label = label, color = NULL),
    hjust = 0, size = 4
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = prior_estimate, linetype = "dotted", alpha = 0.5) +
  scale_color_manual(values = c(gray(0.8), gray(0.2))) +
  labs(x = "Simulated average treatment effect estimate") +
  facet_wrap(~facet) +
  theme_bw() +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
  )

ggsave(
  here::here("maintained", "output", "figure_g2_design_diagnosis.pdf"),
  plot = g, width = 6.5, height = 4
)
ggsave(
  here::here("maintained", "output", "figure_g2_design_diagnosis.png"),
  plot = g, width = 6.5, height = 4, dpi = 300
)

print(diagnosands)

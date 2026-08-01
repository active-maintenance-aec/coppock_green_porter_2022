# coppock_green_porter_2022/maintained/figure_1_bayesian_learning.R
# Output: output/figure_1_bayesian_learning.pdf, output/figure_1_bayesian_learning.png,
#   output/figure_1_bayesian_learning.csv
# Depends on: output/table_4_vote_share_precinct.csv, helpers.R
# Description: Reproduces Figure 1, the sequence of normal posteriors obtained by
#   updating a diffuse prior with each of the four field experiments in this literature.
#
# The first three estimates are the published results of other studies and are entered
# as literature values. The fourth is this paper's own covariate-adjusted estimate, which
# is read from the Table 4 output rather than typed in, so the figure cannot disagree
# with the table it summarises.

source(here::here("maintained", "helpers.R"))

present_study <- read_csv(
  here::here("maintained", "output", "table_4_vote_share_precinct.csv"),
  show_col_types = FALSE
) |>
  filter(model == "Model 2", term == "Z")

study_levels <- c(
  "Prior",
  "Broockman and Green (2014)",
  "Turitto et al. (2014)",
  "Hager (2019)",
  "The present study"
)

experiments_df <- tibble(
  study = factor(study_levels[2:5], levels = study_levels),
  estimate = c(0.016, 0.011, 0.009, present_study$estimate),
  std.error = c(0.014, 0.021, 0.006, present_study$std.error)
)

# Inverse-variance combination of a normal prior with a normal likelihood.
bayes_updater <- function(estimate_1, std_error_1, estimate_2, std_error_2) {
  precision <- 1 / c(std_error_1, std_error_2)^2
  tibble(
    mu = weighted.mean(c(estimate_1, estimate_2), w = precision),
    sigma = sqrt(1 / sum(precision))
  )
}

bayes_learning <- function(prior_mu, prior_sigma, experiments_df) {
  beliefs_df <- tibble(mu = prior_mu, sigma = prior_sigma, time = "Prior")
  for (i in seq_len(nrow(experiments_df))) {
    update_df <- bayes_updater(
      beliefs_df$mu[i], beliefs_df$sigma[i],
      experiments_df$estimate[i], experiments_df$std.error[i]
    )
    update_df$time <- paste0("Update ", i)
    beliefs_df <- bind_rows(beliefs_df, update_df)
  }
  beliefs_df
}

bayes_df <- bayes_learning(
  prior_mu = 0,
  prior_sigma = 0.05,
  experiments_df = experiments_df
) |>
  mutate(study = factor(study_levels, levels = study_levels)) |>
  left_join(experiments_df, by = "study")

write_csv(bayes_df, here::here("maintained", "output", "figure_1_bayesian_learning.csv"))

panel_labels <- bayes_df |>
  mutate(
    label = if_else(
      study == "Prior",
      str_glue("Prior: N({sprintf('%.3f', mu)},{sprintf('%.3f', sigma)})"),
      str_glue(
        "{study}\nEstimate: {sprintf('%.3f', estimate)} (SE: {sprintf('%.3f', std.error)})\n",
        "Posterior: N({sprintf('%.3f', mu)}, {sprintf('%.3f', sigma)})"
      )
    )
  ) |>
  mutate(label = factor(label, levels = label))

gg_df <- panel_labels |>
  reframe(
    label = label,
    mu = mu,
    sigma = sigma,
    x = list(seq(-0.05, 0.05, 0.0001)),
    .by = study
  ) |>
  unnest(x) |>
  mutate(y = dnorm(x, mean = mu, sd = sigma))

g <- ggplot(gg_df, aes(x = x, y = y)) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_ribbon(
    data = \(d) filter(d, x > 0),
    aes(ymax = y, ymin = 0),
    fill = "lightgray", colour = NA, alpha = 0.5
  ) +
  scale_x_continuous(breaks = c(-0.03, 0, 0.03)) +
  facet_grid(~label) +
  theme_bw() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(size = 5),
    axis.text.x = element_text(size = 5),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave(
  here::here("maintained", "output", "figure_1_bayesian_learning.pdf"),
  plot = g, width = 6.5, height = 1.9
)
ggsave(
  here::here("maintained", "output", "figure_1_bayesian_learning.png"),
  plot = g, width = 6.5, height = 1.9, dpi = 300
)

print(bayes_df)

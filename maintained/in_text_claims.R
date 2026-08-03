# coppock_green_porter_2022/maintained/in_text_claims.R
# Output: printed to the console; no file
# Depends on: helpers.R, everything under output/, ground_truth/published_claims.csv
# Description: Every number the article and its online appendix print, beside the sentence
#   that prints it, in the order a reader meets them.
#
#   This file recomputes. It reads the same maintained/output/ files the ground truth reads
#   and does its own selection, unit conversion and rounding, so the two instruments arrive
#   at each number by separate paths and a disagreement between them is a finding rather
#   than a coincidence. It never refits: estimation happens once, in the analysis scripts,
#   and only derivation happens twice. It never reads the ground truth.
#
#   It does read ground_truth/published_claims.csv, which is the extraction rather than the
#   comparison. A block cannot print a value at the article's own precision without the
#   string the article printed, and quoting that string beside the computed number is what
#   makes the output readable without the article open.
#
#   Every claim prints one line, CLAIM <id> = <value> || <label>. That printed id is the
#   only link between a block and the claim it covers: ground_truth/build_ground_truth.R
#   runs this file non-interactively, counts the CLAIM lines, and stops unless they are
#   exactly the claims the extraction says need one. cat() is used because a labelled line
#   per claim is what makes the output scannable beside the sentences; it is permitted here
#   and in no other file in this repository.
#
#   Float cells carry no verbatim quote. Prose claims all do.

source(here::here("maintained", "helpers.R"))

options(width = 200)

out <- function(f) {
  read_csv(here::here("maintained", "output", f), show_col_types = FALSE)
}

published_claims <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(.default = col_character())
)

# Reporting ----
# The article's own precision governs how a value is printed, and the article's own string
# is the only place that precision is recorded: 0.80 and 0.8 are the same double.

page <- function(id) {
  value <- published_claims$value_paper[published_claims$claim_id == id]
  stopifnot(length(value) == 1)
  value
}

printed_decimals <- function(txt) {
  if (is.na(txt) || !str_detect(txt, fixed("."))) 0L else nchar(str_remove(txt, "^[^.]*[.]"))
}

report <- function(id, value, gloss, digits = NULL) {
  stopifnot(length(value) == 1)
  target <- page(id)
  if (is.null(digits)) digits <- printed_decimals(target)
  text <- if (is.na(value)) "NA" else sprintf(str_c("%.", digits, "f"), value)
  cat("CLAIM ", id, " = ", text, " || ", gloss,
      if (is.na(target) || target == "") "" else str_c(" [article: ", target, "]"), "\n", sep = "")
}

# A descriptive claim has no printed number, so what it reports is a truth value. The
# evidence behind it is printed beside the verdict rather than instead of it.
report_flag <- function(id, holds, gloss) {
  stopifnot(length(holds) == 1)
  cat("CLAIM ", id, " = ", if (is.na(holds)) "NA" else if (holds) "TRUE" else "FALSE",
      " || ", gloss, "\n", sep = "")
}

# Accessors ----
# Each stops unless the filter selects exactly one row, so a claim cannot quietly read a
# row that is not there, or read two rows and print the first.

cell <- function(d, ...) {
  hits <- filter(d, ...)
  stopifnot(nrow(hits) == 1)
  hits
}

pick <- function(d, quantity_name) {
  hits <- d$value[d$quantity == quantity_name]
  stopifnot(length(hits) == 1)
  hits
}

# Published regression tables ----
# The nine four-model tables and the two balance tables are read cell by cell rather than
# in summary, because a float covered by one row is a float nobody has checked. The
# claim_id encodes the table, the model, the coefficient and whether the cell is an
# estimate or a standard error; the coefficient keys are resolved into the deposit's own
# variable names one table at a time, since the same key means a vote share in Table 4 and
# a vote margin in Table A.1.

covariate_terms <- function(stem) {
  set_names(
    c(sprintf(stem, c(2016, 2014, 2012)), sprintf("vote_%d_missingTRUE", c(2016, 2014, 2012))),
    c("x2016", "x2014", "x2012", "m2016", "m2014", "m2012")
  )
}

share_terms <- covariate_terms("dem_two_party_share_%d_nona")
margin_terms <- covariate_terms("dem_two_party_vote_margin_%d_nona")
precinct_total_terms <- covariate_terms("vote_total_%d_nona")
zip_total_terms <- covariate_terms("two_party_vote_total_%d_nona")

treatment_terms <- c(any = "Z", tv1 = "treatmentvideo_1", tv2 = "treatmentvideo_2",
                     int = "(Intercept)")

ids_for <- function(prefix) {
  published_claims$claim_id[str_detect(published_claims$claim_id, str_c("^", prefix, "_"))]
}

# Table B.3's published cells are the vote margin cells of Table B.4, and the rewrite's are
# vote share. Formatting a vote share to the two decimals a vote margin table prints turns
# every one of them into 0.00 and destroys the evidence, so those cells print at four.
report_regression_cell <- function(id, tidy_d, gof_d, terms, gloss_prefix, digits = NULL) {
  parts <- str_split_1(id, "_")
  model_label <- str_c("Model ", str_sub(parts[2], 2))
  key <- parts[3]
  if (key %in% c("r2", "nobs", "nclust")) {
    row <- cell(gof_d, model == model_label)
    value <- switch(key, r2 = row$r.squared, nobs = row$nobs, nclust = row$nclusters)
    label <- switch(key, r2 = "R2", nobs = "observations", nclust = "clusters")
    report(id, value, str_c(gloss_prefix, ", ", model_label, ": ", label))
  } else {
    term_name <- c(treatment_terms, terms)[[key]]
    row <- cell(tidy_d, model == model_label, term == term_name)
    value <- if (parts[4] == "est") row$estimate else row$std.error
    label <- if (parts[4] == "est") "estimate" else "standard error"
    report(id, value, str_c(gloss_prefix, ", ", model_label, ", ", term_name, ": ", label),
           digits = digits)
  }
}

report_regression_table <- function(prefix, stem, terms, gloss_prefix, digits = NULL) {
  tidy_d <- out(str_c(stem, ".csv"))
  gof_d <- out(str_c(stem, "_gof.csv"))
  for (id in ids_for(prefix)) {
    coefficient_cell <- str_detect(id, "_(est|se)$")
    report_regression_cell(id, tidy_d, gof_d, terms, gloss_prefix,
                           digits = if (coefficient_cell) digits else NULL)
  }
}

report_balance_table <- function(prefix, stem, terms, gloss_prefix) {
  tidy_d <- out(str_c(stem, ".csv"))
  gof_d <- out(str_c(stem, "_ftest.csv"))
  for (id in ids_for(prefix)) {
    parts <- str_split_1(id, "_")
    key <- parts[3]
    if (key %in% c("r2", "nobs", "nclust")) {
      value <- switch(key, r2 = gof_d$r.squared, nobs = gof_d$nobs, nclust = gof_d$nclusters)
      label <- switch(key, r2 = "R2", nobs = "observations", nclust = "clusters")
      report(id, value, str_c(gloss_prefix, ": ", label))
    } else {
      term_name <- c(treatment_terms, terms)[[key]]
      row <- cell(tidy_d, term == term_name)
      value <- if (parts[4] == "est") row$estimate else row$std.error
      label <- if (parts[4] == "est") "estimate" else "standard error"
      report(id, value, str_c(gloss_prefix, ", ", term_name, ": ", label))
    }
  }
}

# Pipeline output ----

t1 <- out("table_1_sample_description.csv")
t4 <- out("table_4_vote_share_precinct.csv")
ta1 <- out("table_a1_vote_margin_precinct.csv")
ta2 <- out("table_a2_turnout_precinct.csv")
tc6 <- out("table_c6_equivalence_tests.csv")
te7 <- out("table_e7_balance_precinct.csv")
te7_f <- out("table_e7_balance_precinct_ftest.csv")
tf8_f <- out("table_f8_balance_zip_ftest.csv")
tf9 <- out("table_f9_vote_share_zip.csv")
tf9_gof <- out("table_f9_vote_share_zip_gof.csv")
tf10 <- out("table_f10_vote_margin_zip.csv")
tf11 <- out("table_f11_turnout_zip.csv")
f1 <- out("figure_1_bayesian_learning.csv")
fg2 <- out("figure_g2_design_diagnosis.csv")
in_text <- out("text_in_text_calculations.csv")
ri <- out("text_randomization_inference.csv")

t1_count <- function(sample_label, condition_label, unit) {
  row <- cell(t1, sample == sample_label, condition == condition_label)
  if (unit == "prec") row$precincts else row$zip_codes
}

t4_cell <- function(model_label, term_name, what) {
  row <- cell(t4, model == model_label, term == term_name)
  if (what == "est") row$estimate else row$std.error
}

# Abstract ----

# "This wide saturation notwithstanding, we find that these advertisements had very small
#  estimated effects on Democratic vote share at the precinct level (-0.04 percentage
#  points, SE: 0.85 points)."
report("abs_adjusted_est", 100 * t4_cell("Model 2", "Z", "est"),
       "Covariate-adjusted effect on vote share, percentage points")
report("abs_adjusted_se", 100 * t4_cell("Model 2", "Z", "se"),
       "Standard error of the covariate-adjusted effect, percentage points")

# Field experiment: Florida advertisements ----

# "Our experimental units are the 210 ZIP codes associated with Florida congressional
#  districts 15, 16, 26, and 27."
report("design_zip_codes", t1_count("No exclusions", "Total", "zip"),
       "ZIP codes randomized")

# "ZIP codes are typically much larger than voting precincts, with the average ZIP code
#  containing approximately 10-20 voting precincts."
#
# The deposit ships only precincts already matched to a single ZIP code, so it cannot
# speak to the unrestricted count the sentence describes. What it does give is printed
# beside the claim, and the ground truth records the locus as the archive.
precincts_per_zip <- t1_count("Only nested precincts", "Total", "prec") /
  t1_count("Only nested precincts", "Total", "zip")
report("design_precincts_per_zip_low", precincts_per_zip,
       "Nested precincts per ZIP code in the deposit; the unrestricted mapping is not deposited")
report("design_precincts_per_zip_high", precincts_per_zip,
       "Nested precincts per ZIP code in the deposit; the unrestricted mapping is not deposited")

# "ZIP codes could be assigned to one of three conditions: a control condition (N = 104) or
#  one of two advertisement conditions (N = 53 each)."
report("design_control_zips", t1_count("No exclusions", "Control", "zip"),
       "ZIP codes assigned to control")
report("design_treatment_zips", t1_count("No exclusions", "Treatment video 1", "zip"),
       "ZIP codes assigned to advertisement video 1")

# "After excluding precincts that span ZIP codes and matching to 2018 election returns, our
#  dataset consists of 853 voting precincts fully nested within 154 ZIP codes."
report("design_precincts", t1_count("With outcome data", "Total", "prec"),
       "Voting precincts in the analysis sample")
report("design_zips_analysis", t1_count("With outcome data", "Total", "zip"),
       "ZIP codes in the analysis sample")

# Table 1 ----

for (id in ids_for("t1")) {
  parts <- str_split_1(id, "_")
  sample_label <- c(noex = "No exclusions", nested = "Only nested precincts",
                    outcome = "With outcome data")[[parts[2]]]
  condition_label <- c(control = "Control", v1 = "Treatment video 1",
                       v2 = "Treatment video 2", total = "Total")[[parts[3]]]
  report(id, t1_count(sample_label, condition_label, parts[4]),
         str_c("Table 1, ", sample_label, ", ", condition_label, ": ",
               if (parts[4] == "prec") "precincts" else "ZIP codes"))
}

# Results ----

# "The first column shows the unadjusted difference-in-means estimate of the treatment
#  effect of any video: a 2.1 percentage point increase in Democratic vote share, though
#  this estimate is uncertain, as evidenced by its large standard error of 3.0 percentage
#  points (one-tailed randomization inference p-value = 0.471)."
report("results_unadj_est", 100 * t4_cell("Model 1", "Z", "est"),
       "Unadjusted effect on vote share, percentage points")
report("results_unadj_se", 100 * t4_cell("Model 1", "Z", "se"),
       "Standard error of the unadjusted effect, percentage points")
report("results_ri_p_unadj", cell(ri, estimator == "DIM binary")$p_upper,
       "One-tailed randomization inference p-value, unadjusted")

# "The inclusion of covariates (column 2) dramatically increases precision: the adjusted
#  estimate is -0.04 percentage points with a standard error of just 0.85 percentage
#  points. Model two is clearly more informative and suggests that the average treatment
#  effect is very close to zero (p-value = 0.508)."
report("results_adj_est", 100 * t4_cell("Model 2", "Z", "est"),
       "Covariate-adjusted effect on vote share, percentage points")
report("results_adj_se", 100 * t4_cell("Model 2", "Z", "se"),
       "Standard error of the covariate-adjusted effect, percentage points")
report("results_ri_p_adj", cell(ri, estimator == "OLS binary")$p_upper,
       "One-tailed randomization inference p-value, covariate adjusted")

# "This approach has the advantage that we can include all 210 randomized ZIP codes in the
#  analysis, but relies on a proportional allocation rule for handling precincts that span
#  ZIP code boundaries."
report("results_zip_all", cell(tf9_gof, model == "Model 1")$nobs,
       "ZIP codes in the ZIP-level analysis")

# Table 4 ----

report_regression_table("t4", "table_4_vote_share_precinct", share_terms, "Table 4")

# Figure 1 ----

# "We begin with a diffuse prior centered on zero with a standard deviation of 5 percentage
#  points."
report("f1_prior_mu", cell(f1, study == "Prior")$mu, "Figure 1 prior mean")
report("f1_prior_sd", cell(f1, study == "Prior")$sigma, "Figure 1 prior standard deviation")
report("bayes_prior_sd_pp", 100 * cell(f1, study == "Prior")$sigma,
       "Prior standard deviation, percentage points")

panel_studies <- c(bg = "Broockman and Green (2014)", tur = "Turitto et al. (2014)",
                   hag = "Hager (2019)", pres = "The present study")

for (key in names(panel_studies)) {
  study_name <- panel_studies[[key]]
  row <- cell(f1, study == study_name)
  report(str_c("f1_", key, "_post_mu"), row$mu, str_c("Figure 1 posterior mean after ", study_name))
  report(str_c("f1_", key, "_post_sd"), row$sigma,
         str_c("Figure 1 posterior standard deviation after ", study_name))
}

# The present study's own panel label is the only one of the five that the pipeline
# produces rather than carries: it is Table 4 Model 2, read back at three decimals.
report("f1_pres_est", cell(f1, study == "The present study")$estimate,
       "Figure 1 panel label, the present study's estimate")
report("f1_pres_se", cell(f1, study == "The present study")$std.error,
       "Figure 1 panel label, the present study's standard error")

# "After these first three studies, the posterior (panel 4) has a mean of 1.0 percentage
#  points with a standard deviation of 0.5 percentage points."
report("bayes_panel4_mean", 100 * cell(f1, study == "Hager (2019)")$mu,
       "Posterior mean after the first three studies, percentage points")
report("bayes_panel4_sd", 100 * cell(f1, study == "Hager (2019)")$sigma,
       "Posterior standard deviation after the first three studies, percentage points")

# "Contributing an estimate centered precisely on zero, the Florida study shrinks the
#  posterior to a mean of 0.7 points with a standard deviation of 0.4 points."
report("bayes_final_mean", 100 * cell(f1, study == "The present study")$mu,
       "Posterior mean after all four studies, percentage points")
report("bayes_final_sd", 100 * cell(f1, study == "The present study")$sigma,
       "Posterior standard deviation after all four studies, percentage points")

# "Despite the fact that each of the four studies returned statistically insignificant
#  results, the accumulation of evidence over time yields a relatively sharp picture of
#  small positive effects."
insignificant <- f1 |>
  drop_na(estimate) |>
  mutate(significant = abs(estimate / std.error) > qnorm(0.975))
report_flag("bayes_all_insignificant", !any(insignificant$significant),
            str_c("Studies whose estimate clears two standard errors: ",
                  sum(insignificant$significant), " of ", nrow(insignificant)))

# "In the appendix, we conduct a design diagnosis (Blair et al., 2019) that shows how, on
#  its own, our experiment is relatively underpowered for the 1.0 percentage point average
#  effect implied by the previous literature (power: 21%). However, if we conceive of the
#  design as first obtaining an experimental estimate, then combining it with prior
#  evidence to produce a Bayesian posterior, the power is much stronger (89%)."
report("power_pate_pp", 100 * cell(f1, study == "Hager (2019)")$mu,
       "Average effect implied by the previous literature, percentage points")
report("power_own_pct", 100 * fg2$power_data, "Power of the experiment on its own, per cent")
report("power_combined_pct", 100 * fg2$power_posterior,
       "Power of the Bayesian posterior estimator, per cent")

# Discussion ----

# "That said, if one were to take the mean of our final Bayesian posterior (0.7 percentage
#  points) at face value, it would imply a cost per vote of $8.68, a figure that compares
#  favorably to other campaign tactics (Green and Gerber, 2019)."
report("disc_posterior_mean", 100 * pick(in_text, "posterior_mean_all_four_studies"),
       "Mean of the final Bayesian posterior, percentage points")
report("disc_cost_per_vote", pick(in_text, "cost_per_vote_rounded_inputs"),
       "Implied cost per vote, dollars")

# "It is telling that, for every voter who watched our ads all the way through, roughly 10
#  viewers skipped our ads after 3 seconds."
#
# Table 3's exposure counts are the advertisement vendor's and are in no deposited file, so
# this ratio is computed from the article's own printed cells rather than from the deposit.
vendor <- function(id) as.numeric(page(id))
skip_ratio <- (vendor("t3_ad1_three") + vendor("t3_ad2_three")) /
  (vendor("t3_ad1_full") + vendor("t3_ad2_full"))
report("disc_skip_ratio", skip_ratio,
       "Three-second views per full view, from Table 3's own cells; the deposit carries neither")

# "This calculation is based on delivering advertisements to 822,783 voters living in
#  targeted ZIP codes, at a total cost of $100,000, inclusive of production and
#  distribution costs. 100,000 / (822783 * 0.007 * 2) = 8.68."
report("fn_voters", pick(in_text, "total_two_party_votes_treated_zips"),
       "Two-party votes cast in treated ZIP codes")
report("fn_calc_voters", pick(in_text, "total_two_party_votes_treated_zips"),
       "The same vote total, inside the printed expression")
report("fn_calc_posterior", pick(in_text, "posterior_mean_all_four_studies"),
       "Posterior mean inside the printed expression")
report("fn_calc_result", pick(in_text, "cost_per_vote_rounded_inputs"),
       "Result of the printed expression, dollars")

# Appendix A: alternative outcome measures ----

# "As in the main analysis, we find small, statistically insignificant effects of our
#  advertisements on both alternative outcome measures."
alternative_treatment_rows <- bind_rows(ta1, ta2) |>
  filter(term %in% treatment_terms[c("any", "tv1", "tv2")])
report_flag("app_a_small_insignificant", all(alternative_treatment_rows$p.value > 0.05),
            str_c("Treatment coefficients in Tables A.1 and A.2 significant at 0.05: ",
                  sum(alternative_treatment_rows$p.value <= 0.05), " of ",
                  nrow(alternative_treatment_rows)))

report_regression_table("ta1", "table_a1_vote_margin_precinct", margin_terms, "Table A.1")
report_regression_table("ta2", "table_a2_turnout_precinct", precinct_total_terms, "Table A.2")

# Appendix B: congressional district fixed effects ----
#
# Published Table B.3 is a reprint of Table B.4, so its cells are vote margins beneath a
# vote share caption. The rewrite fits the vote share models the caption describes; those
# print at four decimals here, because formatting a vote share to the two decimals a vote
# margin table uses collapses every cell to 0.00 and hides what the comparison shows.
report_regression_table("tb3", "table_b3_vote_share_precinct_cd", share_terms, "Table B.3",
                        digits = 4)
report_regression_table("tb4", "table_b4_vote_margin_precinct_cd", margin_terms, "Table B.4")
report_regression_table("tb5", "table_b5_turnout_precinct_cd", precinct_total_terms, "Table B.5")

# Appendix C: equivalence tests ----

# "The difference-in-means estimates of the difference between the two treatment groups are
#  quite imprecise, with the result that none of the p-values for the difference-in-means
#  estimates are smaller than 0.05."
dim_p <- tc6 |> filter(estimator == "DIM") |> select(p_eq_0.2, p_eq_0.1) |> unlist()
report_flag("app_c_dim_none_below", all(dim_p >= 0.05),
            str_c("Smallest difference-in-means equivalence p-value: ",
                  sprintf("%.3f", min(dim_p))))

# "Using the covariate-adjusted models (marked as 'OLS'), we can affirm equivalence at
#  p < 0.05 for all three outcome variables using a 0.2 standard deviation equivalence
#  tolerance."
ols_02 <- tc6 |> filter(estimator == "OLS") |> pull(p_eq_0.2)
report_flag("app_c_ols_02_all", all(ols_02 < 0.05),
            str_c("Largest covariate-adjusted p at the 0.2 SD tolerance: ",
                  sprintf("%.3f", max(ols_02))))

# "When we turn to the more restrictive tolerance of 0.1 SDs, only the OLS estimate of the
#  difference on vote total can be affirmed equivalent."
ols_01 <- tc6 |> filter(estimator == "OLS", p_eq_0.1 < 0.05)
report_flag("app_c_ols_01_total_only",
            nrow(ols_01) == 1 && ols_01$outcome_name == "Vote Total",
            str_c("Outcomes affirmed equivalent at the 0.1 SD tolerance: ",
                  str_c(ols_01$outcome_name, collapse = ", ")))

for (id in ids_for("tc6")) {
  parts <- str_split_1(id, "_")
  outcome_label <- c(share = "Vote Share", margin = "Vote Margin", total = "Vote Total")[[parts[2]]]
  row <- cell(tc6, outcome_name == outcome_label, estimator == toupper(parts[3]))
  value <- switch(parts[4], sd = row$sd_1.0, est = row$estimate, se = row$std.error,
                  p02 = row$p_eq_0.2, p01 = row$p_eq_0.1)
  report(id, value, str_c("Table C.6, ", outcome_label, ", ", toupper(parts[3]), ": ", parts[4]))
}

# Appendix D: map ----

# "The map only represents the 148 ZIP codes that have geographic boundaries, not the full
#  set of 210 ZIP codes in our experiment, some of which can not be represented on a map."
report("app_d_all_zips", t1_count("No exclusions", "Total", "zip"),
       "ZIP codes in the experiment, restated in the map section")

# Appendix E: balance ----

# "Each of the individual coefficients is nonsignificant, as is a joint test of
#  significance (p = 0.33)."
balance_coefficients <- te7 |> filter(term != "(Intercept)")
report_flag("app_e_coefs_nonsig", all(balance_coefficients$p.value > 0.05),
            str_c("Balance coefficients significant at 0.05: ",
                  sum(balance_coefficients$p.value <= 0.05), " of ",
                  nrow(balance_coefficients)))
report("app_e_joint_p", te7_f$p.value, "Joint significance test p-value, precinct level")

report_balance_table("te7", "table_e7_balance_precinct", margin_terms, "Table E.7")

# Appendix F: ZIP-level aggregation ----

# "As described in the main text, we block-randomized 210 ZIP-codes to receive treatment
#  advertisements or not."
report("app_f_zips", t1_count("No exclusions", "Total", "zip"),
       "ZIP codes block-randomized, restated in the aggregation section")

# "We first show that this approach generates experimental balance in Table F.8. A joint
#  significance test returns a p-value of 0.328."
report("app_f8_joint_p", tf8_f$p.value, "Joint significance test p-value, ZIP level")

report_balance_table("tf8", "table_f8_balance_zip", margin_terms, "Table F.8")

# "Whether we adjust for covariates or not, or whether we disaggregate by the two treatment
#  videos or not, we find very small effects of treatment that cannot be distinguished from
#  zero."
zip_treatment_rows <- bind_rows(tf9, tf10, tf11) |>
  filter(term %in% treatment_terms[c("any", "tv1", "tv2")])
report_flag("app_f_small_effects", all(zip_treatment_rows$p.value > 0.05),
            str_c("ZIP-level treatment coefficients significant at 0.05: ",
                  sum(zip_treatment_rows$p.value <= 0.05), " of ",
                  nrow(zip_treatment_rows)))

report_regression_table("tf9", "table_f9_vote_share_zip", share_terms, "Table F.9")
report_regression_table("tf10", "table_f10_vote_margin_zip", margin_terms, "Table F.10")
report_regression_table("tf11", "table_f11_turnout_zip", zip_total_terms, "Table F.11")

# Appendix G: design diagnosis ----

# "We drew simulated treatment effects from the distribution implied by similar studies
#  conducted to-date: a normal distribution centered at 0.01 with a standard deviation of
#  0.005."
report("app_g_pate_center", fg2$pate_center, "Centre of the simulated effect distribution")
report("app_g_pate_sd", fg2$pate_sd, "Standard deviation of the simulated effect distribution")

# "Figure G.2 shows that own its own, the power of our study is low, at 21.0%. However,
#  when we combine our estimate with previously-available information, power increases
#  dramatically, to 89.3%."
report("app_g_power_own", 100 * fg2$power_data,
       "Power of the study on its own, per cent, at the appendix's precision")
report("app_g_power_combined", 100 * fg2$power_posterior,
       "Power of the Bayesian posterior estimator, per cent, at the appendix's precision")

# The published annotation reads "PATE = 0.1" against a simulated centre of 0.01. Printed
# to the one decimal the annotation uses, the rewrite's value reads 0.0 and the comparison
# says nothing, so both sides are rendered at two.
report("fg2_ols_pate_label", fg2$pate_center, "PATE named in the Figure G.2 left panel",
       digits = 2)
report("fg2_post_pate_label", fg2$pate_center, "PATE named in the Figure G.2 right panel",
       digits = 2)
report("fg2_ols_power", fg2$power_data, "Power printed in the Figure G.2 left panel")
report("fg2_post_power", fg2$power_posterior, "Power printed in the Figure G.2 right panel")

# Appendix H: pre-analysis plan ----

# "These 210 zip codes and districts are listed in the accompanying randomization script."
# "##     0       104         0         0 / ##     1        0        53        53"
report("pap_zips", t1_count("No exclusions", "Total", "zip"),
       "ZIP codes named in the pre-analysis plan")
report("pap_control", t1_count("No exclusions", "Control", "zip"),
       "Control ZIP codes in the pre-analysis plan's assignment table")
report("pap_v1", t1_count("No exclusions", "Treatment video 1", "zip"),
       "Video 1 ZIP codes in the pre-analysis plan's assignment table")
report("pap_v2", t1_count("No exclusions", "Treatment video 2", "zip"),
       "Video 2 ZIP codes in the pre-analysis plan's assignment table")

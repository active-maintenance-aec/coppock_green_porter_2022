# coppock_green_porter_2022/ground_truth/build_ground_truth.R
# Output: ground_truth/coppock_green_porter_2022_ground_truth.csv,
#   ground_truth/float_coverage.csv
# Depends on: maintained/output/ (run run_all.R first), ground_truth/published_claims.csv,
#   ground_truth/archive_values.csv, maintained/in_text_claims.R, errata_entries.csv
# Description: Assemble the ground truth, then gate it.
#
#   value_paper is the string the article prints and comes from ground_truth/
#   published_claims.csv, the hand-reviewed extraction of every numeric token in the
#   article and the online appendix. It is a comparison target and never an input to a
#   computation, here or anywhere under maintained/.
#
#   value_rewrite is read back out of maintained/output/ by this script, so the table
#   cannot drift from the pipeline. value_script is what the deposited scripts produce; it
#   is read from ground_truth/archive_values.csv, which records the cells measured from a
#   run of the deposit. That file is a recorded measurement rather than a generated one:
#   this repository has no extract_archive_values.R, so 215 of the cells carry an archive
#   value and the rest carry none. Which is which is visible in the match column.
#
#   Every verdict is computed here. A value agrees when the rewrite's number, printed to
#   the precision the article printed, gives the same digits; a numeric tolerance cannot
#   express that, because a double does not record how many decimals were printed.

library(here)
library(tidyverse)

here::i_am("ground_truth/build_ground_truth.R")

out <- function(f) read_csv(here::here("maintained", "output", f), show_col_types = FALSE)

# value_paper is forced to character in every reader. A column whose entries all look
# numeric is guessed as double, which turns the article's 0.0250 into 0.025 and destroys
# the printed precision the comparison depends on.
published_claims <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(.default = col_character())
) |>
  mutate(needs_block = needs_block == "TRUE", digits = as.integer(digits))

stopifnot(!anyDuplicated(published_claims$claim_id))

archive_values <- read_csv(
  here::here("ground_truth", "archive_values.csv"),
  col_types = cols(claim_id = col_character(), value_script = col_double())
)

stopifnot(!anyDuplicated(archive_values$claim_id),
          all(archive_values$claim_id %in% published_claims$claim_id))

# Every join here goes through this, which asserts uniqueness on both sides and a
# one-to-one result. A many-to-many join in a ground-truth build is a defect marker.
join_one <- function(x, y, by) {
  stopifnot(!anyDuplicated(x[[by]]), !anyDuplicated(y[[by]]))
  joined <- left_join(x, y, by = by)
  stopifnot(nrow(joined) == nrow(x))
  joined
}

# Rewrite values: published regression tables ----
# The tidy output is pivoted long and matched to the coefficient keys the claim ids use,
# so every cell of every published table is read rather than a chosen few. The same key
# means a vote share in Table 4 and a vote margin in Table A.1, so the term vector is
# resolved one table at a time.

covariate_terms <- function(stem) {
  set_names(
    c(sprintf(stem, c(2016, 2014, 2012)), sprintf("vote_%d_missingTRUE", c(2016, 2014, 2012))),
    c("x2016", "x2014", "x2012", "m2016", "m2014", "m2012")
  )
}

term_keys <- function(stem) {
  c(c(any = "Z", tv1 = "treatmentvideo_1", tv2 = "treatmentvideo_2", int = "(Intercept)"),
    covariate_terms(stem))
}

share <- "dem_two_party_share_%d_nona"
margin <- "dem_two_party_vote_margin_%d_nona"
precinct_total <- "vote_total_%d_nona"
zip_total <- "two_party_vote_total_%d_nona"

four_model_tables <- tribble(
  ~prefix, ~stem, ~variables,
  "t4", "table_4_vote_share_precinct", share,
  "ta1", "table_a1_vote_margin_precinct", margin,
  "ta2", "table_a2_turnout_precinct", precinct_total,
  "tb3", "table_b3_vote_share_precinct_cd", share,
  "tb4", "table_b4_vote_margin_precinct_cd", margin,
  "tb5", "table_b5_turnout_precinct_cd", precinct_total,
  "tf9", "table_f9_vote_share_zip", share,
  "tf10", "table_f10_vote_margin_zip", margin,
  "tf11", "table_f11_turnout_zip", zip_total
)

coefficient_rows <- function(prefix, stem, variables) {
  keys <- term_keys(variables)
  out(str_c(stem, ".csv")) |>
    select(model, term, est = estimate, se = std.error) |>
    pivot_longer(c(est, se), names_to = "quantity", values_to = "value_rewrite") |>
    mutate(key = names(keys)[match(term, keys)]) |>
    drop_na(key) |>
    transmute(
      claim_id = str_c(prefix, "_m", str_sub(model, -1), "_", key, "_", quantity),
      value_rewrite
    )
}

gof_rows <- function(prefix, stem, ...) {
  out(str_c(stem, "_gof.csv")) |>
    pivot_longer(c(r.squared, nobs, nclusters), names_to = "key", values_to = "value_rewrite") |>
    mutate(key = recode(key, r.squared = "r2", nobs = "nobs", nclusters = "nclust")) |>
    transmute(claim_id = str_c(prefix, "_m", str_sub(model, -1), "_", key), value_rewrite) |>
    drop_na(value_rewrite)
}

balance_rows <- function(prefix, stem, variables) {
  keys <- term_keys(variables)
  coefficients <- out(str_c(stem, ".csv")) |>
    select(term, est = estimate, se = std.error) |>
    pivot_longer(c(est, se), names_to = "quantity", values_to = "value_rewrite") |>
    mutate(key = names(keys)[match(term, keys)]) |>
    drop_na(key) |>
    transmute(claim_id = str_c(prefix, "_m1_", key, "_", quantity), value_rewrite)
  fit <- out(str_c(stem, "_ftest.csv"))
  summary_rows <- fit |>
    select(r2 = r.squared, nobs, any_of(c(nclust = "nclusters"))) |>
    pivot_longer(everything(), names_to = "key", values_to = "value_rewrite") |>
    transmute(claim_id = str_c(prefix, "_m1_", key), value_rewrite)
  bind_rows(coefficients, summary_rows)
}

table_cells <- bind_rows(
  pmap(four_model_tables, coefficient_rows) |> list_rbind(),
  pmap(four_model_tables, gof_rows) |> list_rbind(),
  balance_rows("te7", "table_e7_balance_precinct", margin),
  balance_rows("tf8", "table_f8_balance_zip", margin)
)

# Rewrite values: Table 1, Table C.6, Figure 1, Figure G.2 ----

sample_keys <- c("No exclusions" = "noex", "Only nested precincts" = "nested",
                 "With outcome data" = "outcome")
condition_keys <- c("Control" = "control", "Treatment video 1" = "v1",
                    "Treatment video 2" = "v2", "Total" = "total")

table_1_cells <- out("table_1_sample_description.csv") |>
  pivot_longer(c(precincts, zip_codes), names_to = "unit", values_to = "value_rewrite") |>
  transmute(
    claim_id = str_c("t1_", sample_keys[sample], "_", condition_keys[condition], "_",
                     if_else(unit == "precincts", "prec", "zip")),
    value_rewrite
  )

outcome_keys <- c("Vote Share" = "share", "Vote Margin" = "margin", "Vote Total" = "total")

table_c6_cells <- out("table_c6_equivalence_tests.csv") |>
  rename(sd = sd_1.0, est = estimate, se = std.error, p02 = p_eq_0.2, p01 = p_eq_0.1) |>
  pivot_longer(c(sd, est, se, p02, p01), names_to = "quantity", values_to = "value_rewrite") |>
  transmute(
    claim_id = str_c("tc6_", outcome_keys[outcome_name], "_", str_to_lower(estimator), "_",
                     quantity),
    value_rewrite
  )

panel_keys <- c("Prior" = "prior", "Broockman and Green (2014)" = "bg",
                "Turitto et al. (2014)" = "tur", "Hager (2019)" = "hag",
                "The present study" = "pres")

figure_1 <- out("figure_1_bayesian_learning.csv")

figure_1_cells <- bind_rows(
  figure_1 |>
    filter(study == "Prior") |>
    pivot_longer(c(mu, sigma), names_to = "quantity", values_to = "value_rewrite") |>
    transmute(claim_id = if_else(quantity == "mu", "f1_prior_mu", "f1_prior_sd"), value_rewrite),
  figure_1 |>
    filter(study != "Prior") |>
    pivot_longer(c(mu, sigma), names_to = "quantity", values_to = "value_rewrite") |>
    transmute(
      claim_id = str_c("f1_", panel_keys[study], "_post_",
                       if_else(quantity == "mu", "mu", "sd")),
      value_rewrite
    ),
  figure_1 |>
    filter(study == "The present study") |>
    pivot_longer(c(estimate, std.error), names_to = "quantity", values_to = "value_rewrite") |>
    transmute(claim_id = if_else(quantity == "estimate", "f1_pres_est", "f1_pres_se"),
              value_rewrite)
)

diagnosis <- out("figure_g2_design_diagnosis.csv")

figure_g2_cells <- tibble(
  claim_id = c("fg2_ols_pate_label", "fg2_post_pate_label", "fg2_ols_power", "fg2_post_power"),
  value_rewrite = c(diagnosis$pate_center, diagnosis$pate_center,
                    diagnosis$power_data, diagnosis$power_posterior)
)

# Rewrite values: prose ----
# Each of these is derived from a quantity already in output/ rather than recomputed. The
# article's units govern, so a vote share becomes percentage points at the comparison
# point and not before.

in_text <- out("text_in_text_calculations.csv")
randomization <- out("text_randomization_inference.csv")
te7_fit <- out("table_e7_balance_precinct_ftest.csv")
tf8_fit <- out("table_f8_balance_zip_ftest.csv")
tf9_gof <- out("table_f9_vote_share_zip_gof.csv")

quantity <- function(name) {
  hits <- in_text$value[in_text$quantity == name]
  stopifnot(length(hits) == 1)
  hits
}

cell_value <- function(d, ...) {
  hits <- filter(d, ...)
  stopifnot(nrow(hits) == 1)
  hits
}

t4_any <- function(model_label, what) {
  row <- cell_value(out("table_4_vote_share_precinct.csv"), model == model_label, term == "Z")
  if (what == "est") row$estimate else row$std.error
}

t1_of <- function(id) {
  hits <- table_1_cells$value_rewrite[table_1_cells$claim_id == id]
  stopifnot(length(hits) == 1)
  hits
}

posterior <- function(study_name, what) {
  row <- cell_value(figure_1, study == study_name)
  if (what == "mu") row$mu else row$sigma
}

precincts_per_zip <- t1_of("t1_nested_total_prec") / t1_of("t1_nested_total_zip")

# Table 3's exposure counts are the advertisement vendor's and are in no deposited file, so
# the skip ratio is formed from the article's own printed cells. Reading the extraction is
# reading the transcription, not reading a comparison, and it is the only way to give this
# sentence a verdict at all.
vendor <- function(id) {
  value <- published_claims$value_paper[published_claims$claim_id == id]
  stopifnot(length(value) == 1)
  as.numeric(value)
}

prose_cells <- tribble(
  ~claim_id, ~value_rewrite,
  "abs_adjusted_est", 100 * t4_any("Model 2", "est"),
  "abs_adjusted_se", 100 * t4_any("Model 2", "se"),
  "design_zip_codes", t1_of("t1_noex_total_zip"),
  "design_control_zips", t1_of("t1_noex_control_zip"),
  "design_treatment_zips", t1_of("t1_noex_v1_zip"),
  "design_precincts", t1_of("t1_outcome_total_prec"),
  "design_zips_analysis", t1_of("t1_outcome_total_zip"),
  "design_precincts_per_zip_low", precincts_per_zip,
  "design_precincts_per_zip_high", precincts_per_zip,
  "results_unadj_est", 100 * t4_any("Model 1", "est"),
  "results_unadj_se", 100 * t4_any("Model 1", "se"),
  "results_ri_p_unadj", cell_value(randomization, estimator == "DIM binary")$p_upper,
  "results_adj_est", 100 * t4_any("Model 2", "est"),
  "results_adj_se", 100 * t4_any("Model 2", "se"),
  "results_ri_p_adj", cell_value(randomization, estimator == "OLS binary")$p_upper,
  "results_zip_all", cell_value(tf9_gof, model == "Model 1")$nobs,
  "bayes_prior_sd_pp", 100 * posterior("Prior", "sigma"),
  "bayes_panel4_mean", 100 * posterior("Hager (2019)", "mu"),
  "bayes_panel4_sd", 100 * posterior("Hager (2019)", "sigma"),
  "bayes_final_mean", 100 * posterior("The present study", "mu"),
  "bayes_final_sd", 100 * posterior("The present study", "sigma"),
  "power_pate_pp", 100 * posterior("Hager (2019)", "mu"),
  "power_own_pct", 100 * diagnosis$power_data,
  "power_combined_pct", 100 * diagnosis$power_posterior,
  "disc_posterior_mean", 100 * quantity("posterior_mean_all_four_studies"),
  "disc_cost_per_vote", quantity("cost_per_vote_rounded_inputs"),
  "disc_skip_ratio", (vendor("t3_ad1_three") + vendor("t3_ad2_three")) /
    (vendor("t3_ad1_full") + vendor("t3_ad2_full")),
  "fn_voters", quantity("total_two_party_votes_treated_zips"),
  "fn_calc_voters", quantity("total_two_party_votes_treated_zips"),
  "fn_calc_posterior", quantity("posterior_mean_all_four_studies"),
  "fn_calc_result", quantity("cost_per_vote_rounded_inputs"),
  "app_d_all_zips", t1_of("t1_noex_total_zip"),
  "app_e_joint_p", te7_fit$p.value,
  "app_f_zips", t1_of("t1_noex_total_zip"),
  "app_f8_joint_p", tf8_fit$p.value,
  "app_g_pate_center", diagnosis$pate_center,
  "app_g_pate_sd", diagnosis$pate_sd,
  "app_g_power_own", 100 * diagnosis$power_data,
  "app_g_power_combined", 100 * diagnosis$power_posterior,
  "pap_zips", t1_of("t1_noex_total_zip"),
  "pap_control", t1_of("t1_noex_control_zip"),
  "pap_v1", t1_of("t1_noex_v1_zip"),
  "pap_v2", t1_of("t1_noex_v2_zip")
)

# Descriptive claims ----
# A descriptive claim has no printed value to compare, so its verdict is computed here and
# carried in `holds`. match and match_rewrite stay NA: they answer a question about digits
# that these sentences never pose.

treatment_terms <- c("Z", "treatmentvideo_1", "treatmentvideo_2")

alternative_outcomes <- bind_rows(
  out("table_a1_vote_margin_precinct.csv"), out("table_a2_turnout_precinct.csv")
) |>
  filter(term %in% treatment_terms)

zip_outcomes <- bind_rows(
  out("table_f9_vote_share_zip.csv"), out("table_f10_vote_margin_zip.csv"),
  out("table_f11_turnout_zip.csv")
) |>
  filter(term %in% treatment_terms)

balance_coefficients <- out("table_e7_balance_precinct.csv") |> filter(term != "(Intercept)")

equivalence <- out("table_c6_equivalence_tests.csv")
dim_p <- equivalence |> filter(estimator == "DIM") |> select(p_eq_0.2, p_eq_0.1) |> unlist()
ols_02 <- equivalence |> filter(estimator == "OLS") |> pull(p_eq_0.2)
ols_01 <- equivalence |> filter(estimator == "OLS", p_eq_0.1 < 0.05)

four_studies <- figure_1 |> drop_na(estimate)

descriptive_cells <- tribble(
  ~claim_id, ~holds,
  "bayes_all_insignificant", all(abs(four_studies$estimate / four_studies$std.error) < qnorm(0.975)),
  # The deposit ships only precincts already matched to a single ZIP code, so it cannot
  # speak to the unrestricted count this sentence describes. No verdict, and the locus
  # records that the archive is where the answer is missing.
  "design_precincts_per_zip_low", NA,
  "design_precincts_per_zip_high", NA,
  "disc_skip_ratio", TRUE,
  "app_a_small_insignificant", all(alternative_outcomes$p.value > 0.05),
  "app_c_dim_none_below", all(dim_p >= 0.05),
  "app_c_ols_02_all", all(ols_02 < 0.05),
  "app_c_ols_01_total_only", nrow(ols_01) == 1 && ols_01$outcome_name == "Vote Total",
  "app_e_coefs_nonsig", all(balance_coefficients$p.value > 0.05),
  "app_f_small_effects", all(zip_outcomes$p.value > 0.05)
)

# "roughly 10 viewers skipped our ads after 3 seconds" is hedged, so its verdict is whether
# the ratio rounds to the figure the article states rather than whether it equals it.
descriptive_cells$holds[descriptive_cells$claim_id == "disc_skip_ratio"] <-
  round(prose_cells$value_rewrite[prose_cells$claim_id == "disc_skip_ratio"]) == 10

# Assembly ----

rewrite_cells <- bind_rows(
  table_cells, table_1_cells, table_c6_cells, figure_1_cells, figure_g2_cells, prose_cells
)

stopifnot(!anyDuplicated(rewrite_cells$claim_id),
          all(rewrite_cells$claim_id %in% published_claims$claim_id))

# A published value with no rewrite counterpart must be a deliberate decision, not a
# mistyped label that quietly lands in the unverifiable bucket.
expected_without_rewrite <- str_c("t1_noex_", c("control", "v1", "v2", "total"), "_prec")

gt <- published_claims |>
  filter(needs_block) |>
  transmute(
    paper_id = "coppock_green_porter_2022",
    claim_id,
    float = str_remove(location, ",.*$"),
    claim,
    location,
    claim_type,
    value_paper,
    extracted_digits = digits,
    notes
  ) |>
  join_one(rewrite_cells, "claim_id") |>
  join_one(archive_values, "claim_id") |>
  join_one(descriptive_cells, "claim_id")

missing_rewrite <- gt$claim_id[is.na(gt$value_rewrite) & is.na(gt$holds)]
stopifnot(setequal(missing_rewrite, expected_without_rewrite))

# Verdicts ----
# value_paper is the string the article prints, and the number alone does not record its
# own precision: 0.80 and 0.8 are the same double. A value agrees when the pipeline's
# number, printed to the page's precision, gives the same digits.

printed_decimals <- function(txt) {
  if_else(is.na(txt) | !str_detect(txt, fixed(".")), 0L,
          nchar(str_remove(txt, "^[^.]*[.]")))
}

# Two places where the page's own precision destroys the evidence rather than expressing
# it. Table B.3's published cells are vote margins and the rewrite's are vote shares, so
# two decimals turns every rewrite cell into 0.00; and Figure G.2's annotation names a PATE
# of 0.1 against a simulated 0.01, which one decimal reads as 0.0. The extraction's digits
# column carries both overrides, and the gate below holds it to this list and to the
# printed precision everywhere else.
digits_override <- c(
  set_names(rep(4L, sum(str_detect(gt$claim_id, "^tb3_.*_(est|se)$"))),
            gt$claim_id[str_detect(gt$claim_id, "^tb3_.*_(est|se)$")]),
  c(fg2_ols_pate_label = 2L, fg2_post_pate_label = 2L)
)

digit_disagreement <- gt |>
  mutate(digits = coalesce(digits_override[claim_id], printed_decimals(value_paper))) |>
  filter(!is.na(value_paper), digits != extracted_digits)

if (nrow(digit_disagreement) > 0) {
  print(select(digit_disagreement, claim_id, value_paper, digits, extracted_digits))
  stop("published_claims.csv carries a digit count the builder does not.")
}

gt <- gt |>
  mutate(
    digits = coalesce(extracted_digits, printed_decimals(value_paper)),
    target = sprintf(str_c("%.", digits, "f"), parse_double(value_paper)),
    # A verdict needs something to compare. Where nothing was computed the verdict is NA;
    # a printed "NA" string would record the article as disagreeing with itself.
    match = if_else(claim_type == "descriptive" | is.na(value_script), NA_real_,
                    if_else(sprintf(str_c("%.", digits, "f"), value_script) == target, 1, 0)),
    match_rewrite = if_else(claim_type == "descriptive" | is.na(value_rewrite), NA_real_,
                            if_else(sprintf(str_c("%.", digits, "f"), value_rewrite) == target, 1, 0))
  )

# The transcription is checked against itself: rendering the parsed number back at the
# digit count the string carries must reproduce the string. That catches a value_paper
# entry whose digits are right and whose precision is not, which numeric equality passes.
transcription_defects <- gt |>
  filter(!claim_id %in% names(digits_override),
         sprintf(str_c("%.", printed_decimals(value_paper), "f"),
                 parse_double(value_paper)) != value_paper)

if (nrow(transcription_defects) > 0) {
  print(select(transcription_defects, claim_id, value_paper), n = 40)
  stop("published_claims.csv carries a value_paper that does not round-trip through its own precision.")
}

# Loci ----
# A zero reads as a failure of the rewrite, which it almost never is, so every adverse
# verdict says where the fault lies.

locus <- c(
  # The article contradicts its own Table 4. The sentence states the unadjusted estimate
  # and its standard error in percentage points an order of magnitude too large.
  results_unadj_est = "paper_internal",
  results_unadj_se = "paper_internal",
  # Neither simulation script in the deposit sets a seed, so no rerun by anyone,
  # including the authors, lands on the published draw.
  results_ri_p_unadj = "archive",
  results_ri_p_adj = "archive",
  app_g_power_own = "archive",
  app_g_power_combined = "archive",
  fg2_ols_power = "archive",
  fg2_post_power = "archive",
  # The deposited script hardcodes the panel annotation rather than reading the design
  # parameter one line above it.
  fg2_ols_pate_label = "archive",
  fg2_post_pate_label = "archive",
  # The archive's single unseeded draw put the power at 23 per cent against a published
  # 21; the rewrite's seeded run at the published number of draws reads 21.5 under
  # DeclareDesign 2.0 and rounded to 21 before it, so the gap is simulation noise.
  power_own_pct = "archive",
  # The posterior power takes no locus: it rounded to the published 89 before estimatr and
  # fabricatr 2.0, read 89.9 under them, and reads 88.7 under DeclareDesign 2.0. Each is one
  # draw of a Monte Carlo quantity whose standard error at 5000 draws is about 0.4 points.
  # The deposited script types Table 4's rounded standard error into the Figure 1 label
  # instead of passing the fitted one.
  f1_pres_se = "archive",
  # The deposit holds only the precincts nested within a single ZIP code.
  t1_noex_control_prec = "archive",
  t1_noex_v1_prec = "archive",
  t1_noex_v2_prec = "archive",
  t1_noex_total_prec = "archive",
  design_precincts_per_zip_low = "archive",
  design_precincts_per_zip_high = "archive"
)

gt <- gt |> mutate(defect_locus = unname(locus[claim_id]))

# Published Table B.3 is a reprint of Table B.4, so every cell beneath a vote share caption
# is a vote margin cell. The fault is in the article, not in either body of code. Four of
# the fifty-six cells agree anyway, because a table printed in the wrong place still
# carries the right number of observations and clusters; those are clean matches and take
# no locus.
tb3_adverse <- gt$claim_id[str_starts(gt$claim_id, "tb3_") &
                             coalesce(gt$match_rewrite == 0, FALSE)]
locus <- c(locus, set_names(rep("paper_internal", length(tb3_adverse)), tb3_adverse))

# The deposit's equivalence script calls lh_robust() with clustered CR2 standard errors,
# which current estimatr refuses, so the archive produces none of Table C.6. The rewrite
# reproduces all thirty cells through the delta method on the same variance matrix.
locus <- c(locus, set_names(rep("environment", sum(str_starts(gt$claim_id, "tc6_"))),
                            gt$claim_id[str_starts(gt$claim_id, "tc6_")]))

gt <- gt |> mutate(defect_locus = unname(locus[claim_id]))

# Gate: an adverse verdict and a locus go together ----
# Three states, not two. A descriptive claim carries its verdict in `holds`, so a gate
# stated on match_rewrite alone passes every failing descriptive claim with no locus. And a
# locus is required whenever EITHER verdict is zero, because match == 0 with
# match_rewrite == 1 is the whole signature of environment drift.

gt <- gt |>
  mutate(
    adverse = coalesce(match == 0, FALSE) | coalesce(match_rewrite == 0, FALSE) |
      coalesce(!holds, FALSE),
    # A row the deposit cannot answer is not a clean match either, whether because the
    # data are not there, because the sentence has no computable verdict, or because the
    # deposited code no longer runs.
    unsupported = (is.na(value_rewrite) & is.na(holds)) |
      (claim_type == "descriptive" & is.na(holds)) | str_starts(claim_id, "tc6_"),
    needs_locus = adverse | unsupported
  )

locus_gate <- gt |> filter(needs_locus != !is.na(defect_locus))

if (nrow(locus_gate) > 0) {
  print(select(locus_gate, claim_id, claim_type, match, match_rewrite, holds, defect_locus),
        n = 60)
  stop("Rows carrying an adverse or unsupported verdict with no locus, or a locus with neither.")
}

stopifnot(all(gt$defect_locus %in%
                c("paper_internal", "archive", "environment", "rewrite", "unresolved") |
                is.na(gt$defect_locus)))

# Gate: the second instrument ran, printed, and agrees ----
# maintained/in_text_claims.R reaches the same numbers out of the same outputs by its own
# path. It is run here rather than read, because a block that errors at its first line, or
# that ends in a bare expression and so prints nothing under source(), passes a scan for
# markers while checking nothing at all.
#
# Sourced into its own environment. Both files necessarily read the same outputs and name
# objects for what they hold, so a bare source() would replace this script's own
# `published_claims`, `out()` and `cell_value()` with the claims file's.
claims_output <- capture.output(
  source(here::here("maintained", "in_text_claims.R"), local = new.env())
)

printed_claims <- tibble(line = claims_output) |>
  filter(str_starts(line, "CLAIM "), !str_starts(line, "CLAIM SUMMARY")) |>
  transmute(
    claim_id = str_match(line, "^CLAIM ([^ ]+) = ")[, 2],
    value_in_text = str_match(line, "^CLAIM [^ ]+ = (.*?) \\|\\| ")[, 2]
  )

stopifnot(!anyDuplicated(printed_claims$claim_id), !any(is.na(printed_claims$claim_id)),
          !any(is.na(printed_claims$value_in_text)))

declared <- published_claims$claim_id[published_claims$needs_block]
required_rows <- published_claims$claim_id[published_claims$claim_type %in%
                                             c("pipeline", "descriptive")]

blockless <- setdiff(declared, printed_claims$claim_id)
invented <- setdiff(printed_claims$claim_id, declared)
rowless <- setdiff(required_rows, gt$claim_id)

if (length(blockless) > 0 || length(invented) > 0 || length(rowless) > 0) {
  stop(str_glue(
    "Coverage gate failed. ",
    "Claims with no block in maintained/in_text_claims.R ({length(blockless)}): ",
    "{str_c(head(blockless, 40), collapse = ', ')}. ",
    "Blocks naming a claim the extraction does not declare ({length(invented)}): ",
    "{str_c(head(invented, 40), collapse = ', ')}. ",
    "Claims with no ground truth row ({length(rowless)}): ",
    "{str_c(head(rowless, 40), collapse = ', ')}."
  ))
}

stopifnot(nrow(printed_claims) == length(declared))

# The two instruments must land on the same number, and on the same verdict where the
# claim is descriptive. in_text_claims.R prints in the article's units and rounding and
# never sees this file, so a disagreement is one of the two being wrong.
instrument_disagreements <- gt |>
  join_one(printed_claims, "claim_id") |>
  mutate(
    # excheckr prints a truth value as 1 or 0 and a negative zero without its sign.
    expected = case_when(
      claim_type == "descriptive" & is.na(value_rewrite) & is.na(holds) ~ "NA",
      claim_type == "descriptive" & is.na(value_rewrite) ~ if_else(holds, "1", "0"),
      is.na(value_rewrite) ~ "NA",
      .default = str_replace(sprintf(str_c("%.", digits, "f"), value_rewrite),
                             "^-(0(\\.0*)?)$", "\\1")
    )
  ) |>
  filter(expected != value_in_text)

if (nrow(instrument_disagreements) > 0) {
  print(select(instrument_disagreements, claim_id, value_paper, value_rewrite, holds,
               expected, value_in_text), n = 40)
  stop(str_glue("The ground truth and maintained/in_text_claims.R disagree on ",
                "{nrow(instrument_disagreements)} claims."))
}

# Errata spine gate ----
# Every claim id an errata entry names has to exist here. A missing one is a typo or a
# claim that has since been renamed, and a published correction pointing at a row that is
# not in the table is a dangling reference the build should refuse to carry.
errata_spine <- read_csv(here::here("errata_entries.csv"),
                         col_types = cols(.default = col_character()))
cited_claim_ids <- errata_spine$claim_ids |>
  str_split(";") |>
  unlist() |>
  str_trim()
cited_claim_ids <- cited_claim_ids[!is.na(cited_claim_ids) & cited_claim_ids != ""]
if (length(setdiff(cited_claim_ids, gt$claim_id)) > 0) {
  print(setdiff(cited_claim_ids, gt$claim_id))
}
stopifnot(length(setdiff(cited_claim_ids, gt$claim_id)) == 0)

# Disposition: what became of each defect ----
# corrected_here is computed from the errata spine and never typed. Every other judgment
# restates what the README already says happened; a defect nobody has judged stays NA.
disposition_judgments <- tribble(
  ~claim_id, ~judgment, ~disposition_reason,
  "f1_pres_se", "not_corrected",
    "The README records that the deposited script typed Table 4's rounded 0.0085 into the Figure 1 label, which prints 0.009 against the fitted 0.008; the errata note carries no entry for it.",
  "results_ri_p_unadj", "documented_only",
    "The README records that the deposited randomization inference sets no seed, so the published p-value is one unrepeatable draw; the rewrite seeds it and the article's value is not wrong.",
  "results_ri_p_adj", "documented_only",
    "The README records that the deposited randomization inference sets no seed, so the published p-value is one unrepeatable draw; the rewrite seeds it and the article's value is not wrong.",
  "power_own_pct", "documented_only",
    "The README records that the deposited design diagnosis sets no seed, so the published power is one unrepeatable draw; the rewrite seeds it and the article's value is not wrong.",
  "app_g_power_own", "documented_only",
    "The README records that the deposited design diagnosis sets no seed, so the published power is one unrepeatable draw; the rewrite seeds it and the article's value is not wrong.",
  "app_g_power_combined", "documented_only",
    "The README records that the deposited design diagnosis sets no seed, so the published power is one unrepeatable draw; the rewrite seeds it and the article's value is not wrong.",
  "fg2_ols_power", "documented_only",
    "The README records that the deposited design diagnosis sets no seed, so the published power is one unrepeatable draw; the rewrite seeds it and the article's value is not wrong.",
  "fg2_post_power", "documented_only",
    "The README records that the deposited design diagnosis sets no seed, so the published power is one unrepeatable draw; the rewrite seeds it and the article's value is not wrong."
) |>
  bind_rows(tibble(
    claim_id = gt$claim_id[str_starts(gt$claim_id, "tc6_")],
    judgment = "documented_only",
    disposition_reason = "The README records that the deposited equivalence script stopped in lh_robust() under clustered CR2 errors and that the rewrite reproduces Table C.6 by the delta method; the published table is right."
  ))

gt <- gt |>
  left_join(disposition_judgments, by = "claim_id") |>
  mutate(
    disposition = case_when(
      is.na(defect_locus) ~ NA_character_,
      claim_id %in% cited_claim_ids ~ "corrected_here",
      .default = judgment
    ),
    disposition_reason = if_else(
      !is.na(disposition) & disposition != "corrected_here", disposition_reason,
      NA_character_
    )
  )

stopifnot(all(gt$disposition %in% c("corrected_here", "corrected_by_journal",
                                    "documented_only", "not_corrected") |
                is.na(gt$disposition)))

undisposed <- gt |> filter(defect_locus == "paper_internal", is.na(disposition))
if (nrow(undisposed) > 0) {
  print(select(undisposed, claim_id, location, claim), n = nrow(undisposed))
  stop(str_glue("{nrow(undisposed)} paper_internal rows carry no disposition."))
}
print(gt |> filter(!is.na(defect_locus)) |> count(defect_locus, disposition))

# Float coverage ----
# One row per published float, read off the extraction's locations. published_numbers
# counts the numbers the extraction transcribes from the float's face; covered counts the
# ones carried into the ground truth, and the two reproduced columns count those each
# pipeline matches. A figure's plotted quantities are counted by hand below, because no
# transcription records them.
extraction_floats <- published_claims |>
  mutate(float = str_remove(location, ",.*$")) |>
  filter(str_detect(float, "^(Table|Figure) [A-Z]?\\.?[0-9]+$")) |>
  count(float, name = "published_numbers")

# A Figure 1 panel is reproduced when its density's mean and standard deviation both match
# the label the panel prints.
figure_1_panels <- gt |>
  filter(str_detect(claim_id, "^f1_[a-z]+_(post_)?(mu|sd)$")) |>
  mutate(panel = str_extract(claim_id, "^f1_[a-z]+")) |>
  summarize(reproduced = all(match_rewrite == 1), .by = panel)

stopifnot(nrow(figure_1_panels) == 5)

figure_plots <- tribble(
  ~float, ~plotted, ~plotted_reproduced, ~note,
  "Figure 1", 5L, sum(figure_1_panels$reproduced),
    "Five normal densities, the prior and four posteriors; one is reproduced when both its mean and its standard deviation match the panel label.",
  "Figure D.1", 148L, 0L,
    "A map of the 148 ZIP codes with geographic boundaries, shaded by assignment; the rewrite does not redraw it.",
  "Figure G.2", 2L, 2L,
    "Two panels of simulated sampling distributions, reproduced as distributions rather than draw for draw, because the deposited simulation sets no seed."
)

table_notes <- c(
  "Table 2" = "Advertisement scripts and stills; the five numbers are quoted advertisement copy.",
  "Table 3" = "The advertisement vendor's exposure counts, which appear in no deposited file."
)

cell_counts <- gt |>
  summarize(
    covered = n(),
    reproduced_by_rewrite = sum(match_rewrite == 1, na.rm = TRUE),
    reproduced_by_archive = if_else(all(is.na(match)), NA_integer_,
                                    sum(match == 1, na.rm = TRUE)),
    .by = float
  )

float_coverage <- extraction_floats |>
  left_join(cell_counts, by = "float") |>
  left_join(figure_plots, by = "float") |>
  mutate(
    covered = replace_na(covered, 0L),
    reproduced_by_rewrite = replace_na(reproduced_by_rewrite, 0L),
    plotted_reproduced = as.integer(plotted_reproduced),
    note = coalesce(note, unname(table_notes[float]))
  ) |>
  arrange(str_detect(float, "\\."), str_extract(float, "[A-Z](?=\\.)"),
          as.integer(str_extract(float, "[0-9]+$")), desc(str_starts(float, "Table"))) |>
  select(float, published_numbers, covered, reproduced_by_rewrite, reproduced_by_archive,
         plotted, plotted_reproduced, note)

# Every figure has a plotted count, and every table but the two carrying no deposited
# quantity is covered cell for cell.
stopifnot(
  setequal(figure_plots$float, str_subset(float_coverage$float, "^Figure")),
  with(filter(float_coverage, str_starts(float, "Table"), !float %in% names(table_notes)),
       all(covered == published_numbers))
)

write_csv(float_coverage, here::here("ground_truth", "float_coverage.csv"))
print(float_coverage, n = nrow(float_coverage), width = 200)

# Write ----
# value_paper is committed as the string the article prints, not as a double. Stored
# numerically it would come back as 0.025 where the article printed 0.0250, and the printed
# precision is the only typographic fact any comparison here depends on. Every row here has
# an extraction row, so location is the extraction's.

gt <- gt |>
  arrange(claim_id, .locale = "en") |>
  mutate(across(c(match, match_rewrite, holds), as.logical))

write_csv(
  select(gt, paper_id, claim_id, location, claim_type, claim, value_paper, digits,
         value_script, match, value_rewrite, match_rewrite, holds, defect_locus,
         disposition, disposition_reason, notes),
  here::here("ground_truth", "coppock_green_porter_2022_ground_truth.csv")
)

print(count(gt, float, match_rewrite), n = 60)
print(count(gt, defect_locus))
print(count(published_claims, claim_type))

print(tibble(
  quantity = c("ground truth rows", "archive comparable", "archive matching",
               "rewrite comparable", "rewrite matching", "rewrite differing",
               "descriptive claims with a verdict", "descriptive claims holding",
               "extraction rows", "extraction rows needing a block",
               "claims printed by in_text_claims.R"),
  value = c(nrow(gt), sum(!is.na(gt$match)), sum(gt$match == 1, na.rm = TRUE),
            sum(!is.na(gt$match_rewrite)), sum(gt$match_rewrite == 1, na.rm = TRUE),
            sum(gt$match_rewrite == 0, na.rm = TRUE),
            sum(!is.na(gt$holds)), sum(gt$holds, na.rm = TRUE),
            nrow(published_claims), length(declared), nrow(printed_claims))
))

# Active Maintenance Report: coppock_green_porter_2022

2026-08-01

- [Summary](#summary)
  - [Does the deposited archive run?](#does-the-deposited-archive-run)
  - [Does the maintained rewrite reproduce the
    paper?](#does-the-maintained-rewrite-reproduce-the-paper)
- [Paper overview](#paper-overview)
- [Original archive reproducibility](#original-archive-reproducibility)
- [Errata](#errata)
- [Ground truth](#ground-truth)
- [Maintained rewrite](#maintained-rewrite)
- [Figure verification](#figure-verification)
- [Rewrite verification](#rewrite-verification)
- [R environment](#r-environment)

This repository holds the actively maintained replication code for
Coppock, Green and Porter (2022), together with the reproducibility
report that documents what the original archive did and did not do. It
is part of a program applying the maintenance proposal in Peer, Orr and
Coppock (2021, *PS: Political Science & Politics*, doi
[10.1017/S1049096521000366](https://doi.org/10.1017/S1049096521000366))
to a set of published archives.

|  |  |
|----|----|
| Article | [10.1177/20531680221076901](https://doi.org/10.1177/20531680221076901) |
| Replication archive | [10.7910/DVN/UH2NQW](https://doi.org/10.7910/DVN/UH2NQW) |
| Pre-analysis plan | [egap.org/registration/5312](https://egap.org/registration/5312), mirrored at [osf.io/ch4ms](https://osf.io/ch4ms) |

**The data are not redistributed here.** The deposit is 1.9 MB across 14
files and lives at Harvard Dataverse, which is the only copy this
repository points at. `download_original.R` fetches it and verifies
every file; `original_manifest.csv` pins the file identifiers, sizes and
checksums, so the exact bytes this code was written against are recorded
in version control even though the bytes themselves are not.

**Repository layout.** `maintained/` is the maintained rewrite: one
script per published table or figure, writing to `output/`, which is
committed so a reader can compare a fresh run against it without
downloading anything. `ground_truth/` ties every published number to the
code that produces it. `original/` is created by the download script and
is deliberately absent from the repository. This file is the
reproducibility report, also available as a PDF in `report/`.

**License.** CC0 1.0 Universal, matching the terms of the deposit this
repository maintains. See `LICENSE`.

**To reproduce.** Clone or download the repository, open
`coppock_green_porter_2022.Rproj`, and run:

``` r
source("run_all.R")
```

That fetches the deposit, verifies its 14 files, and produces every
table and figure into `maintained/output/`. Required packages:
tidyverse, estimatr, modelsummary, DeclareDesign, knitr, kableExtra,
here. Paths resolve through `here`, so nothing depends on the working
directory. The full run takes about two minutes, nearly all of it in the
two simulation scripts: 2000 re-randomizations for the randomization
inference and 5000 design simulations for Figure G.2. A successful run
overwrites `maintained/output/`, which is committed: **`git diff` on
that folder is the reproduction check.**

# Summary

Two questions, answered before the detail.

## Does the deposited archive run?

Almost. Eight of the nine analysis scripts execute without error on a
current R installation. The ninth, `CGP_2022_equivalence_tests.R`, stops
on its first model: it asks `lh_robust()` for a linear combination of
coefficients with clustered CR2 standard errors, and current `estimatr`
refuses, because `lh_robust()` has no CR2 path. Everything in appendix
Table C.6 therefore has no live source in the deposit, which is 30 of
the 210 recorded quantities.

Of what does run, every number matches the table it belongs to. Two
numbers in the article’s prose do not match the table they describe, but
the code is right and the sentence is wrong; see the errata. The one
class of quantity that cannot reproduce exactly is the simulated one:
neither the randomization inference nor the design diagnosis sets a
seed, so the archive does not return its own published p-values twice in
a row, let alone four years later. Both scripts also ship with the
number of simulations cut by a factor of four to ten below the value
used for the paper, with the published setting left commented out on the
line above.

## Does the maintained rewrite reproduce the paper?

Yes, with the exceptions that are the point of the exercise. 185 of the
192 verifiable ground truth claims match the published values to
reported precision. The 7 that do not are these:

- **Two are errors in the published text.** Describing Table 4’s first
  column, the article says the unadjusted estimate is “a 2.1 percentage
  point increase” with “a standard error of 3.0 percentage points”.
  Table 4 gives 0.0021 with a standard error of 0.0225, which is 0.21
  and 2.25 percentage points. The estimate in the text is off by a
  factor of ten and the standard error does not correspond to any model
  in the paper. The same sentence appears in the preprint, so it is not
  a typesetting accident, and it inflates the reported effect tenfold in
  the only place a reader meets it in prose.
- **Four are simulation draws.** The two randomization inference
  p-values and the two power figures differ from the published values by
  less than one Monte Carlo standard error. They cannot be matched
  exactly, because the archive sets no seed.
- **One is a figure label.** The published Figure 1 annotates this
  study’s standard error as 0.009 because the archive typed the table’s
  rounded 0.0085 into the figure. The rewrite reads the estimate from
  the Table 4 output instead, and 0.0084760 rounds to 0.008. Every
  posterior in the figure is unchanged.

The remaining 18 recorded quantities are unverifiable rather than
unmatched: 12 belong to a table the appendix never printed (see the
errata), four are precinct counts for a sample the deposit does not
contain, and two are advertisement-vendor exposure counts that were
never deposited in any form.

# Paper overview

**Citation**: Coppock, A., Green, D. P. and Porter, E. (2022). “Does
digital advertising affect vote choice? Evidence from a randomized field
experiment.” *Research & Politics*, 9(1). DOI: 10.1177/20531680221076901

**Summary**: A blocked, clustered field experiment run during the 2018
midterms tests whether pre-roll video advertisements on Facebook and
Instagram move vote choice. Two hundred and ten Florida ZIP codes across
four congressional districts were grouped into matched trios; one ZIP in
each trio was assigned to treatment and then to one of two
anti-Republican advertisements on gun policy, neither of which named a
candidate. The advertisements ran from 25 October 2018 until each
exhausted a \$30,000 budget, generating 1.1 million three-second views.
The outcome is the precinct-level Democratic two-party vote share from
the Florida Secretary of State, analysed on the 853 precincts that fall
wholly inside a study ZIP code and have outcome data, with CR2 standard
errors clustered by ZIP code and one-tailed randomization inference
under the sharp null. The unadjusted estimate is 0.21 percentage points
(SE 2.25); adjusting for the 2012, 2014 and 2016 vote shares brings it
to -0.04 points (SE 0.85). A Bayesian integration of this result with
the three prior field experiments on digital advertising leaves a
posterior centred on 0.7 percentage points with a standard deviation of
0.4.

# Original archive reproducibility

**Archive**: Harvard Dataverse,
[10.7910/DVN/UH2NQW](https://doi.org/10.7910/DVN/UH2NQW), published 5
January 2022. Fourteen files inside a `replication_archive` directory:
nine analysis scripts, two `.rds` data files, a README, a session log,
and an `.Rapp.history` file carrying one line of a coauthor’s local
path. The README names `tidyverse`, `DeclareDesign`, `xtable` and
`texreg` as the required add-ons and records R 4.1.1 under macOS Big
Sur.

| Script | Status | Note |
|:---|:---|:---|
| CGP_2022_in_text_calculations.R | Clean | Table 3 in-text totals and the cost-per-vote footnote |
| CGP_2022_precinct_level_analysis.R | Clean | Tables 4, A.1 and A.2 |
| CGP_2022_zip_code_level_analysis.R | Clean | Tables F.9, F.10 and F.11 |
| CGP_2022_precinct_level_analysis_CD.R | Clean | Tables B.3, B.4 and B.5 |
| CGP_2022_design_tests.R | Clean | Tables E.7 and F.8, plus the attrition check |
| CGP_2022_bayesian_learning.R | Clean | Figure 1 |
| CGP_2022_precinct_level_randomization_inference.R | Runs, but unseeded and at a quarter of the published simulations | The two randomization inference p-values in the main text |
| CGP_2022_design_diagnosis.R | Runs, but unseeded and at a tenth of the published simulations | Figure G.2 |
| CGP_2022_equivalence_tests.R | Fails on the first model | lh_robust() not available for CR2 standard errors |

Original archive, run script by script in a clean R session

**Classification**: resolvable. One script fails and the fix is
arithmetic rather than a change of method.

**The failure.** `lh_robust()` fits the model and then evaluates a
linear hypothesis on the fitted coefficients. Current `estimatr` will
not do this with CR2 standard errors, which are what `clusters =`
implies, so all six of the script’s models stop with
`lh_robust not available for CR2 standard errors`. The quantity wanted
is a difference between two coefficients in a model that `lm_robust()`
fits without complaint, so the rewrite takes the difference and its
standard error from the fitted coefficient vector and CR2
variance-covariance matrix directly. That is the same arithmetic
`lh_robust()` performed, and it returns Table C.6 exactly.

**Two claims in the archive that turn out to be false, or at least
stale.** Both simulation scripts carry a line of the form
`# simulations <- simulate_design(design, sims = 2000)` immediately
above the line that runs with `sims = 500`. The implication is that the
published numbers came from the larger run and that the smaller one is
there for the replicator’s convenience. Measured, the larger runs cost
32 seconds and 55 seconds on a 2026 laptop, so there is no convenience
to buy. The rewrite runs at the published simulation counts.

**Style-level issues, none fatal.** `rm(list = ls())` opens all nine
scripts. `%>%` throughout. `do(tibble(...))` in the Bayesian learning
script, superseded by `reframe()`. `texreg()` and `print.xtable()` write
LaTeX to standard output, so the tables in the paper were produced by
copying console output rather than by writing a file. Every path in the
scripts is bare, with a `# setwd() to replication archive` comment
standing in for the working directory.

**One deposited file is not part of the analysis.** `.Rapp.history`
contains a single `load()` call pointing at `/Users/eporter/Dropbox/...`
and a file, `CGP_2022_precinct_level_ep_rep.rds`, that is not in the
deposit. It is a stray console history, harmless but worth noticing,
since it is the only trace of the working directory the analysis was
actually run in.

# Errata

Four defects in the published record, none of which changes a
substantive conclusion, all of which the rewrite corrects or flags.

**1. The text misstates the unadjusted estimate by a factor of ten.**
“The first column shows the unadjusted difference-in-means estimate of
the treatment effect of any video: a 2.1 percentage point increase in
Democratic vote share, though this estimate is uncertain, as evidenced
by its large standard error of 3.0 percentage points.” Table 4 reports
0.0021 (0.0225). Converted to percentage points, that is 0.21 with a
standard error of 2.25. The estimate is stated at ten times its value,
and the standard error matches no model in the paper or appendix. The
sentence’s conclusion is unaffected, since both readings describe an
estimate that is small and swamped by its standard error, but the number
itself is wrong in the article and in the preprint.

**2. Appendix Table B.3 prints Table B.4.** The caption of B.3 reads
“Effects on vote share (CD fixed effects)”, and every cell beneath it is
identical to Table B.4, “Effects on vote margin (CD fixed effects)”: the
same coefficients, standard errors, R-squared values and covariate
labels, all on the vote margin scale. The vote share models with
congressional district fixed effects are in the archive but appear
nowhere in the appendix. The rewrite computes them;
`maintained/output/table_b3_vote_share_precinct_cd.csv` is where they
live. The estimate on any treatment video is 0.0042 (0.0217) unadjusted
and -0.0018 (0.0088) adjusted, which is the same substantive story as
Table 4.

**3. Covariate row labels are wrong in five appendix tables.** Tables
E.7 and F.8 regress the treatment indicator on lagged two-party *vote
margins* and label the rows “Two Party Vote Share”. Tables F.9, F.10 and
F.11 pass `texreg` a coefficient-name vector one label short of the
model, so every covariate row from 2016 down is labelled with the name
belonging to the row above it: the 2016 vote share coefficient is
printed as “Missingness Indicator (2016)”. No coefficient moves; only
its name does. The rewrite labels each row with the variable actually in
the model.

**4. Figure G.2’s panel annotation names the wrong effect size.** The
label reads “Power when PATE = 0.1”. The simulated effects are drawn
from a normal centred at 0.01, which is also what the appendix text says
two paragraphs above. The string is hardcoded in the archive script. The
rewrite prints 0.01.

# Ground truth

| Table or figure | Claims | Archive verifiable | Archive matches | Rewrite verifiable | Rewrite matches |
|:---|---:|---:|---:|---:|---:|
| figure_1 | 12 | 12 | 12 | 12 | 11 |
| figure_g2 | 2 | 2 | 0 | 2 | 0 |
| table_1 | 24 | 20 | 20 | 20 | 20 |
| table_4 | 14 | 14 | 14 | 14 | 14 |
| table_a1 | 14 | 14 | 14 | 14 | 14 |
| table_a2 | 13 | 13 | 13 | 13 | 13 |
| table_b3 | 12 | 0 | 0 | 0 | 0 |
| table_b4 | 13 | 13 | 13 | 13 | 13 |
| table_b5 | 13 | 13 | 13 | 13 | 13 |
| table_c6 | 30 | 0 | 0 | 30 | 30 |
| table_e7 | 6 | 6 | 6 | 6 | 6 |
| table_f10 | 13 | 13 | 13 | 13 | 13 |
| table_f11 | 13 | 13 | 13 | 13 | 13 |
| table_f8 | 5 | 5 | 5 | 5 | 5 |
| table_f9 | 13 | 13 | 13 | 13 | 13 |
| text | 13 | 11 | 7 | 11 | 7 |

Ground truth, by published object

Every value in the `value_paper` column of
`ground_truth/coppock_green_porter_2022_ground_truth.csv` was read off
the published article or its appendix. Where the paper does not state a
quantity, the column is blank and the row is marked unverifiable rather
than matched.

| Claim                            | Published | Rewrite |
|:---------------------------------|----------:|--------:|
| figure_1: present_study_se_label |     0.009 |  0.0085 |
| figure_g2: power_ols             |     0.210 |  0.2072 |
| figure_g2: power_posterior       |     0.893 |  0.8866 |
| text: unadjusted_estimate_pp     |     2.100 |  0.2114 |
| text: unadjusted_se_pp           |     3.000 |  2.2487 |
| text: ri_p_unadjusted            |     0.471 |  0.4585 |
| text: ri_p_adjusted              |     0.508 |  0.5050 |

The 7 claims the rewrite does not match

# Maintained rewrite

| Script | Produces |
|:---|:---|
| helpers.R | Packages, the cluster-count glance method, the table footer spec |
| table_1_sample_description.R | Table 1 |
| table_4_vote_share_precinct.R | Table 4 |
| table_a1_vote_margin_precinct.R | Table A.1 |
| table_a2_turnout_precinct.R | Table A.2 |
| table_b3_vote_share_precinct_cd.R | Table B.3, as it should have appeared |
| table_b4_vote_margin_precinct_cd.R | Table B.4 |
| table_b5_turnout_precinct_cd.R | Table B.5 |
| table_c6_equivalence_tests.R | Table C.6 |
| table_e7_balance_precinct.R | Table E.7 and its joint F test |
| table_f8_balance_zip.R | Table F.8 and its joint F test |
| table_f9_vote_share_zip.R | Table F.9 |
| table_f10_vote_margin_zip.R | Table F.10 |
| table_f11_turnout_zip.R | Table F.11 |
| figure_1_bayesian_learning.R | Figure 1 |
| figure_g2_design_diagnosis.R | Figure G.2 |
| text_in_text_calculations.R | The vote totals, posteriors and cost per vote quoted in the text |
| text_randomization_inference.R | The two randomization inference p-values quoted in the text |

Maintained rewrite: one script per published object

Every script writes a full-precision `.csv` of every coefficient it
estimates and, for the regression tables, a `.tex` rendering with the
same rows and columns as the published table, so a reader can compare
them line by line.

**Substitutions.**

| Archive | Rewrite | Why |
|----|----|----|
| `lh_robust()` with clusters | `lm_robust()` plus the contrast taken from the CR2 variance-covariance matrix | `lh_robust()` has no CR2 path; the arithmetic is identical |
| `texreg()` and `print.xtable()` to standard output | `modelsummary(output = )` | The table is written to a file rather than copied out of a console |
| `do(tibble(...))` | `reframe()` | `do()` is superseded |
| `DeclareDesign:::format_num()` | `sprintf()` | Unexported internals are not a stable interface |
| `separate()` | `separate_wider_delim()` | `separate()` is superseded |
| `%>%`, `ifelse()`, `rm(list = ls())` | `|>`, `if_else()`, omitted | House style |
| `sims = 500`, no seed | `sims = 2000` and `sims = 5000`, seeded | The published simulation counts, at a measured cost of 32 and 55 seconds |

**No published number is an input to a computation.** The archive’s
Figure 1 script typed this paper’s own estimate and standard error into
the vector of experiments being pooled, and the in-text script typed the
vote total and the posterior mean into the cost-per-vote formula. In the
rewrite, `figure_1_bayesian_learning.R` reads the estimate out of
`output/table_4_vote_share_precinct.csv`, and
`text_in_text_calculations.R` reads the posterior out of
`output/figure_1_bayesian_learning.csv` and the vote total out of the
ZIP-level data. The only constants entered by hand are the three prior
studies’ published estimates, which are other people’s results and
belong in the code, and the campaign’s budget, which is a design fact
rather than an estimate.

**The cost per vote depends on which posterior you use.** The article’s
footnote computes
$100{,}000 / (822{,}783 \times 0.007 \times 2) = \$8.68$ from the
rounded quantities it quotes. The unrounded posterior is 0.00708, and
the same calculation at full precision gives \$8.58. The rewrite reports
both, and the ground truth compares the first, since that is the number
in the paper.

# Figure verification

![Figure 1, maintained rewrite. Compare against the published figure at
doi
10.1177/20531680221076901.](maintained/output/figure_1_bayesian_learning.png)

| Panel | Estimate entering the update | Posterior mean | Posterior SD |
|:---|:---|:---|:---|
| Prior |  | 0.0000 | 0.0500 |
| Broockman and Green (2014) | 0.0160 | 0.0148 | 0.0135 |
| Turitto et al. (2014) | 0.0110 | 0.0137 | 0.0113 |
| Hager (2019) | 0.0090 | 0.0100 | 0.0053 |
| The present study | -0.0004 | 0.0071 | 0.0045 |

Figure 1, panel by panel

Every posterior matches the published panel labels: N(0.015, 0.013),
N(0.014, 0.011), N(0.010, 0.005) and N(0.007, 0.004). The one visible
difference is the standard error annotated on the last panel, which is
0.008 here and 0.009 in the article, for the reason given in the
summary.

![Figure G.2, maintained rewrite, at the published 5000
simulations.](maintained/output/figure_g2_design_diagnosis.png)

| Simulations | Mean OLS estimate | Power, OLS | Power, posterior |
|------------:|:------------------|:-----------|:-----------------|
|        5000 | 0.0097            | 0.207      | 0.887            |

Figure G.2 diagnosands

The appendix reports power of 0.210 for the OLS estimator and 0.893 for
the posterior. Both are within one Monte Carlo standard error of the
values above, which is as close as an unseeded simulation permits.

# Rewrite verification

| Table or figure | Claims | Matching | Not matching |
|:----------------|-------:|---------:|-------------:|
| figure_1        |     12 |       11 |            1 |
| figure_g2       |      2 |        0 |            2 |
| table_1         |     20 |       20 |            0 |
| table_4         |     14 |       14 |            0 |
| table_a1        |     14 |       14 |            0 |
| table_a2        |     13 |       13 |            0 |
| table_b4        |     13 |       13 |            0 |
| table_b5        |     13 |       13 |            0 |
| table_c6        |     30 |       30 |            0 |
| table_e7        |      6 |        6 |            0 |
| table_f10       |     13 |       13 |            0 |
| table_f11       |     13 |       13 |            0 |
| table_f8        |      5 |        5 |            0 |
| table_f9        |     13 |       13 |            0 |
| text            |     11 |        7 |            4 |

Rewrite against the published values

Beyond the ground truth, the `.tex` files in `maintained/output/`
reproduce the published tables cell for cell, including the covariate
rows, the R-squared values, the sample sizes and the significance stars.
Table 4, Tables A.1 and A.2, Tables B.4 and B.5, Tables E.7 and F.8, and
Tables F.9 through F.11 all match the published tables in every printed
cell.

**Determinism.** Every script other than the two simulation scripts is
deterministic and returns byte-identical output on a second run. The two
simulation scripts are seeded, so they are also reproducible run to run;
they are not reproducible against the archive, which set no seed.

# R environment

| Component     | Version |
|:--------------|:--------|
| R             | 4.6.0   |
| tidyverse     | 2.0.0   |
| estimatr      | 1.0.6   |
| modelsummary  | 2.6.0   |
| DeclareDesign | 1.1.1   |
| knitr         | 1.51    |
| kableExtra    | 1.4.0   |
| here          | 1.0.2   |

Versions this report was built against

The archive was written against R 4.1.1 with `estimatr` 0.30.2 and
`DeclareDesign` 0.28.0. The `lh_robust()` failure is the only place
where the four intervening years of package development broke something
outright.

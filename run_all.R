# coppock_green_porter_2022/run_all.R
# Runs the whole reproduction in order: fetch and verify the deposited archive, then
# every published table, then the figures, then the in-text quantities.
# Every script is self-contained and can also be run on its own, with the exception of
# figure_1_bayesian_learning.R, which reads the Table 4 output, and
# text_in_text_calculations.R, which reads the Figure 1 output.

library(here)
here::i_am("run_all.R")

# Deposited archive ----
# Downloads from Dataverse on a fresh clone; verifies checksums either way.
source(here::here("download_original.R"))

# Tables ----
source(here::here("maintained", "table_1_sample_description.R"))
source(here::here("maintained", "table_4_vote_share_precinct.R"))
source(here::here("maintained", "table_a1_vote_margin_precinct.R"))
source(here::here("maintained", "table_a2_turnout_precinct.R"))
source(here::here("maintained", "table_b3_vote_share_precinct_cd.R"))
source(here::here("maintained", "table_b4_vote_margin_precinct_cd.R"))
source(here::here("maintained", "table_b5_turnout_precinct_cd.R"))
source(here::here("maintained", "table_c6_equivalence_tests.R"))
source(here::here("maintained", "table_e7_balance_precinct.R"))
source(here::here("maintained", "table_f8_balance_zip.R"))
source(here::here("maintained", "table_f9_vote_share_zip.R"))
source(here::here("maintained", "table_f10_vote_margin_zip.R"))
source(here::here("maintained", "table_f11_turnout_zip.R"))

# Figures ----
# figure_1 reads the Table 4 estimates, so it runs after the tables.
# figure_g2 simulates the design 5000 times and takes about a minute.
source(here::here("maintained", "figure_1_bayesian_learning.R"))
source(here::here("maintained", "figure_g2_design_diagnosis.R"))

# In-text quantities ----
# text_in_text_calculations reads the Figure 1 posteriors, so it runs after the figures.
# text_randomization_inference re-randomizes the study 2000 times and takes about
# half a minute.
source(here::here("maintained", "text_in_text_calculations.R"))
source(here::here("maintained", "text_randomization_inference.R"))

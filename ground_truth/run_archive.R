# coppock_green_porter_2022/ground_truth/run_archive.R
# Output: ground_truth/archive_run_status.csv, ground_truth/archive_overwrites.csv
# Depends on: original/ (run download_original.R first)
# Description: Runs every deposited script, each in its own clean R session, in a
#   scratch copy of the deposit, and records how each one ends and what it leaves
#   behind in the directory it ran in.
#
#   The deposit is nine analysis scripts beside the two analysis-ready data files they
#   read. No deposited code builds those files, so there is nothing to withhold and
#   the stripped pass would run the same scripts on the same inputs. Only the
#   as_shipped pass is run.
#
#   Every script is run with the deposit's own directory as its working directory,
#   which is what the "# setwd() to replication archive" comment each one opens with
#   asks for. A copy is used so that nothing a script writes can reach original/.
#
#   The two simulation scripts run at the simulation counts the deposit ships (500),
#   not the counts left commented out above them.
#
#   This script is not part of run_all.R. It takes about two minutes, and its
#   result does not feed the ground truth: the archive values the ground truth reads
#   are the recorded measurement in archive_values.csv.

library(here)
library(tidyverse)
library(tools)

here::i_am("ground_truth/run_archive.R")

deposit <- here::here("original", "replication_archive")
stopifnot(dir.exists(deposit))

scripts <- c(
  "CGP_2022_in_text_calculations.R",
  "CGP_2022_precinct_level_analysis.R",
  "CGP_2022_zip_code_level_analysis.R",
  "CGP_2022_precinct_level_analysis_CD.R",
  "CGP_2022_design_tests.R",
  "CGP_2022_bayesian_learning.R",
  "CGP_2022_precinct_level_randomization_inference.R",
  "CGP_2022_design_diagnosis.R",
  "CGP_2022_equivalence_tests.R"
)

stopifnot(setequal(scripts, list.files(deposit, pattern = "\\.R$")))

timeout_seconds <- 600

# Scratch copy ----

work <- file.path(tempdir(), "replication_archive")
unlink(work, recursive = TRUE)
dir.create(work)
file.copy(list.files(deposit, full.names = TRUE, all.files = TRUE, no.. = TRUE), work)

snapshot <- function(dir) {
  files <- list.files(dir, all.files = TRUE, no.. = TRUE, recursive = TRUE)
  tibble(file = files, md5 = unname(md5sum(file.path(dir, files))),
         mtime = file.mtime(file.path(dir, files)))
}

before <- snapshot(work)

# Run ----
# The first line of standard error that starts with "Error" is where a script stopped;
# a timeout has no such line, so it is named instead.

run_script <- function(script) {
  log_file <- tempfile(fileext = ".log")
  status <- withr::with_dir(
    work,
    system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(script)),
            stdout = log_file, stderr = log_file, timeout = timeout_seconds)
  )
  log_lines <- readr::read_lines(log_file)
  error_line <- str_subset(log_lines, "^Error")
  tibble(
    script,
    exit_status = as.integer(status),
    outcome = case_when(
      status == 0 ~ "clean",
      status == 124 ~ "timeout",
      .default = "error"
    ),
    stopped_at = case_when(
      status == 0 ~ NA_character_,
      status == 124 ~ str_glue("killed after {timeout_seconds} seconds"),
      length(error_line) > 0 ~ str_squish(error_line[1]),
      .default = str_squish(tail(log_lines, 1))
    )
  )
}

runs <- map(scripts, run_script) |> list_rbind()

after <- snapshot(work)

# Write ----
# Every script here is an analysis script; the deposit carries no cleaning code and no
# helper that is only ever sourced.

run_status <- runs |>
  transmute(pass = "as_shipped", script, role = "analysis", exit_status, outcome,
            stopped_at)

overwrites <- full_join(before, after, by = "file", suffix = c("_before", "_after")) |>
  mutate(
    effect = case_when(
      is.na(md5_before) ~ "added",
      is.na(md5_after) ~ "deleted",
      mtime_after != mtime_before ~ "overwritten",
      .default = NA_character_
    ),
    matches_deposit = case_when(
      effect == "overwritten" ~ md5_after == md5_before,
      .default = NA
    )
  ) |>
  drop_na(effect) |>
  transmute(pass = "as_shipped", file, effect, matches_deposit)

write_csv(run_status, here::here("ground_truth", "archive_run_status.csv"))
write_csv(overwrites, here::here("ground_truth", "archive_overwrites.csv"))

print(run_status, width = 200)
print(overwrites)

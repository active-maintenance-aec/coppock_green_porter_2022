# coppock_green_porter_2022/maintained/helpers.R
# Output: none
# Depends on: nothing
# Description: Loads every package the rewrite uses and defines the two pieces of
#   shared machinery: a glance method that exposes the cluster count for lm_robust
#   fits, and the goodness-of-fit map that puts R2, N and N clusters at the foot of
#   every regression table.

library(here)
library(tidyverse)
library(estimatr)
library(modelsummary)
library(knitr)
library(kableExtra)
library(DeclareDesign)

options(
  modelsummary_factory_latex = "kableExtra",
  modelsummary_format_numeric_latex = "plain"
)

# lm_robust reports the number of clusters on the fit object but not in glance().
# The published tables all carry an "N Clusters" row, so modelsummary needs it.
glance_custom.lm_robust <- function(x, ...) {
  tibble(nclusters = if (is.null(x$nclusters)) NA_integer_ else x$nclusters)
}

# Goodness-of-fit rows, with R2 shown at whatever precision the published table used.
gof_map_lm <- function(r2_digits) {
  tibble::tribble(
    ~raw,         ~clean,       ~fmt,
    "r.squared",  "R2",         \(x) sprintf(paste0("%.", r2_digits, "f"), x),
    "nobs",       "Num. obs.",  \(x) sprintf("%.0f", x),
    "nclusters",  "N Clusters", \(x) sprintf("%.0f", x)
  )
}

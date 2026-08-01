# coppock_green_porter_2022/maintained/table_1_sample_description.R
# Output: output/table_1_sample_description.csv, output/table_1_sample_description.tex
# Depends on: CGP_2022_precinct_level.rds, CGP_2022_zip_code_level.rds, helpers.R
# Description: Reproduces Table 1, the count of precincts and ZIP codes in each
#   experimental condition under three sample definitions.

source(here::here("maintained", "helpers.R"))

precinct_level <- read_rds(
  here::here("original", "replication_archive", "CGP_2022_precinct_level.rds")
)
zip_code_level <- read_rds(
  here::here("original", "replication_archive", "CGP_2022_zip_code_level.rds")
)

# The deposit carries the 1096 precincts nested within a single ZIP code, not the
# 9235 precincts of the unrestricted sample, so the precinct counts in the first
# panel of Table 1 cannot be recomputed here. The ZIP counts can.
no_exclusions <- zip_code_level |>
  count(treatment, name = "zip_codes") |>
  mutate(precincts = NA_integer_, sample = "No exclusions")

nested <- precinct_level |>
  group_by(treatment) |>
  summarise(precincts = n(), zip_codes = n_distinct(vb_tsmart_zip), .groups = "drop") |>
  mutate(sample = "Only nested precincts")

with_outcome <- precinct_level |>
  filter(!is.na(dem_two_party_share_2018)) |>
  group_by(treatment) |>
  summarise(precincts = n(), zip_codes = n_distinct(vb_tsmart_zip), .groups = "drop") |>
  mutate(sample = "With outcome data")

results <- bind_rows(no_exclusions, nested, with_outcome) |>
  mutate(
    condition = recode_values(
      treatment,
      "control" ~ "Control",
      "video_1" ~ "Treatment video 1",
      "video_2" ~ "Treatment video 2"
    ),
    sample = factor(
      sample,
      levels = c("No exclusions", "Only nested precincts", "With outcome data")
    )
  ) |>
  select(sample, condition, precincts, zip_codes) |>
  arrange(sample, condition)

totals <- results |>
  group_by(sample) |>
  summarise(
    condition = "Total",
    precincts = if (all(is.na(precincts))) NA_integer_ else sum(precincts),
    zip_codes = sum(zip_codes),
    .groups = "drop"
  )

results <- bind_rows(results, totals) |>
  arrange(sample, condition == "Total", condition)

write_csv(results, here::here("maintained", "output", "table_1_sample_description.csv"))

wide <- results |>
  pivot_wider(
    names_from = sample,
    values_from = c(precincts, zip_codes),
    names_sep = ": "
  ) |>
  select(
    Condition = condition,
    `Precincts: No exclusions` = `precincts: No exclusions`,
    `ZIP codes: No exclusions` = `zip_codes: No exclusions`,
    `Precincts: Only nested` = `precincts: Only nested precincts`,
    `ZIP codes: Only nested` = `zip_codes: Only nested precincts`,
    `Precincts: With outcome data` = `precincts: With outcome data`,
    `ZIP codes: With outcome data` = `zip_codes: With outcome data`
  )

wide |>
  kable("latex", booktabs = TRUE,
        caption = "Table 1: Sample description at the ZIP code and voting precinct levels") |>
  kable_styling(latex_options = "hold_position") |>
  write_lines(here::here("maintained", "output", "table_1_sample_description.tex"))

print(wide)

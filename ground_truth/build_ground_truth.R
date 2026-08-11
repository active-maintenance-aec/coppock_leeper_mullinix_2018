# coppock_leeper_mullinix_2018/ground_truth/build_ground_truth.R
# Output: ground_truth/coppock_leeper_mullinix_2018_ground_truth.csv,
#   ground_truth/float_coverage.csv
# Depends on: maintained/output/ (run run_all.R first),
#   maintained/in_text_claims.R,
#   ground_truth/published_claims.csv,
#   ground_truth/published_appendix_values.csv,
#   original/replication_archive/{study_table.tex, group_table.tex,
#   appendix_tables.tex, CLM_results.rds, CLM_scatter_df.rds, CLM_f_tests_df.rds}
# Description: Assemble the comparison table, then run the coverage gate over the
#   second instrument. value_paper is the number the article or its appendix
#   prints, carried as the string the page carries, and comes only from those
#   documents: Table 1, Table 2 and the in-text quantities are transcribed below
#   from the typeset pages, and the 5,509 appendix cells were read off the
#   appendix PDF into published_appendix_values.csv. value_script is read out of
#   the deposit's own output files. value_rewrite is read out of
#   maintained/output/. No published number is an input to any computation here
#   or in maintained/.

library(here)
library(tidyverse)

here::i_am("ground_truth/build_ground_truth.R")

options(width = 200)

paper_id <- "coppock_leeper_mullinix_2018"

out <- function(f) read_csv(here::here("maintained", "output", f),
                            show_col_types = FALSE)
deposit <- function(f) here::here("original", "replication_archive", f)

# The extraction -------------------------------------------------------------------
# published_claims.csv is the exhaustive numeric-token extraction from the article
# and the appendix. It governs coverage for both instruments and is the single
# home of the per-claim precision, so neither file can name a different one.

published_claims <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(value_paper = col_character(), .default = col_guess())
)

# Rendering and comparison ----------------------------------------------------------

# "The string the article prints" means its digits, not its typography: the
# Unicode minus, thousands separators and a missing leading zero are normalised
# away, and the number of decimals, which is the one typographic fact the
# comparison needs, is preserved.
normalise_printed <- function(x) {
  x |>
    str_replace_all("−", "-") |>
    str_remove_all(",") |>
    str_replace("^(-?)\\.", "\\10")
}

# Signed zero is normalised on this side as well as in the claims file; whichever
# instrument normalises, both must.
render_at <- function(x, digits) {
  rendered <- sprintf(paste0("%.", digits, "f"), x)
  str_replace(rendered, "^-(0(\\.0+)?)$", "\\1")
}

printed_decimals <- function(x) {
  if_else(str_detect(x, "\\."), nchar(str_remove(x, "^.*\\.")), 0L)
}

# A value agrees when the pipeline's number, printed to the page's own precision,
# gives the same digits. The epsilon keeps a value sitting a hair from the
# rounding boundary from being rejected by floating point.
agrees <- function(value, value_paper, digits) {
  target <- suppressWarnings(as.numeric(normalise_printed(value_paper)))
  d <- if_else(is.na(digits), 0L, as.integer(digits))
  case_when(
    is.na(value) | is.na(target) | is.na(digits) ~ NA_real_,
    render_at(value, d) == normalise_printed(value_paper) ~ 1,
    abs(round(value, d) - target) < 1e-9 * pmax(1, abs(target)) ~ 1,
    .default = 0
  )
}

# Checks that depend only on the extraction ----------------------------------------
# These run before anything consumes it, so a wrong precision trips its own check
# rather than the value comparison downstream.

stopifnot(
  !any(duplicated(published_claims$claim_id)),
  all(nzchar(published_claims$claim_id)),
  all(published_claims$claim_type %in%
        c("pipeline", "descriptive", "definitional", "structural", "transcribed")),
  all(published_claims$needs_block %in% c(TRUE, FALSE)),
  all(is.na(published_claims$comparison) |
        published_claims$comparison %in% c("==", "<", ">", "<=", ">=", "approx")),
  all(published_claims$needs_block[
    published_claims$claim_type %in% c("pipeline", "descriptive")])
)

# A stored value_paper that does not survive a round trip through its own recorded
# precision means digits is wrong about the precision even where it is right about
# the value, which numeric equality would pass.
round_trips <- function(value_paper, digits) {
  numeric_rows <- !is.na(value_paper) & !is.na(digits) &
    str_detect(value_paper, "^-?\\d+(\\.\\d+)?$")
  stopifnot(
    all(value_paper[numeric_rows] == normalise_printed(value_paper[numeric_rows])),
    all(render_at(as.numeric(value_paper[numeric_rows]), digits[numeric_rows]) ==
          value_paper[numeric_rows])
  )
  invisible(NULL)
}

round_trips(published_claims$value_paper, published_claims$digits)

# The precision of a prose claim comes from the extraction and from nowhere else.
prose_digits <- function(id) {
  d <- published_claims$digits[match(id, published_claims$claim_id)]
  stopifnot(!any(is.na(d)))
  d
}

# The rewrite ----------------------------------------------------------------------

cate_estimates_rewrite <- out("cate_estimates.csv")
table_1_rewrite <- out("table_1_within_study_correspondence.csv")
table_2_rewrite <- out("table_2_across_study_correspondence.csv")
table_a_rewrite <- out("table_a_cate_estimates.csv")
text_rewrite <- out("text_correspondence_summary.csv")
scatter_rewrite <- out("cate_scatter.csv")
correlations_rewrite <- out("text_within_study_correlation.csv")
f_tests_rewrite <- out("f_tests.csv")
study_ns_rewrite <- out("study_ns.csv")
figure_1_rewrite <- out("figure_1_across_study_correspondence.csv")
figure_2_rewrite <- out("figure_2_within_study_correspondence.csv")

# The deposit's own output ----------------------------------------------------------
# Three deposited .tex files carry the numbers the archive produced in 2018, and
# three deposited .rds objects carry the estimates behind them. They are the
# archive's answer, independent of anything recomputed here.

study_table_script <- read_lines(deposit("study_table.tex")) |>
  keep(~ str_detect(.x, "\\\\\\\\")) |>
  str_match(paste0("^\\s*(.+?) & (\\d+) & (\\d+) & (-?[\\d.]+) \\((-?[\\d.]+)\\)",
                   " & \\[(-?[\\d.]+), (-?[\\d.]+)\\] & \\s*(\\d+) & ([\\d.]+)")) |>
  as_tibble(.name_repair = "minimal") |>
  set_names(c("raw", "study_label", "original_n", "mt_n", "slope", "slope_se",
              "ci_low", "ci_high", "n_comparisons", "f_p_value")) |>
  select(-raw) |>
  mutate(across(-study_label, as.numeric))

group_table_script <- read_lines(deposit("group_table.tex")) |>
  keep(~ str_detect(.x, "\\\\\\\\")) |>
  str_match(paste0("^\\s*(.+?) & (-?[\\d.]+) \\((-?[\\d.]+)\\)",
                   " & \\[(-?[\\d.]+), (-?[\\d.]+)\\] & \\s*(\\d+)")) |>
  as_tibble(.name_repair = "minimal") |>
  set_names(c("raw", "covariate_class", "slope", "slope_se", "ci_low",
              "ci_high", "n_comparisons")) |>
  select(-raw) |>
  mutate(across(-covariate_class, as.numeric))

# The appendix .tex interleaves two panels per study, with the first data row
# of each panel sharing a line with the panel header.
parse_appendix_tex <- function(path) {
  lines <- read_lines(path)
  study <- NA_character_
  sample <- NA_character_
  parsed <- list()
  for (ln in lines) {
    caption <- str_match(ln, "\\\\caption\\{(.+?) Original and Replication Results\\}")[, 2]
    if (!is.na(caption)) {
      study <- caption
      next
    }
    if (str_detect(ln, "\\{Original\\}")) sample <- "original"
    if (str_detect(ln, "\\{Mechanical Turk\\}")) sample <- "mt"
    body <- if (str_detect(ln, "\\\\midrule")) str_remove(ln, "^.*\\\\midrule") else ln
    m <- str_match(body, paste0("\\s*([A-Za-z][^&]*?) & (-?[\\d.]+) & ([\\d.]+) & ([\\d.]+)",
                                " & \\[(-?[\\d.]+), (-?[\\d.]+)\\] & (\\d+) & ([\\d.]+)"))
    if (!is.na(m[1, 1])) {
      parsed[[length(parsed) + 1]] <- tibble(
        study_label = study, sample = sample,
        covariate_class = str_trim(m[1, 2]),
        cate = as.numeric(m[1, 3]), se = as.numeric(m[1, 4]),
        p_value = as.numeric(m[1, 5]), ci_low = as.numeric(m[1, 6]),
        ci_high = as.numeric(m[1, 7]), n = as.numeric(m[1, 8]),
        prop = as.numeric(m[1, 9])
      )
    }
  }
  list_rbind(parsed)
}

appendix_script <- parse_appendix_tex(deposit("appendix_tables.tex"))

results_script <- as_tibble(ungroup(read_rds(deposit("CLM_results.rds"))))
scatter_script <- as_tibble(ungroup(read_rds(deposit("CLM_scatter_df.rds"))))
f_tests_script <- as_tibble(ungroup(read_rds(deposit("CLM_f_tests_df.rds"))))

# Identifiers ------------------------------------------------------------------------

slug <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_remove_all("^_|_$")
}

cell_names <- c(
  original_n = "original sample size", mt_n = "MTurk sample size",
  slope = "Deming slope", slope_se = "Deming slope SE",
  ci_low = "95% CI lower limit", ci_high = "95% CI upper limit",
  n_comparisons = "number of comparisons", f_p_value = "joint F test p-value"
)

long_cells <- function(d, key, value_name) {
  d |>
    pivot_longer(-all_of(key), names_to = "cell", values_to = value_name)
}

# Table 1, as printed ------------------------------------------------------------------
# Transcribed from page 2 of the published PDF, as strings, at the precision the
# page prints. The one cell whose precision differs from its column is Hiscox's
# Deming slope, which the journal typeset as 2.5 where the deposit's own
# study_table.tex writes 2.50. Study labels follow the deposit's spelling where
# the typeset table shortens one.
table_1_paper <- tribble(
  ~study_label, ~original_n, ~mt_n, ~slope, ~slope_se, ~ci_low, ~ci_high, ~n_comparisons, ~f_p_value,
  "Bergan (2012)", "1206", "1913", "0.75", "0.20", "0.37", "1.12", "16", "0.09",
  "Brader (2005)", "280", "1709", "3.56", "1.68", "-30.74", "37.86", "12", "0.69",
  "Brandt (2013)", "1225", "3131", "4.49", "1.96", "-6.25", "15.23", "13", "0.20",
  "Caprariello and Reis (2013)", "825", "2729", "-4.38", "2.00", "-7.83", "-0.93", "16", "0.63",
  "Chong and Druckman (2010)", "958", "1400", "0.17", "0.18", "-0.58", "0.92", "13", "0.61",
  "Craig and Richeson (2014)", "608", "847", "-0.95", "0.36", "-1.56", "-0.34", "16", "0.24",
  "Denny (2012)", "1733", "1913", "2.83", "1.04", "1.19", "4.47", "16", "0.59",
  "Epley et al. (2009)", "1019", "1913", "0.68", "0.64", "-2.52", "3.88", "10", "0.14",
  "Flavin (2011)", "2015", "2729", "0.23", "0.20", "-0.15", "0.62", "16", "0.06",
  "Gash and Murakami (2009)", "1022", "3131", "2.78", "1.01", "1.59", "3.96", "16", "0.73",
  "Hiscox (2006)", "1610", "2972", "2.5", "1.07", "0.94", "4.07", "16", "0.96",
  "Hopkins and Mummolo (2017)", "3266", "2972", "-1.84", "0.85", "-4.06", "0.37", "16", "0.27",
  "Jacobsen, Snyder and Saultz (2014)", "1111", "3171", "-4.73", "1.99", "-8.32", "-1.14", "16", "0.09",
  "Johnston and Ballard (2016)", "2045", "2985", "0.13", "0.53", "-0.26", "0.53", "16", "0.06",
  "Levendusky and Malhotra (2015)", "1053", "1987", "-0.16", "0.35", "-1.50", "1.19", "16", "0.01",
  "McGinty, Webster and Barry (2013)", "2935", "2985", "2.53", "1.10", "1.09", "3.97", "16", "0.72",
  "Murtagh et al. (2012)", "2112", "3131", "0.34", "0.34", "-0.20", "0.88", "10", "0.98",
  "Nicholson (2012)", "781", "1099", "-23.05", "16.21", "-396.69", "350.58", "12", "0.94",
  "Parmer (2011)", "521", "3277", "1.71", "0.75", "-0.12", "3.54", "16", "0.61",
  "Pedulla (2014)", "1407", "1913", "-57.93", "17.90", "-363.42", "247.55", "15", "0.73",
  "Peffley and Hurwitz (2007)", "905", "1285", "2.23", "1.17", "-1.08", "5.54", "13", "0.19",
  "Piazza (2015)", "1135", "3171", "-2.15", "0.74", "-3.83", "-0.47", "16", "0.81",
  "Shafer (2017)", "2592", "2729", "-24.13", "9.75", "-162.31", "114.05", "16", "0.49",
  "Thompson and Schlehofer (2014)", "591", "3277", "0.24", "0.60", "-1.08", "1.56", "16", "0.68",
  "Transue (2007)", "345", "367", "-1.67", "1.02", "-7.52", "4.17", "7", "0.29",
  "Turaga (2010)", "774", "3277", "1.44", "0.54", "-0.86", "3.74", "16", "0.73",
  "Wallace (2011)", "2929", "2729", "4.74", "1.86", "-7.96", "17.43", "16", "0.00"
)

table_1_rows <- table_1_paper |>
  long_cells("study_label", "value_paper") |>
  left_join(long_cells(study_table_script, "study_label", "value_script"),
            by = c("study_label", "cell")) |>
  left_join(
    table_1_rewrite |>
      select(study_label, all_of(names(cell_names))) |>
      long_cells("study_label", "value_rewrite"),
    by = c("study_label", "cell")
  ) |>
  transmute(
    claim_id = paste0("table_1_", slug(study_label), "_", cell),
    table_figure = "Table 1",
    claim = paste0(study_label, ", ", cell_names[cell]),
    value_script, value_paper, digits = printed_decimals(value_paper),
    value_rewrite, holds = NA, defect_locus = NA_character_, notes = ""
  )

# Table 2, as printed ------------------------------------------------------------------
# Transcribed from page 3 of the published PDF. Row labels follow the deposit's
# spelling, which capitalises where the typeset table does not.
table_2_paper <- tribble(
  ~covariate_class, ~slope, ~slope_se, ~ci_low, ~ci_high, ~n_comparisons,
  "Age: 18 - 39", "0.82", "0.08", "0.51", "1.12", "27",
  "Age: 40 - 59", "0.86", "0.08", "0.55", "1.17", "27",
  "Age: More than 60", "0.95", "0.13", "0.61", "1.29", "26",
  "Less than College", "0.93", "0.06", "0.61", "1.25", "26",
  "College", "0.87", "0.10", "0.46", "1.29", "26",
  "Graduate School", "0.72", "0.13", "0.28", "1.15", "26",
  "Men", "0.87", "0.07", "0.49", "1.25", "26",
  "Women", "0.91", "0.07", "0.61", "1.20", "26",
  "Liberal", "0.71", "0.11", "0.37", "1.05", "20",
  "Moderate", "1.01", "0.11", "0.49", "1.53", "20",
  "Conservative", "0.75", "0.09", "0.52", "0.99", "20",
  "Democrat", "0.88", "0.07", "0.50", "1.26", "24",
  "Independent", "0.94", "0.16", "0.19", "1.68", "23",
  "Republican", "0.86", "0.08", "0.63", "1.08", "24",
  "Nonwhite", "0.95", "0.11", "0.45", "1.46", "25",
  "White", "0.92", "0.06", "0.66", "1.18", "27"
)

table_2_rows <- table_2_paper |>
  long_cells("covariate_class", "value_paper") |>
  left_join(long_cells(group_table_script, "covariate_class", "value_script"),
            by = c("covariate_class", "cell")) |>
  left_join(
    table_2_rewrite |>
      select(covariate_class, slope, slope_se, ci_low, ci_high, n_comparisons) |>
      long_cells("covariate_class", "value_rewrite"),
    by = c("covariate_class", "cell")
  ) |>
  transmute(
    claim_id = paste0("table_2_", slug(covariate_class), "_", cell),
    table_figure = "Table 2",
    claim = paste0(covariate_class, ", ", cell_names[cell]),
    value_script, value_paper, digits = printed_decimals(value_paper),
    value_rewrite, holds = NA, defect_locus = NA_character_, notes = ""
  )

# Appendix Tables 1 to 27 ----------------------------------------------------------------
# 787 rows of seven cells each. Each table contributes three rows to the ground
# truth: the five estimation cells, the N column and the Prop column. Every
# appendix column is typeset at a fixed precision, two decimals throughout except
# the integer N, so the parsed transcription carries no less information than the
# page does.
appendix_paper <- read_csv(here::here("ground_truth", "published_appendix_values.csv"),
                           show_col_types = FALSE)

appendix_digits <- c(cate = 2, se = 2, p_value = 2, ci_low = 2, ci_high = 2,
                     n = 0, prop = 2)

appendix_joined <- appendix_paper |>
  left_join(table_a_rewrite,
            by = c("appendix_table", "study_label", "sample", "covariate_class"),
            suffix = c("_paper", "_rewrite")) |>
  left_join(appendix_script,
            by = c("study_label", "sample", "covariate_class"),
            suffix = c("", "_script")) |>
  rename(cate_script = cate, se_script = se, p_value_script = p_value,
         ci_low_script = ci_low, ci_high_script = ci_high,
         n_script = n, prop_script = prop)

stopifnot(nrow(appendix_joined) == nrow(appendix_paper),
          !any(is.na(appendix_joined$cate_rewrite)),
          !any(is.na(appendix_joined$cate_script)))

cell_agrees <- function(value, paper, column) {
  agrees(value, render_at(paper, appendix_digits[[column]]),
         appendix_digits[[column]])
}

appendix_cells <- appendix_joined |>
  transmute(
    appendix_table, study_label, sample, covariate_class,
    estimation_script = cell_agrees(cate_script, cate_paper, "cate") *
      cell_agrees(se_script, se_paper, "se") *
      cell_agrees(p_value_script, p_value_paper, "p_value") *
      cell_agrees(ci_low_script, ci_low_paper, "ci_low") *
      cell_agrees(ci_high_script, ci_high_paper, "ci_high"),
    estimation_rewrite = cell_agrees(cate_rewrite, cate_paper, "cate") *
      cell_agrees(se_rewrite, se_paper, "se") *
      cell_agrees(p_value_rewrite, p_value_paper, "p_value") *
      cell_agrees(ci_low_rewrite, ci_low_paper, "ci_low") *
      cell_agrees(ci_high_rewrite, ci_high_paper, "ci_high"),
    n_script_ok = cell_agrees(n_script, n_paper, "n"),
    n_rewrite_ok = cell_agrees(n_rewrite, n_paper, "n"),
    prop_script_ok = cell_agrees(prop_script, prop_paper, "prop"),
    prop_rewrite_ok = cell_agrees(prop_rewrite, prop_paper, "prop")
  )

appendix_rows <- appendix_cells |>
  summarize(
    n_rows = n(),
    estimation_script = 5 * sum(estimation_script),
    estimation_rewrite = 5 * sum(estimation_rewrite),
    n_script_ok = sum(n_script_ok),
    n_rewrite_ok = sum(n_rewrite_ok),
    prop_script_ok = sum(prop_script_ok),
    prop_rewrite_ok = sum(prop_rewrite_ok),
    .by = c(appendix_table, study_label)
  ) |>
  arrange(appendix_table) |>
  (\(d) bind_rows(
    d |> transmute(
      claim_id = paste0("appendix_table_", appendix_table, "_estimation"),
      table_figure = paste0("Appendix Table ", appendix_table),
      claim = paste0(study_label, ", CATE, SE, p-value and confidence limit cells reproduced"),
      value_script = estimation_script, value_paper = as.character(5 * n_rows),
      digits = 0, value_rewrite = estimation_rewrite, holds = NA,
      defect_locus = NA_character_, notes = ""
    ),
    d |> transmute(
      claim_id = paste0("appendix_table_", appendix_table, "_n"),
      table_figure = paste0("Appendix Table ", appendix_table),
      claim = paste0(study_label, ", N cells reproduced"),
      value_script = n_script_ok, value_paper = as.character(n_rows),
      digits = 0, value_rewrite = n_rewrite_ok, holds = NA,
      defect_locus = if_else(n_rewrite_ok < n_rows, "archive", NA_character_),
      notes = if_else(
        n_rewrite_ok < n_rows,
        paste0("The published N is the residual degrees of freedom plus two, ",
               "which undercounts by one for every regressor beyond treatment ",
               "and the intercept. The rewrite reports the number of ",
               "observations the fit used."),
        ""
      )
    ),
    d |> transmute(
      claim_id = paste0("appendix_table_", appendix_table, "_prop"),
      table_figure = paste0("Appendix Table ", appendix_table),
      claim = paste0(study_label, ", Prop cells reproduced"),
      value_script = prop_script_ok, value_paper = as.character(n_rows),
      digits = 0, value_rewrite = prop_rewrite_ok, holds = NA,
      defect_locus = if_else(prop_rewrite_ok < n_rows, "archive", NA_character_),
      notes = if_else(
        prop_rewrite_ok < n_rows,
        paste0("The published Prop divides each subgroup's N by a total that ",
               "counts every subgroup once per regression term, so it is the ",
               "share of the sample divided by the number of terms. The ",
               "rewrite reports the share itself, which sums to one within ",
               "each covariate."),
        ""
      )
    )
  ))() |>
  arrange(table_figure, claim)

# In-text quantities -------------------------------------------------------------------
# One row per prose claim the extraction marks as needing a block. value_paper is
# transcribed here independently of published_claims.csv and reconciled against it
# in the gate below; the precision comes from the extraction and from nowhere else.

# The archive's answers, from its own deposited estimates rather than from the
# summary the rewrite writes.
script_levels <- results_script |>
  summarize(levels = n_distinct(group), .by = covariate)

script_age_bounds <- results_script |>
  filter(covariate == "age") |>
  distinct(group) |>
  arrange(group) |>
  pull(group) |>
  str_extract_all("\\d+") |>
  map(~ as.numeric(.x[-1]))

script_alpha <- c(
  max(scatter_script$p_diff_in_cates[scatter_script$`Difference in CATES` == "Significant"]),
  min(scatter_script$p_diff_in_cates[scatter_script$`Difference in CATES` != "Significant"])
)

script_level <- results_script |>
  filter(std.error > 1e-12) |>
  mutate(level = 2 * pt((conf.high - estimate) / std.error, df) - 1)

# The rewrite's answers. Where the ground truth can reach a quantity without going
# through text_correspondence_summary.csv it does, so that the summary is checked
# rather than assumed.
rewrite_levels <- cate_estimates_rewrite |>
  summarize(levels = n_distinct(group), .by = covariate)

rewrite_age_bounds <- cate_estimates_rewrite |>
  filter(covariate == "age") |>
  distinct(group) |>
  arrange(group) |>
  pull(group) |>
  str_extract_all("\\d+") |>
  map(~ as.numeric(.x[-1]))

rewrite_alpha <- c(
  max(scatter_rewrite$p_diff_in_cates[scatter_rewrite$diff_in_cates_significant]),
  min(scatter_rewrite$p_diff_in_cates[!scatter_rewrite$diff_in_cates_significant])
)

rewrite_level <- cate_estimates_rewrite |>
  filter(std.error > 1e-12) |>
  mutate(level = 2 * pt((conf.high - estimate) / std.error, df) - 1)

pull_text <- function(q) text_rewrite$value[text_rewrite$quantity == q]

excludes_one_rewrite <- table_2_rewrite |> filter(ci_low > 1 | ci_high < 1)
within_study_correlations <- correlations_rewrite |> filter(study != "all")
published_prop_sums <- table_a_rewrite |>
  mutate(attribute = str_split_i(covariate_class, ":", 1)) |>
  summarize(share = sum(prop_deposit_formula), .by = c(study_label, sample, attribute))
within_version_spread <- cate_estimates_rewrite |>
  filter(term == "Z") |>
  summarize(sd_cate = sd(estimate), .by = c(study, sample))
adjusted_studies <- cate_estimates_rewrite |>
  filter(!term %in% c("(Intercept)", "Z")) |>
  distinct(study_label)

# A note that states a number is a claim like any other, so the numbers in these
# notes are the same computation that sets the row's verdict rather than a second
# copy of it.
f2 <- function(x) sprintf("%.2f", x)

text_rows <- tribble(
  ~claim_id, ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~holds, ~defect_locus, ~notes,

  "abstract_study_pairs", "Abstract", "Study pairs analysed",
    n_distinct(results_script$study), "27", pull_text("Study pairs"), NA, NA_character_,
    "\"We analyze subgroup conditional average treatment effects using 27 original-replication study pairs\"",

  "abstract_survey_responses", "Abstract", "Individual survey responses",
    sum(study_table_script$original_n) + sum(study_table_script$mt_n), "101745",
    pull_text("Total survey responses"), NA, NA_character_,
    paste0("\"(encompassing 101,745 individual survey responses)\". The total is the sum of ",
           "Table 1's two sample size columns. For the sixteen studies taken from Mullinix et al. (2015) ",
           "those columns count the whole survey wave rather than the respondents assigned to the ",
           "experiment, and the same MTurk waves are counted once per study, so the figure is not the ",
           "number of observations the estimates use. See study_ns.csv."),

  "significance_study_pairs", "Significance statement", "Survey experiments replicated",
    n_distinct(results_script$study), "27", pull_text("Study pairs"), NA, NA_character_,
    "\"We replicated 27 survey experiments\"",

  "significance_survey_responses", "Significance statement", "Individual survey responses",
    sum(study_table_script$original_n) + sum(study_table_script$mt_n), "101745",
    pull_text("Total survey responses"), NA, NA_character_,
    "\"(encompassing 101,745 individual survey responses)\". The same quantity the abstract states.",

  "methods_study_pairs", "Methods and Materials", "Original-replication pairs reanalysed",
    n_distinct(results_script$study), "27", pull_text("Study pairs"), NA, NA_character_,
    "\"through reanalyses of 27 original-replication pairs collected by refs. 12 and 13\"",

  "methods_subgroups", "Methods and Materials", "Distinct subgroups",
    n_distinct(results_script$group), "16", n_distinct(table_a_rewrite$covariate_class),
    NA, NA_character_,
    "\"among 16 distinct subgroups defined by subjects' pretreatment background characteristics\"",

  "methods_table1_sample_sizes", "Methods and Materials", "Table 1 reports the analysis sample sizes",
    NA, NA_character_, NA,
    all(study_ns_rewrite$reported_total_n == study_ns_rewrite$analysis_obs_total_n),
    "paper_internal",
    str_glue("\"The full list of studies, with the sample sizes used in the analyses reported ",
             "here, is presented in Table 1.\" Table 1's N columns count respondents and total ",
             "{sum(study_ns_rewrite$reported_total_n)}, which is the figure the abstract gives ",
             "for individual survey responses. The fits use ",
             "{sum(study_ns_rewrite$analysis_obs_total_n)} observations, the respondents assigned ",
             "to the two arms each treatment contrast compares: the reported sample is larger in ",
             "{sum(study_ns_rewrite$analysis_obs_total_n < study_ns_rewrite$reported_total_n)} of ",
             "the {nrow(study_ns_rewrite)} pairs, equal in ",
             "{sum(study_ns_rewrite$analysis_obs_total_n == study_ns_rewrite$reported_total_n)}, ",
             "and smaller in ",
             "{sum(study_ns_rewrite$analysis_obs_total_n > study_ns_rewrite$reported_total_n)}. ",
             "The article contradicts itself rather than merely being imprecise: Table 1 reports ",
             "{study_ns_rewrite$reported_original_n[study_ns_rewrite$study_label == 'Bergan (2012)']} ",
             "for Bergan (2012)'s original sample where every attribute in the corresponding ",
             "panel of appendix Table 1 sums to ",
             "{study_ns_rewrite$analysis_obs_original_n[study_ns_rewrite$study_label == 'Bergan (2012)']}."),

  "methods_cate_difference_in_means", "Methods and Materials",
    "CATEs estimated by difference-in-means", NA, NA_character_, NA,
    NA, NA_character_,
    str_glue("\"We estimate all CATEs via difference-in-means\", repeated in the appendix. ",
             "{n_distinct(adjusted_studies$study_label)} of ",
             "{n_distinct(cate_estimates_rewrite$study)} study pairs fit a regressor beyond ",
             "treatment, and in every case it is another arm of the study's own factorial design or ",
             "the party identification its assignment probabilities depend on. Recorded as an ",
             "approximation with no verdict."),

  "methods_separate_experiments", "Methods and Materials", "Separate experiments",
    n_distinct(paste(results_script$study, results_script$sample)), "54",
    pull_text("Separate experiments"), NA, NA_character_,
    "\"the varied experimental protocols for each of the 54 separate experiments (27 study pairs)\"",

  "methods_study_pairs_parenthetical", "Methods and Materials", "Study pairs, same sentence",
    n_distinct(results_script$study), "27", pull_text("Study pairs"), NA, NA_character_,
    "\"the 54 separate experiments (27 study pairs) reanalyzed here\"",

  "methods_attributes", "Methods and Materials", "Pretreatment attributes",
    n_distinct(results_script$covariate), "6", n_distinct(cate_estimates_rewrite$covariate),
    NA, NA_character_,
    "\"We have identified six attributes that are measured in nearly all studies\"",

  "methods_max_categories", "Methods and Materials", "Largest number of categories per attribute",
    max(script_levels$levels), "3", max(rewrite_levels$levels), NA, NA_character_,
    "\"we have coarsened each to a maximum of three categories to maintain comparability across studies\"",

  "methods_age_band_1_low", "Methods and Materials", "Youngest age band, lower bound",
    script_age_bounds[[1]][1], "18", rewrite_age_bounds[[1]][1], NA, NA_character_,
    "\"age (18 to 39, 40 to 59, 60+)\"",

  "methods_age_band_1_high", "Methods and Materials", "Youngest age band, upper bound",
    script_age_bounds[[1]][2], "39", rewrite_age_bounds[[1]][2], NA, NA_character_,
    "Same sentence",

  "methods_age_band_2_low", "Methods and Materials", "Middle age band, lower bound",
    script_age_bounds[[2]][1], "40", rewrite_age_bounds[[2]][1], NA, NA_character_,
    "Same sentence",

  "methods_age_band_2_high", "Methods and Materials", "Middle age band, upper bound",
    script_age_bounds[[2]][2], "59", rewrite_age_bounds[[2]][2], NA, NA_character_,
    "Same sentence",

  "methods_age_band_3_low", "Methods and Materials", "Oldest age band, lower bound",
    script_age_bounds[[3]][1], "60", rewrite_age_bounds[[3]][1], NA, NA_character_,
    "Same sentence",

  "footnote_se_formula", "Results footnote", "Standard error of the difference-in-CATEs",
    NA, NA_character_, NA,
    all(near(scatter_rewrite$std.error_diff_in_cates^2,
             scatter_rewrite$std.error_mt^2 + scatter_rewrite$std.error_original^2)),
    NA_character_,
    str_glue("\"We estimate the standard error of the difference-in-CATEs as the square root of ",
             "SE_1 squared plus SE_2 squared.\" The stated formula reproduces every one of the ",
             "{nrow(scatter_rewrite)} standard errors of the difference."),

  "footnote_normal_approximation", "Results footnote", "Tests under a normal approximation",
    NA, NA_character_, NA,
    all(near(scatter_rewrite$p_diff_in_cates,
             2 * pnorm(abs(scatter_rewrite$est_diff_in_cates /
                             scatter_rewrite$std.error_diff_in_cates), lower.tail = FALSE))),
    NA_character_,
    "\"and conduct hypothesis tests under a normal approximation\"",

  "footnote_alpha", "Results footnote", "Significance threshold for the difference",
    mean(script_alpha), "0.05", mean(rewrite_alpha), NA, NA_character_,
    paste0("\"We deem a difference statistically significant if the P value is less than 0.05.\" ",
           "The threshold is not printed anywhere in the output, so it is recovered from where the ",
           "classification changes."),

  "results_opportunities", "Results", "Comparison opportunities",
    nrow(scatter_script), "393", nrow(figure_1_rewrite), NA, NA_character_,
    "\"Out of 393 opportunities, the difference-in-CATEs is significant 59 times, or 15% of the time.\"",

  "results_diff_significant", "Results", "Difference-in-CATEs significant",
    sum(scatter_script$`Difference in CATES` == "Significant"), "59",
    sum(figure_1_rewrite$difference == "Significant"), NA, NA_character_, "Same sentence",

  "results_diff_share", "Results", "Share of comparisons significant",
    100 * mean(scatter_script$`Difference in CATES` == "Significant"), "15",
    100 * mean(figure_1_rewrite$difference == "Significant"), NA, NA_character_,
    "Same sentence. Carried on the article's percentage scale.",

  "results_sign_disagreements", "Results", "Sign disagreements with both versions significant",
    sum(sign(scatter_script$estimate_mt) != sign(scatter_script$estimate_original) &
          scatter_script$sig_mt == "Significant" & scatter_script$sig_original == "Significant"),
    "0", pull_text("Sign disagreements with both versions significant"), NA, NA_character_,
    "\"In 0 of 393 opportunities do the CATEs have different signs while both being statistically distinguishable from 0.\"",

  "results_sign_opportunities", "Results", "Comparison opportunities, same sentence",
    nrow(scatter_script), "393", nrow(scatter_rewrite), NA, NA_character_, "Same sentence",

  "results_significant_original", "Results", "CATEs significant in the original version",
    sum(scatter_script$sig_original == "Significant"), "156",
    pull_text("CATEs significant in the original version"), NA, NA_character_,
    "\"Of the 156 CATEs that were significantly different from no effect in the original, 118 are significantly different from no effect in the MTurk replication.\"",

  "results_significant_both", "Results", "Of those, significant in the MTurk version",
    sum(scatter_script$sig_original == "Significant" & scatter_script$sig_mt == "Significant"),
    "118", pull_text("Of those, significant in the MTurk version"), NA, NA_character_,
    "Same sentence",

  "results_null_original", "Results", "CATEs indistinguishable from zero in the original version",
    sum(scatter_script$sig_original == "Not Significant"), "237",
    pull_text("CATEs indistinguishable from zero in the original version"), NA, NA_character_,
    "\"Of the 237 CATEs that were statistically indistinguishable from no effect in the original, 158 were statistically indistinguishable from 0 in the MTurk version.\"",

  "results_null_both", "Results", "Of those, indistinguishable in the MTurk version",
    sum(scatter_script$sig_original == "Not Significant" & scatter_script$sig_mt == "Not Significant"),
    "158", pull_text("Of those, indistinguishable in the MTurk version"), NA, NA_character_,
    "Same sentence",

  "results_match_rate", "Results", "Overall significance match rate",
    100 * mean(scatter_script$sig_original == scatter_script$sig_mt), "70",
    100 * pull_text("Overall significance match rate"), NA, NA_character_,
    "\"The overall 'significance match' rate is therefore 70%.\" Carried on the article's percentage scale.",

  "results_slopes_all_positive", "Results", "Across-study slopes all positive",
    NA, NA_character_, NA, all(table_2_rewrite$slope > 0), NA_character_,
    "\"The slopes are all positive, ranging from 0.71 to 1.01.\"",

  "results_slope_min", "Results", "Smallest across-study slope",
    min(group_table_script$slope), "0.71", min(table_2_rewrite$slope), NA, NA_character_,
    "Same sentence",

  "results_slope_max", "Results", "Largest across-study slope",
    max(group_table_script$slope), "1.01", max(table_2_rewrite$slope), NA, NA_character_,
    "Same sentence",

  "results_intervals_excluding_one", "Results", "Across-study intervals excluding one",
    sum(group_table_script$ci_low > 1 | group_table_script$ci_high < 1), "1",
    nrow(excludes_one_rewrite), NA, NA_character_,
    "\"All but one of the 95% CIs include 1 ... The CI for the conservative group (just barely) excludes 1\"",

  "results_conservative_excludes_one", "Results", "The excluding interval is the conservative group",
    NA, NA_character_, NA,
    nrow(excludes_one_rewrite) == 1 && excludes_one_rewrite$covariate_class == "Conservative",
    NA_character_,
    str_glue("\"The CI for the conservative group (just barely) excludes 1\". The interval that ",
             "excludes one belongs to {str_flatten_comma(excludes_one_rewrite$covariate_class)}, ",
             "at [{f2(excludes_one_rewrite$ci_low)}, {f2(excludes_one_rewrite$ci_high)}]."),

  "results_mostly_uncorrelated", "Results", "Within-study correlation of CATEs across versions",
    NA, NA_character_, NA, NA, NA_character_,
    str_glue("\"The CATEs in the original study are mostly uncorrelated with the CATEs in the ",
             "MTurk versions.\" The article prints no number. The ",
             "{nrow(within_study_correlations)} within-study correlations run from ",
             "{f2(min(within_study_correlations$correlation))} to ",
             "{f2(max(within_study_correlations$correlation))} with a median of ",
             "{f2(median(within_study_correlations$correlation))} and are positive in ",
             "{sum(within_study_correlations$correlation > 0)} of ",
             "{nrow(within_study_correlations)} pairs; see text_within_study_correlation.csv."),

  "results_within_smaller", "Results", "Within-study slopes smaller than across-study slopes",
    NA, NA_character_, NA, NA, NA_character_,
    str_glue("\"We see within-study slopes that are smaller than the across-study slopes and slopes ",
             "of both signs.\" The article prints no number. The within-study slopes have a median ",
             "of {f2(median(table_1_rewrite$slope))} against ",
             "{f2(median(table_2_rewrite$slope))} across studies; ",
             "{sum(table_1_rewrite$slope < min(table_2_rewrite$slope))} of ",
             "{nrow(table_1_rewrite)} fall below every across-study slope and ",
             "{sum(table_1_rewrite$slope > max(table_2_rewrite$slope))} rise above every one, so ",
             "the claim holds of the centre of the distribution and not of its tails. Recorded as ",
             "an approximation with no verdict."),

  "results_both_signs", "Results", "Within-study slopes of both signs",
    NA, NA_character_, NA,
    any(table_1_rewrite$slope < 0) && any(table_1_rewrite$slope > 0), NA_character_,
    "\"and slopes of both signs\"",

  "results_cates_clustered", "Results", "CATEs tightly clustered within a study version",
    NA, NA_character_, NA, NA, NA_character_,
    str_glue("\"Most of the CATEs are tightly clustered around the overall average treatment effect ",
             "in each study version.\" The article prints no number. Within a study version the ",
             "subgroup effects have a standard deviation of ",
             "{f2(median(within_version_spread$sd_cate))} on the standardized outcome at the ",
             "median, running {f2(min(within_version_spread$sd_cate))} to ",
             "{f2(max(within_version_spread$sd_cate))}. Recorded as an approximation with no verdict."),

  "results_f_fail", "Results", "F tests failing to reject at 0.05",
    sum(f_tests_script$p_value > 0.05), "25", sum(f_tests_rewrite$p_value > 0.05),
    NA, NA_character_,
    "\"we fail to reject the null hypothesis most of the time (25 of 27 opportunities)\"",

  "results_f_opportunities", "Results", "Study pairs offering a joint F test",
    nrow(f_tests_script), "27", nrow(f_tests_rewrite), NA, NA_character_, "Same sentence",

  "discussion_study_pairs", "Discussion", "Pairs of survey experiments",
    n_distinct(results_script$study), "27", pull_text("Study pairs"), NA, NA_character_,
    "\"Drawing on a fine-grained analysis of 27 pairs of survey experiments\"",

  "discussion_limited_moderation", "Discussion", "Limited evidence of moderation",
    NA, NA_character_, NA, NA, NA_character_,
    str_glue("\"We find only limited evidence that such moderation occurs and, when it does, the ",
             "differences in effect sizes across groups are small.\" The article prints no number. ",
             "{sum(scatter_rewrite$diff_in_cates_significant)} of {nrow(scatter_rewrite)} ",
             "comparisons differ significantly and the absolute difference in CATEs has a median ",
             "of {f2(median(abs(scatter_rewrite$est_diff_in_cates)))} standard deviations. ",
             "Recorded as an approximation with no verdict."),

  "appendix_p_two_sided", "Appendix front matter", "p-values are two-sided",
    NA, NA_character_, NA,
    all(near(cate_estimates_rewrite$p.value,
             2 * pt(-abs(cate_estimates_rewrite$statistic), cate_estimates_rewrite$df))),
    NA_character_,
    "\"All p-values are two-sided and are not corrected for multiple comparisons.\"",

  "appendix_prop_definition", "Appendix front matter", "Prop is the share of subjects in the class",
    NA, NA_character_, NA,
    all(near(table_a_rewrite$prop_deposit_formula, table_a_rewrite$prop)), "archive",
    str_glue("\"The Prop column describes what proportion of the subjects in a given experiment ",
             "belong to the associated covariate class.\" Every printed Prop is that share divided ",
             "by the number of terms in the regression that produced the row, so an attribute's ",
             "classes sum to between {f2(min(published_prop_sums$share))} and ",
             "{f2(max(published_prop_sums$share))} rather than to 1.00, across ",
             "{nrow(published_prop_sums)} attribute-by-study-version blocks. The deposit's own code ",
             "produces exactly what the appendix prints, so the fault is in the deposit rather than ",
             "in the transcription."),

  "appendix_ci_level", "Appendix table headers", "Confidence level of the CATE intervals",
    100 * max(script_level$level), "95", 100 * max(rewrite_level$level), NA, NA_character_,
    "\"95% CI\", printed once in each of the 54 appendix panels."
) |>
  mutate(digits = prose_digits(claim_id), .after = value_paper)

# Figures ------------------------------------------------------------------------------
figure_rows <- tribble(
  ~claim_id, ~table_figure, ~claim, ~value_script, ~value_paper, ~digits, ~value_rewrite, ~holds, ~defect_locus, ~notes,
  "figure_1_plotted_coordinates", "Figure 1", "Plotted points and confidence limits",
    NA, NA_character_, NA, NA, NA, NA_character_,
    paste0("The figure prints no numbers. All ", nrow(figure_1_rewrite),
           " plotted comparisons and their six coordinates agree with the deposit's own ",
           "CLM_scatter_df.rds to better than 1e-12; see figure_1_across_study_correspondence.csv."),
  "figure_2_plotted_coordinates", "Figure 2", "Plotted points and confidence limits",
    NA, NA_character_, NA, NA, NA, NA_character_,
    paste0("The same ", nrow(figure_2_rewrite),
           " comparisons panelled by study rather than by subgroup; see ",
           "figure_2_within_study_correspondence.csv.")
)

# Assemble -------------------------------------------------------------------------------
ground_truth <- bind_rows(table_1_rows, table_2_rows, appendix_rows, text_rows,
                          figure_rows) |>
  mutate(
    paper_id = paper_id,
    match = agrees(value_script, value_paper, digits),
    match_rewrite = agrees(value_rewrite, value_paper, digits)
  ) |>
  select(paper_id, claim_id, table_figure, claim, value_script, value_paper,
         digits, match, value_rewrite, match_rewrite, holds, defect_locus, notes)

stopifnot(!any(duplicated(ground_truth$claim_id)))

# The locus rule, in three states -----------------------------------------------------
# An adverse row must carry a defect_locus, a clean match must not, and a row with
# no verdict may. A gate stated on match_rewrite alone cannot see either an
# archive failure the rewrite survives or a descriptive claim that does not hold.

adverse <- with(ground_truth,
                (!is.na(match) & match == 0) |
                  (!is.na(match_rewrite) & match_rewrite == 0) |
                  (!is.na(holds) & !holds))
clean <- with(ground_truth,
              !adverse & ((!is.na(match_rewrite) & match_rewrite == 1) |
                            (!is.na(holds) & holds)))

if (any(adverse & is.na(ground_truth$defect_locus))) {
  print(ground_truth |> filter(adverse & is.na(defect_locus)) |>
          select(claim_id, value_paper, value_rewrite, match, match_rewrite, holds),
        n = Inf)
  stop("An adverse row carries no defect_locus.")
}
if (any(clean & !is.na(ground_truth$defect_locus))) {
  print(ground_truth |> filter(clean & !is.na(defect_locus)) |>
          select(claim_id, value_paper, value_rewrite, match_rewrite, holds, defect_locus),
        n = Inf)
  stop("A clean match carries a defect_locus.")
}
stopifnot(all(is.na(ground_truth$defect_locus) |
                ground_truth$defect_locus %in%
                c("paper_internal", "archive", "environment", "rewrite", "unresolved")))

# The extraction against the ground truth ------------------------------------------------
# Two hand transcriptions of the same pages, and nothing else compares them.

reconcile <- published_claims |>
  filter(!is.na(value_paper)) |>
  select(claim_id, extraction = value_paper, digits) |>
  inner_join(ground_truth |> select(claim_id, transcription = value_paper),
             by = "claim_id")

stopifnot(nrow(reconcile) == sum(!is.na(published_claims$value_paper) &
                                   published_claims$claim_id %in% ground_truth$claim_id))
if (!all(normalise_printed(reconcile$extraction) ==
           normalise_printed(reconcile$transcription))) {
  print(reconcile |> filter(normalise_printed(extraction) !=
                              normalise_printed(transcription)), n = Inf)
  stop("The extraction and the ground truth disagree about a published value.")
}

# Float coverage --------------------------------------------------------------------------
# The extraction records how many numbers each published float prints; the ground
# truth records how many of them are covered and how many reproduce. Every float
# reads its own rows.

appendix_covered <- appendix_cells |>
  summarize(
    published_numbers = 7 * n(),
    covered = 7 * n(),
    reproduced_by_rewrite = 5 * sum(estimation_rewrite) + sum(n_rewrite_ok) +
      sum(prop_rewrite_ok),
    reproduced_by_archive = 5 * sum(estimation_script) + sum(n_script_ok) +
      sum(prop_script_ok),
    .by = appendix_table
  ) |>
  transmute(float = paste0("appendix_table_", appendix_table),
            published_numbers, covered, reproduced_by_rewrite, reproduced_by_archive)

main_covered <- tribble(
  ~float, ~published_numbers, ~covered, ~reproduced_by_rewrite, ~reproduced_by_archive,
  "table_1", nrow(table_1_rows), nrow(table_1_rows),
    sum(ground_truth$match_rewrite[ground_truth$table_figure == "Table 1"]),
    sum(ground_truth$match[ground_truth$table_figure == "Table 1"]),
  "table_2", nrow(table_2_rows), nrow(table_2_rows),
    sum(ground_truth$match_rewrite[ground_truth$table_figure == "Table 2"]),
    sum(ground_truth$match[ground_truth$table_figure == "Table 2"]),
  "figure_1", 0L, 0L, NA_integer_, NA_integer_,
  "figure_2", 0L, 0L, NA_integer_, NA_integer_
)

float_coverage <- bind_rows(main_covered, appendix_covered)

declared_floats <- published_claims |>
  filter(str_starts(claim_id, "float_")) |>
  transmute(float = str_remove(claim_id, "^float_"),
            declared = if_else(is.na(value_paper), 0L, as.integer(value_paper)))

float_check <- declared_floats |> full_join(float_coverage, by = "float")
stopifnot(nrow(float_check) == nrow(declared_floats),
          !any(is.na(float_check$published_numbers)))
if (!all(float_check$declared == float_check$published_numbers)) {
  print(float_check |> filter(declared != published_numbers), n = Inf)
  stop("A float prints a different number of cells from the count the extraction declares.")
}

# The coverage gate -------------------------------------------------------------------------
# The second instrument is read as a program, not as text: it is run, its output is
# captured, and the printed claim lines are counted. A block that errors, or that
# prints nothing, satisfies a textual gate completely and fails this one. Its own
# environment, because both files necessarily read the same outputs and name
# objects for what they hold.

claims_output <- capture.output(
  source(here::here("maintained", "in_text_claims.R"), local = new.env(), echo = FALSE)
)

printed <- claims_output |>
  str_subset("^CLAIM ") |>
  str_match("^CLAIM ([^ ]+) = (.*?) \\|\\| (.*)$")
printed_claims <- tibble(claim_id = printed[, 2], printed_value = printed[, 3],
                         label = printed[, 4])

required <- published_claims |> filter(needs_block)

missing_blocks <- setdiff(required$claim_id, printed_claims$claim_id)
unknown_blocks <- setdiff(printed_claims$claim_id, published_claims$claim_id)
if (length(missing_blocks) > 0 || length(unknown_blocks) > 0) {
  print(list(missing = missing_blocks, unknown = unknown_blocks))
  stop("in_text_claims.R does not print exactly the claims the extraction requires.")
}
if (nrow(printed_claims) != nrow(required)) {
  print(printed_claims |> count(claim_id) |> filter(n > 1))
  stop("in_text_claims.R printed ", nrow(printed_claims), " claims against ",
       nrow(required), " extraction rows requiring a block.")
}

# Cross-instrument comparison. The two files reach the same claimed number by
# separate paths from the same pipeline outputs; where they disagree, one of them
# is wrong.
cross <- printed_claims |>
  left_join(ground_truth |> select(claim_id, value_rewrite, holds), by = "claim_id") |>
  left_join(published_claims |> select(claim_id, digits, comparison, claim_type),
            by = "claim_id") |>
  mutate(
    expected = pmap_chr(
      list(claim_type, holds, value_rewrite, digits, comparison),
      function(type, holds_value, value, digits, comparison) {
        if (!is.na(comparison) && comparison == "approx") return(NA_character_)
        if (type == "descriptive") return(as.character(holds_value))
        if (is.na(value) || is.na(digits)) return(NA_character_)
        render_at(value, digits)
      }
    ),
    agrees = is.na(expected) | printed_value == expected
  )

if (!all(cross$agrees)) {
  print(cross |> filter(!agrees) |> select(claim_id, printed_value, expected), n = Inf)
  stop("The two instruments disagree about a claimed value.")
}

# Errata spine gate ---------------------------------------------------------------------------
# Every claim id an errata entry names has to exist here. A missing one is a typo or a claim
# that has since been renamed, and a published correction pointing at a row that is not in the
# table is a dangling reference the build should refuse to carry.

errata_path <- here::here("errata_entries.csv")
if (file.exists(errata_path)) {
  errata_spine <- read_csv(errata_path, show_col_types = FALSE)
  cited_claim_ids <- errata_spine$claim_ids |>
    str_split(";") |>
    unlist() |>
    str_trim()
  cited_claim_ids <- cited_claim_ids[!is.na(cited_claim_ids) & cited_claim_ids != ""]
  if (length(setdiff(cited_claim_ids, ground_truth$claim_id)) > 0) {
    print(setdiff(cited_claim_ids, ground_truth$claim_id))
  }
  stopifnot(length(setdiff(cited_claim_ids, ground_truth$claim_id)) == 0)
}

# Write ---------------------------------------------------------------------------------------

write_csv(float_coverage, here::here("ground_truth", "float_coverage.csv"))
write_csv(ground_truth,
          here::here("ground_truth", paste0(paper_id, "_ground_truth.csv")))

print(paste("rows:", nrow(ground_truth),
            "| match = 1:", sum(ground_truth$match == 1, na.rm = TRUE),
            "| match = 0:", sum(ground_truth$match == 0, na.rm = TRUE),
            "| match = NA:", sum(is.na(ground_truth$match))))
print(paste("match_rewrite = 1:", sum(ground_truth$match_rewrite == 1, na.rm = TRUE),
            "| match_rewrite = 0:", sum(ground_truth$match_rewrite == 0, na.rm = TRUE),
            "| match_rewrite = NA:", sum(is.na(ground_truth$match_rewrite))))
print(ground_truth |> count(holds))
print(ground_truth |> filter(!is.na(defect_locus)) |> count(defect_locus))
print(ground_truth |> filter(match_rewrite == 0) |>
        select(table_figure, claim, value_paper, value_rewrite, defect_locus),
      n = 100, width = 200)
print(paste(nrow(printed_claims), "claims printed by the second instrument against",
            nrow(required), "extraction rows requiring a block;",
            sum(float_coverage$published_numbers), "published cells transcribed."))

# coppock_leeper_mullinix_2018/ground_truth/build_ground_truth.R
# Output: ground_truth/coppock_leeper_mullinix_2018_ground_truth.csv
# Depends on: maintained/output/ (run run_all.R first),
#   ground_truth/published_appendix_values.csv,
#   original/replication_archive/{study_table.tex, group_table.tex,
#   appendix_tables.tex, CLM_scatter_df.rds, CLM_f_tests_df.rds}
# Description: Assemble the comparison table. value_paper is the number the
#   article or its appendix prints, and comes only from those documents: the
#   two main tables and the in-text quantities are transcribed below from the
#   typeset pages, and the 5,509 appendix cells were read off the appendix PDF
#   into published_appendix_values.csv. value_script is read out of the
#   deposit's own output files. value_rewrite is read out of maintained/output/.
#   No published number is an input to any computation here or in maintained/.

library(here)
library(tidyverse)

here::i_am("ground_truth/build_ground_truth.R")

options(width = 200)

out <- function(f) read_csv(here::here("maintained", "output", f),
                            show_col_types = FALSE)
deposit <- function(f) here::here("original", "replication_archive", f)

table_1_rewrite <- out("table_1_within_study_correspondence.csv")
table_2_rewrite <- out("table_2_across_study_correspondence.csv")
table_a_rewrite <- out("table_a_cate_estimates.csv")
text_rewrite <- out("text_correspondence_summary.csv")
scatter_rewrite <- out("cate_scatter.csv")
figure_1_rewrite <- out("figure_1_across_study_correspondence.csv")
figure_2_rewrite <- out("figure_2_within_study_correspondence.csv")

# Agreement at the precision the article prints ----
printed_decimals <- function(x) {
  map_dbl(x, function(v) {
    if (is.na(v)) return(NA_real_)
    txt <- formatC(v, format = "fg", digits = 15, flag = "#")
    txt <- str_remove(txt, "0+$")
    if (str_detect(txt, "\\.")) nchar(str_remove(txt, "^.*\\.")) else 0
  })
}

agrees <- function(value, target) {
  case_when(
    is.na(value) | is.na(target) ~ NA_real_,
    abs(value - target) <= 0.5 * 10^(-printed_decimals(target)) + 1e-9 ~ 1,
    .default = 0
  )
}

# The deposit's own output ----
# Three deposited .tex files carry the numbers the archive produced in 2018.
# They are the archive's answer, independent of anything recomputed here.

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

# The deposit's derived data objects, from which the in-text quantities follow.
scatter_script <- as_tibble(ungroup(read_rds(deposit("CLM_scatter_df.rds"))))
f_tests_script <- as_tibble(ungroup(read_rds(deposit("CLM_f_tests_df.rds"))))

# Table 1, as printed in the article ----
# Transcribed from page 2 of the published PDF. Study labels follow the
# deposit's spelling where the typeset table shortens one.
table_1_paper <- tribble(
  ~study_label, ~original_n, ~mt_n, ~slope, ~slope_se, ~ci_low, ~ci_high, ~n_comparisons, ~f_p_value,
  "Bergan (2012)", 1206, 1913, 0.75, 0.20, 0.37, 1.12, 16, 0.09,
  "Brader (2005)", 280, 1709, 3.56, 1.68, -30.74, 37.86, 12, 0.69,
  "Brandt (2013)", 1225, 3131, 4.49, 1.96, -6.25, 15.23, 13, 0.20,
  "Caprariello and Reis (2013)", 825, 2729, -4.38, 2.00, -7.83, -0.93, 16, 0.63,
  "Chong and Druckman (2010)", 958, 1400, 0.17, 0.18, -0.58, 0.92, 13, 0.61,
  "Craig and Richeson (2014)", 608, 847, -0.95, 0.36, -1.56, -0.34, 16, 0.24,
  "Denny (2012)", 1733, 1913, 2.83, 1.04, 1.19, 4.47, 16, 0.59,
  "Epley et al. (2009)", 1019, 1913, 0.68, 0.64, -2.52, 3.88, 10, 0.14,
  "Flavin (2011)", 2015, 2729, 0.23, 0.20, -0.15, 0.62, 16, 0.06,
  "Gash and Murakami (2009)", 1022, 3131, 2.78, 1.01, 1.59, 3.96, 16, 0.73,
  "Hiscox (2006)", 1610, 2972, 2.5, 1.07, 0.94, 4.07, 16, 0.96,
  "Hopkins and Mummolo (2017)", 3266, 2972, -1.84, 0.85, -4.06, 0.37, 16, 0.27,
  "Jacobsen, Snyder and Saultz (2014)", 1111, 3171, -4.73, 1.99, -8.32, -1.14, 16, 0.09,
  "Johnston and Ballard (2016)", 2045, 2985, 0.13, 0.53, -0.26, 0.53, 16, 0.06,
  "Levendusky and Malhotra (2015)", 1053, 1987, -0.16, 0.35, -1.50, 1.19, 16, 0.01,
  "McGinty, Webster and Barry (2013)", 2935, 2985, 2.53, 1.10, 1.09, 3.97, 16, 0.72,
  "Murtagh et al. (2012)", 2112, 3131, 0.34, 0.34, -0.20, 0.88, 10, 0.98,
  "Nicholson (2012)", 781, 1099, -23.05, 16.21, -396.69, 350.58, 12, 0.94,
  "Parmer (2011)", 521, 3277, 1.71, 0.75, -0.12, 3.54, 16, 0.61,
  "Pedulla (2014)", 1407, 1913, -57.93, 17.90, -363.42, 247.55, 15, 0.73,
  "Peffley and Hurwitz (2007)", 905, 1285, 2.23, 1.17, -1.08, 5.54, 13, 0.19,
  "Piazza (2015)", 1135, 3171, -2.15, 0.74, -3.83, -0.47, 16, 0.81,
  "Shafer (2017)", 2592, 2729, -24.13, 9.75, -162.31, 114.05, 16, 0.49,
  "Thompson and Schlehofer (2014)", 591, 3277, 0.24, 0.60, -1.08, 1.56, 16, 0.68,
  "Transue (2007)", 345, 367, -1.67, 1.02, -7.52, 4.17, 7, 0.29,
  "Turaga (2010)", 774, 3277, 1.44, 0.54, -0.86, 3.74, 16, 0.73,
  "Wallace (2011)", 2929, 2729, 4.74, 1.86, -7.96, 17.43, 16, 0.00
)

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
    table_figure = "Table 1",
    claim = paste0(study_label, ", ", cell_names[cell]),
    value_script, value_paper, value_rewrite,
    defect_locus = NA_character_,
    notes = ""
  )

# Table 2, as printed in the article ----
# Transcribed from page 3 of the published PDF.
table_2_paper <- tribble(
  ~covariate_class, ~slope, ~slope_se, ~ci_low, ~ci_high, ~n_comparisons,
  "Age: 18 - 39", 0.82, 0.08, 0.51, 1.12, 27,
  "Age: 40 - 59", 0.86, 0.08, 0.55, 1.17, 27,
  "Age: More than 60", 0.95, 0.13, 0.61, 1.29, 26,
  "Less than College", 0.93, 0.06, 0.61, 1.25, 26,
  "College", 0.87, 0.10, 0.46, 1.29, 26,
  "Graduate School", 0.72, 0.13, 0.28, 1.15, 26,
  "Men", 0.87, 0.07, 0.49, 1.25, 26,
  "Women", 0.91, 0.07, 0.61, 1.20, 26,
  "Liberal", 0.71, 0.11, 0.37, 1.05, 20,
  "Moderate", 1.01, 0.11, 0.49, 1.53, 20,
  "Conservative", 0.75, 0.09, 0.52, 0.99, 20,
  "Democrat", 0.88, 0.07, 0.50, 1.26, 24,
  "Independent", 0.94, 0.16, 0.19, 1.68, 23,
  "Republican", 0.86, 0.08, 0.63, 1.08, 24,
  "Nonwhite", 0.95, 0.11, 0.45, 1.46, 25,
  "White", 0.92, 0.06, 0.66, 1.18, 27
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
    table_figure = "Table 2",
    claim = paste0(covariate_class, ", ", cell_names[cell]),
    value_script, value_paper, value_rewrite,
    defect_locus = NA_character_,
    notes = ""
  )

# Appendix Tables 1 to 27 ----
# 787 rows of seven cells each. Each table contributes three rows to the ground
# truth: the five estimation cells, the N column and the Prop column.
appendix_paper <- read_csv(here::here("ground_truth", "published_appendix_values.csv"),
                           show_col_types = FALSE)

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

stopifnot(!any(is.na(appendix_joined$cate_rewrite)),
          !any(is.na(appendix_joined$cate_script)))

appendix_cells <- appendix_joined |>
  transmute(
    appendix_table, study_label, sample, covariate_class,
    estimation_script = agrees(cate_script, cate_paper) *
      agrees(se_script, se_paper) * agrees(p_value_script, p_value_paper) *
      agrees(ci_low_script, ci_low_paper) * agrees(ci_high_script, ci_high_paper),
    estimation_rewrite = agrees(cate_rewrite, cate_paper) *
      agrees(se_rewrite, se_paper) * agrees(p_value_rewrite, p_value_paper) *
      agrees(ci_low_rewrite, ci_low_paper) * agrees(ci_high_rewrite, ci_high_paper),
    n_script_ok = agrees(n_script, n_paper),
    n_rewrite_ok = agrees(n_rewrite, n_paper),
    prop_script_ok = agrees(prop_script, prop_paper),
    prop_rewrite_ok = agrees(prop_rewrite, prop_paper)
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
      table_figure = paste0("Appendix Table ", appendix_table),
      claim = paste0(study_label, ", CATE, SE, p-value and confidence limit cells reproduced"),
      value_script = estimation_script, value_paper = 5 * n_rows,
      value_rewrite = estimation_rewrite,
      defect_locus = NA_character_, notes = ""
    ),
    d |> transmute(
      table_figure = paste0("Appendix Table ", appendix_table),
      claim = paste0(study_label, ", N cells reproduced"),
      value_script = n_script_ok, value_paper = n_rows,
      value_rewrite = n_rewrite_ok,
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
      table_figure = paste0("Appendix Table ", appendix_table),
      claim = paste0(study_label, ", Prop cells reproduced"),
      value_script = prop_script_ok, value_paper = n_rows,
      value_rewrite = prop_rewrite_ok,
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

# In-text quantities ----
# Each value_paper is the number the sentence quoted beside it prints.
pull_text <- function(q) text_rewrite$value[text_rewrite$quantity == q]

sig_script <- table(scatter_script$sig_original, scatter_script$sig_mt)
n_script_comparisons <- nrow(scatter_script)

text_rows <- tribble(
  ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~defect_locus, ~notes,
  "Abstract", "Study pairs analysed", 27, 27, pull_text("Study pairs"), NA_character_,
    "\"We analyze subgroup conditional average treatment effects using 27 original-replication study pairs\"",
  "Abstract", "Individual survey responses", sum(study_table_script$original_n) + sum(study_table_script$mt_n),
    101745, pull_text("Total survey responses"), NA_character_,
    paste0("\"(encompassing 101,745 individual survey responses)\". The total is the sum of ",
           "Table 1's two sample size columns. For the sixteen studies taken from Mullinix et al. (2015) ",
           "those columns count the whole survey wave rather than the respondents assigned to the ",
           "experiment, and the same MTurk waves are counted once per study, so the figure is not the ",
           "number of observations the estimates use. See study_ns.csv."),
  "Methods and Materials", "Separate experiments", 54, 54, pull_text("Separate experiments"), NA_character_,
    "\"the varied experimental protocols for each of the 54 separate experiments (27 study pairs)\"",
  "Methods and Materials", "Distinct subgroups", 16, 16, pull_text("Distinct demographic subgroups"), NA_character_,
    "\"among 16 distinct subgroups defined by subjects' pretreatment background characteristics\"",
  "Results", "Comparison opportunities", n_script_comparisons, 393, pull_text("Comparison opportunities"), NA_character_,
    "\"Out of 393 opportunities, the difference-in-CATEs is significant 59 times, or 15% of the time.\"",
  "Results", "Difference-in-CATEs significant", sum(scatter_script$`Difference in CATES` == "Significant"),
    59, pull_text("Difference-in-CATEs significant"), NA_character_, "Same sentence",
  "Results", "Share of comparisons significant (%)",
    100 * mean(scatter_script$`Difference in CATES` == "Significant"), 15,
    100 * pull_text("Share of comparisons with a significant difference"), NA_character_, "Same sentence",
  "Results", "Sign disagreements with both versions significant",
    sum(sign(scatter_script$estimate_mt) != sign(scatter_script$estimate_original) &
          scatter_script$sig_mt == "Significant" & scatter_script$sig_original == "Significant"),
    0, pull_text("Sign disagreements with both versions significant"), NA_character_,
    "\"In 0 of 393 opportunities do the CATEs have different signs while both being statistically distinguishable from 0.\"",
  "Results", "CATEs significant in the original version", sum(scatter_script$sig_original == "Significant"),
    156, pull_text("CATEs significant in the original version"), NA_character_,
    "\"Of the 156 CATEs that were significantly different from no effect in the original, 118 are significantly different from no effect in the MTurk replication.\"",
  "Results", "Of those, significant in the MTurk version",
    sum(scatter_script$sig_original == "Significant" & scatter_script$sig_mt == "Significant"),
    118, pull_text("Of those, significant in the MTurk version"), NA_character_, "Same sentence",
  "Results", "CATEs indistinguishable from zero in the original version",
    sum(scatter_script$sig_original == "Not Significant"), 237,
    pull_text("CATEs indistinguishable from zero in the original version"), NA_character_,
    "\"Of the 237 CATEs that were statistically indistinguishable from no effect in the original, 158 were statistically indistinguishable from 0 in the MTurk version.\"",
  "Results", "Of those, indistinguishable in the MTurk version",
    sum(scatter_script$sig_original == "Not Significant" & scatter_script$sig_mt == "Not Significant"),
    158, pull_text("Of those, indistinguishable in the MTurk version"), NA_character_, "Same sentence",
  "Results", "Overall significance match rate (%)", 100 * sum(diag(sig_script)) / sum(sig_script),
    70, 100 * pull_text("Overall significance match rate"), NA_character_,
    "\"The overall 'significance match' rate is therefore 70%.\"",
  "Results", "Smallest across-study slope", min(group_table_script$slope), 0.71,
    pull_text("Smallest across-study slope"), NA_character_,
    "\"The slopes are all positive, ranging from 0.71 to 1.01.\"",
  "Results", "Largest across-study slope", max(group_table_script$slope), 1.01,
    pull_text("Largest across-study slope"), NA_character_, "Same sentence",
  "Results", "Across-study intervals excluding one",
    sum(group_table_script$ci_low > 1 | group_table_script$ci_high < 1), 1,
    pull_text("Across-study slopes whose interval excludes one"), NA_character_,
    "\"All but one of the 95% CIs include 1 ... The CI for the conservative group (just barely) excludes 1\". The conservative interval is the one, at [0.52, 0.99].",
  "Results", "F tests failing to reject at 0.05", sum(f_tests_script$p_value > 0.05), 25,
    pull_text("F tests failing to reject at 0.05"), NA_character_,
    "\"we fail to reject the null hypothesis most of the time (25 of 27 opportunities)\"",
  "Results", "Within-study correlation of CATEs across versions", NA, NA, NA, NA_character_,
    "\"The CATEs in the original study are mostly uncorrelated with the CATEs in the MTurk versions.\" The article prints no number. The 27 within-study correlations run from -0.63 to 0.87 with a median of 0.24 and are positive in 17 of 27 pairs; see text_within_study_correlation.csv."
)

# Figures ----
figure_rows <- tribble(
  ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~defect_locus, ~notes,
  "Figure 1", "Plotted points and confidence limits", NA, NA, NA, NA_character_,
    paste0("The figure prints no numbers. All ", nrow(figure_1_rewrite),
           " plotted comparisons and their six coordinates agree with the deposit's own ",
           "CLM_scatter_df.rds to better than 1e-12; see figure_1_across_study_correspondence.csv."),
  "Figure 2", "Plotted points and confidence limits", NA, NA, NA, NA_character_,
    paste0("The same ", nrow(figure_2_rewrite),
           " comparisons panelled by study rather than by subgroup; see ",
           "figure_2_within_study_correspondence.csv.")
)

# Assemble ----
gt <- bind_rows(table_1_rows, table_2_rows, appendix_rows, text_rows, figure_rows) |>
  mutate(
    paper_id = "coppock_leeper_mullinix_2018",
    match = agrees(value_script, value_paper),
    match_rewrite = agrees(value_rewrite, value_paper)
  ) |>
  select(paper_id, table_figure, claim, value_script, value_paper, match,
         value_rewrite, match_rewrite, defect_locus, notes)

write_csv(gt, here::here("ground_truth", "coppock_leeper_mullinix_2018_ground_truth.csv"))

print(paste("rows:", nrow(gt),
            "| match = 1:", sum(gt$match == 1, na.rm = TRUE),
            "| match = 0:", sum(gt$match == 0, na.rm = TRUE),
            "| match = NA:", sum(is.na(gt$match))))
print(paste("match_rewrite = 1:", sum(gt$match_rewrite == 1, na.rm = TRUE),
            "| match_rewrite = 0:", sum(gt$match_rewrite == 0, na.rm = TRUE),
            "| match_rewrite = NA:", sum(is.na(gt$match_rewrite))))
print(gt |> filter(match_rewrite == 0) |> select(table_figure, claim, value_paper, value_rewrite, defect_locus),
      n = 100, width = 200)

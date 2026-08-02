# coppock_leeper_mullinix_2018/maintained/table_1_within_study_correspondence.R
# Output: output/table_1_within_study_correspondence.csv
# Depends on: helpers.R, estimate_cates.R, f_tests.R, study_ns.R
# Description: Table 1. Within each study pair, the Deming regression of the
#   original CATEs on the replication CATEs, with jackknife confidence
#   intervals, alongside the sample sizes and the joint F test.

source(here::here("maintained", "helpers.R"))

cate_scatter <- read_csv(file.path(out_dir, "cate_scatter.csv"),
                         show_col_types = FALSE)
f_tests <- read_csv(file.path(out_dir, "f_tests.csv"), show_col_types = FALSE)
study_ns <- read_csv(file.path(out_dir, "study_ns.csv"), show_col_types = FALSE)

# Deming regression by study ----
# An errors-in-variables fit: both axes carry estimated standard errors, and
# deming() takes them directly. The confidence interval is the jackknife over
# the comparisons within the study, which involves no random draw.
study_deming <- cate_scatter |>
  group_by(study_label) |>
  reframe(tidy_deming(deming(estimate_original ~ estimate_mt,
                             ystd = std.error_original,
                             xstd = std.error_mt,
                             data = pick(everything())))) |>
  filter(term == "estimate_mt")

n_comparisons <- cate_scatter |> count(study_label, name = "n_comparisons")

table_1 <- study_ns |>
  left_join(study_deming, by = "study_label") |>
  left_join(n_comparisons, by = "study_label") |>
  left_join(f_tests, by = c("study", "study_label")) |>
  transmute(
    study_label,
    original_n = reported_original_n,
    mt_n = reported_mt_n,
    slope = estimate,
    slope_se = std.error,
    ci_low = conf.low,
    ci_high = conf.high,
    n_comparisons,
    f_p_value = p_value,
    slope_display = sprintf("%.2f (%.2f)", estimate, std.error),
    ci_display = sprintf("[%.2f, %.2f]", conf.low, conf.high)
  ) |>
  arrange(study_label)

write_csv(table_1, file.path(out_dir, "table_1_within_study_correspondence.csv"))

print(table_1 |> select(study_label, original_n, mt_n, slope_display,
                        ci_display, n_comparisons, f_p_value),
      n = nrow(table_1), width = 200)

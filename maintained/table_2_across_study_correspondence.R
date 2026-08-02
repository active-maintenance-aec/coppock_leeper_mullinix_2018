# coppock_leeper_mullinix_2018/maintained/table_2_across_study_correspondence.R
# Output: output/table_2_across_study_correspondence.csv
# Depends on: helpers.R, estimate_cates.R
# Description: Table 2. Within each demographic subgroup, the Deming regression
#   of the original CATEs on the replication CATEs across all 27 study pairs,
#   with jackknife confidence intervals.

source(here::here("maintained", "helpers.R"))

cate_scatter <- read_csv(file.path(out_dir, "cate_scatter.csv"),
                         show_col_types = FALSE) |>
  mutate(group_label = factor(group_label, levels = group_figure_labels))

group_deming <- cate_scatter |>
  group_by(group_label) |>
  reframe(tidy_deming(deming(estimate_original ~ estimate_mt,
                             ystd = std.error_original,
                             xstd = std.error_mt,
                             data = pick(everything())))) |>
  filter(term == "estimate_mt")

n_comparisons <- cate_scatter |> count(group_label, name = "n_comparisons")

table_2 <- group_deming |>
  left_join(n_comparisons, by = "group_label") |>
  transmute(
    covariate_class = as.character(group_label),
    slope = estimate,
    slope_se = std.error,
    ci_low = conf.low,
    ci_high = conf.high,
    n_comparisons,
    slope_display = sprintf("%.2f (%.2f)", estimate, std.error),
    ci_display = sprintf("[%.2f, %.2f]", conf.low, conf.high)
  )

write_csv(table_2, file.path(out_dir, "table_2_across_study_correspondence.csv"))

print(table_2, n = nrow(table_2), width = 200)

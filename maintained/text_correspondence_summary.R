# coppock_leeper_mullinix_2018/maintained/text_correspondence_summary.R
# Output: output/text_correspondence_summary.csv
# Depends on: helpers.R, estimate_cates.R, f_tests.R, study_ns.R,
#   table_2_across_study_correspondence.R
# Description: Every quantity the paper states in prose rather than in a table.

source(here::here("maintained", "helpers.R"))

cate_scatter <- read_csv(file.path(out_dir, "cate_scatter.csv"),
                         show_col_types = FALSE)
f_tests <- read_csv(file.path(out_dir, "f_tests.csv"), show_col_types = FALSE)
study_ns <- read_csv(file.path(out_dir, "study_ns.csv"), show_col_types = FALSE)
table_2 <- read_csv(file.path(out_dir, "table_2_across_study_correspondence.csv"),
                    show_col_types = FALSE)

# Significance agreement ----
# Both versions of each comparison are classified by whether the CATE differs
# significantly from zero, and separately by whether the two CATEs differ
# significantly from each other.
significance <- cate_scatter |>
  count(significant_original, significant_mt) |>
  mutate(cell = paste0("original_", significant_original,
                       "_mt_", significant_mt))

sig_in_original <- sum(cate_scatter$significant_original == 1)
sig_in_both <- sum(cate_scatter$significant_original == 1 &
                     cate_scatter$significant_mt == 1)
null_in_original <- sum(cate_scatter$significant_original == 0)
null_in_both <- sum(cate_scatter$significant_original == 0 &
                      cate_scatter$significant_mt == 0)

# A sign disagreement means the two versions point in opposite directions and
# both are distinguishable from zero.
sign_disagreements <- sum(
  sign(cate_scatter$estimate_mt) != sign(cate_scatter$estimate_original) &
    cate_scatter$significant_mt == 1 &
    cate_scatter$significant_original == 1
)

text_summary <- tibble(
  quantity = c(
    "Study pairs",
    "Separate experiments",
    "Total survey responses",
    "Distinct demographic subgroups",
    "Comparison opportunities",
    "Difference-in-CATEs significant",
    "Share of comparisons with a significant difference",
    "Sign disagreements with both versions significant",
    "CATEs significant in the original version",
    "Of those, significant in the MTurk version",
    "CATEs indistinguishable from zero in the original version",
    "Of those, indistinguishable in the MTurk version",
    "Overall significance match rate",
    "Smallest across-study slope",
    "Largest across-study slope",
    "Across-study slopes whose interval excludes one",
    "F tests failing to reject at 0.05"
  ),
  value = c(
    nrow(study_ns),
    2 * nrow(study_ns),
    sum(study_ns$reported_total_n),
    n_distinct(cate_scatter$group_label),
    nrow(cate_scatter),
    sum(cate_scatter$diff_in_cates_significant),
    mean(cate_scatter$diff_in_cates_significant),
    sign_disagreements,
    sig_in_original,
    sig_in_both,
    null_in_original,
    null_in_both,
    (sig_in_both + null_in_both) / nrow(cate_scatter),
    min(table_2$slope),
    max(table_2$slope),
    sum(table_2$ci_low > 1 | table_2$ci_high < 1),
    sum(f_tests$p_value > 0.05)
  )
)

write_csv(text_summary, file.path(out_dir, "text_correspondence_summary.csv"))

print(text_summary, n = nrow(text_summary), width = 200)
print(significance |> select(cell, n), n = nrow(significance), width = 200)

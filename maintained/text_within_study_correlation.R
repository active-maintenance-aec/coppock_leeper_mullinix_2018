# coppock_leeper_mullinix_2018/maintained/text_within_study_correlation.R
# Output: output/text_within_study_correlation.csv
# Depends on: helpers.R, estimate_cates.R
# Description: The paper says the CATEs within a study are "mostly
#   uncorrelated" across the two versions. That is a claim about the
#   within-study correlations, one per study pair, so they are computed here
#   alongside the pooled correlation over all 393 comparisons.

source(here::here("maintained", "helpers.R"))

cate_scatter <- read_csv(file.path(out_dir, "cate_scatter.csv"),
                         show_col_types = FALSE)

within_study <- cate_scatter |>
  summarize(
    n_comparisons = n(),
    correlation = cor(estimate_mt, estimate_original, use = "complete.obs"),
    .by = c(study, study_label)
  ) |>
  arrange(correlation)

pooled <- tibble(
  study = "all",
  study_label = "All comparisons pooled",
  n_comparisons = nrow(cate_scatter),
  correlation = cor(cate_scatter$estimate_mt, cate_scatter$estimate_original,
                    use = "complete.obs")
)

within_study_correlation <- bind_rows(within_study, pooled)

write_csv(within_study_correlation,
          file.path(out_dir, "text_within_study_correlation.csv"))

print(within_study_correlation, n = nrow(within_study_correlation), width = 200)
print(paste("Within-study correlations: median",
            round(median(within_study$correlation), 3),
            "; positive in", sum(within_study$correlation > 0), "of",
            nrow(within_study), "study pairs"))

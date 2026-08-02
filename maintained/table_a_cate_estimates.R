# coppock_leeper_mullinix_2018/maintained/table_a_cate_estimates.R
# Output: output/table_a_cate_estimates.csv
# Depends on: helpers.R, estimate_cates.R
# Description: The 27 appendix tables, one per study pair, in a single tidy
#   file. Each row is one covariate class in one version of one study: the
#   difference-in-means, its standard error, the two-sided p-value, the
#   confidence interval, the number of observations and the share of the study
#   version those observations are.

source(here::here("maintained", "helpers.R"))

cate_estimates <- read_csv(file.path(out_dir, "cate_estimates.csv"),
                           show_col_types = FALSE)

# Counts as the deposit computes them ----
# The deposited CLM_appendix.R sets the N column to the residual degrees of
# freedom plus two and the Prop column to that N over its sum within the
# covariate. Both are computed over every row of the fit, so a study whose
# regression carries an extra regressor loses one observation per extra term
# from N, and the denominator of Prop is multiplied by the number of terms.
# Reproduced here so the published columns can be compared against something.
deposit_counts <- cate_estimates |>
  mutate(n_deposit_formula = df + 2) |>
  mutate(prop_deposit_formula = n_deposit_formula / sum(n_deposit_formula),
         .by = c(study, sample, covariate)) |>
  filter(term == "Z") |>
  select(study, group, sample, n_deposit_formula, prop_deposit_formula)

appendix_table_order <- study_labels_df |>
  arrange(study_label) |>
  mutate(appendix_table = row_number())

table_a <- cate_estimates |>
  filter(term == "Z") |>
  left_join(deposit_counts, by = c("study", "group", "sample")) |>
  mutate(prop = nobs / sum(nobs), .by = c(study, sample, covariate)) |>
  left_join(appendix_table_order |> select(study, appendix_table), by = "study") |>
  mutate(covariate_class = factor(group, levels = appendix_group_levels,
                                  labels = appendix_group_labels)) |>
  arrange(appendix_table, desc(sample), covariate_class) |>
  transmute(
    appendix_table,
    study_label,
    sample,
    covariate_class = as.character(covariate_class),
    cate = estimate,
    se = std.error,
    p_value = p.value,
    ci_low = conf.low,
    ci_high = conf.high,
    n = nobs,
    prop,
    n_deposit_formula,
    prop_deposit_formula
  )

write_csv(table_a, file.path(out_dir, "table_a_cate_estimates.csv"))

print(paste("Appendix tables:", n_distinct(table_a$appendix_table),
            "; rows:", nrow(table_a),
            "; rows where the deposit's N differs from the observation count:",
            sum(table_a$n != table_a$n_deposit_formula)))

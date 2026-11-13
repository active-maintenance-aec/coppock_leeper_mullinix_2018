# coppock_leeper_mullinix_2018/maintained/in_text_claims.R
# Output: printed to the console; nothing is written
# Depends on: maintained/output/*, ground_truth/published_claims.csv
# Description: The second instrument. Every quantity the article states in prose,
#   rather than inside a table, is recomputed here from the pipeline's own output
#   by a path of its own and printed beside the sentence that states it. It reads
#   the extraction, because a block cannot name the article's own figure without
#   it, and it never reads the ground truth, because agreeing with the comparison
#   would prove nothing.
#
#   Where the ground truth reaches a quantity through text_correspondence_summary.csv,
#   this file goes back to the estimates the summary was built from, so the two
#   derivations are separate. Nothing here refits anything.
#
#   The scoring comes from excheckr. Each printed line is
#
#       CLAIM <id> = <value> || [verdict] <label>
#
#   and the id on that line is the only link the coverage gate in
#   build_ground_truth.R uses. The expectation is derived, never typed here:
#   claim_start() reads it out of published_claims.csv, which says what the article
#   prints, and errata_entries.csv, which says which of those a published quantity
#   erratum corrects. No entry in this paper's note is of class quantity, so every
#   claim with a published value is expected to match.
#
#   A sentence that states no number scores [shape], and comes in two kinds. One
#   reduces to a truth value, which is printed as 1 (it holds) or 0 (it does not)
#   and compared against holds in the ground truth. The other describes a
#   distribution without naming a threshold ("mostly uncorrelated", "tightly
#   clustered"); its block declares expect = "shape" and prints one summary of the
#   distribution beside the rest of it in the label, and nothing compares it.

source(here::here("maintained", "helpers.R"))
library(excheckr)

options(width = 200)

# Every column is read as character, so a published 0.05 keeps the precision the
# comparison depends on.
published_claims <- read_csv(here::here("ground_truth", "published_claims.csv"),
                             col_types = cols(.default = col_character()))
errata <- read_csv(here::here("errata_entries.csv"),
                   col_types = cols(.default = col_character()))

claim_start(published = published_claims, errata = errata, format = "id")

# Pipeline output ------------------------------------------------------------------

cate_estimates <- read_csv(file.path(out_dir, "cate_estimates.csv"), show_col_types = FALSE)
cate_scatter <- read_csv(file.path(out_dir, "cate_scatter.csv"), show_col_types = FALSE)
table_1 <- read_csv(file.path(out_dir, "table_1_within_study_correspondence.csv"),
                    show_col_types = FALSE)
table_2 <- read_csv(file.path(out_dir, "table_2_across_study_correspondence.csv"),
                    show_col_types = FALSE)
table_a <- read_csv(file.path(out_dir, "table_a_cate_estimates.csv"), show_col_types = FALSE)
f_tests <- read_csv(file.path(out_dir, "f_tests.csv"), show_col_types = FALSE)
figure_1 <- read_csv(file.path(out_dir, "figure_1_across_study_correspondence.csv"),
                     show_col_types = FALSE)
correlations <- read_csv(file.path(out_dir, "text_within_study_correlation.csv"),
                         show_col_types = FALSE)

treatment_effects <- cate_estimates |> filter(term == "Z")
within_study_correlations <- correlations |> filter(study != "all")

# Abstract ----

# "We analyze subgroup conditional average treatment effects using 27 original-
#  replication study pairs (encompassing 101,745 individual survey responses) to
#  assess the extent to which subgroup effect estimates generalize."
claim("abstract_study_pairs", n_distinct(cate_estimates$study),
      "Study pairs with at least one estimated subgroup effect")
claim("abstract_survey_responses", sum(table_1$original_n) + sum(table_1$mt_n),
      "Sum of Table 1's two sample size columns")

# Significance statement ----

# "We replicated 27 survey experiments (encompassing 101,745 individual survey
#  responses) originally conducted on nationally representative samples using
#  online convenience samples, finding very high correspondence despite obvious
#  differences in sample composition."
claim("significance_study_pairs", n_distinct(cate_estimates$study),
      "Study pairs, as the significance statement counts them")
claim("significance_survey_responses", sum(table_1$original_n) + sum(table_1$mt_n),
      "Survey responses, as the significance statement counts them")

# Methods and Materials ----

# "We aim to distinguish between scenarios A and B through reanalyses of 27
#  original-replication pairs collected by refs. 12 and 13."
claim("methods_study_pairs", n_distinct(cate_estimates$study),
      "Original-replication pairs reanalysed")

# "Our goal here is to assess the degree of correspondence of conditional average
#  treatment effect (CATE) estimates among 16 distinct subgroups defined by
#  subjects' pretreatment background characteristics."
claim("methods_subgroups", n_distinct(table_a$covariate_class),
      "Distinct covariate classes in the appendix tables")

# "The full list of studies, with the sample sizes used in the analyses reported
#  here, is presented in Table 1."
study_ns <- read_csv(file.path(out_dir, "study_ns.csv"), show_col_types = FALSE)
claim(
  "methods_table1_sample_sizes",
  as.numeric(all(study_ns$reported_total_n == study_ns$analysis_obs_total_n)),
  str_glue("Table 1's N columns total {sum(study_ns$reported_total_n)} respondents ",
           "against {sum(study_ns$analysis_obs_total_n)} observations in the fits: ",
           "the reported sample is larger in ",
           "{sum(study_ns$analysis_obs_total_n < study_ns$reported_total_n)} of the ",
           "{nrow(study_ns)} pairs, equal in ",
           "{sum(study_ns$analysis_obs_total_n == study_ns$reported_total_n)}, and ",
           "smaller in ",
           "{sum(study_ns$analysis_obs_total_n > study_ns$reported_total_n)}. The ",
           "column counts respondents, which is what the abstract's total needs, ",
           "and not what the analyses are fitted on"),
  digits = 0)

# "We estimate all CATEs via difference-in-means." The appendix repeats it: "the
#  CATE column refers to the difference-in-means estimate of the treatment effect,
#  conditional on membership in the covariate class."
adjusted_studies <- cate_estimates |>
  filter(!term %in% c("(Intercept)", "Z")) |>
  distinct(study_label)
claim("methods_cate_difference_in_means", nrow(adjusted_studies),
      str_glue("Of {n_distinct(cate_estimates$study)} study pairs, ",
               "{nrow(adjusted_studies)} fit a regressor beyond treatment ",
               "({str_flatten_comma(sort(adjusted_studies$study_label))}); ",
               "each of those regressors is another arm of the study's own factorial ",
               "design or the party identification its assignment probabilities depend on"),
      digits = 0, expect = "shape")

# "Because of the varied experimental protocols for each of the 54 separate
#  experiments (27 study pairs) reanalyzed here, the largest challenge we face is
#  measuring subject characteristics in an identical manner across experiments."
claim("methods_separate_experiments",
      n_distinct(paste(cate_estimates$study, cate_estimates$sample)),
      "Study-by-version cells, which is what a separate experiment is")
claim("methods_study_pairs_parenthetical", n_distinct(cate_estimates$study),
      "Study pairs, as the same sentence counts them")

# "We have identified six attributes that are measured in nearly all studies."
claim("methods_attributes", n_distinct(cate_estimates$covariate),
      "Pretreatment attributes the subgroup estimates are cut by")

# "These attributes are not always measured in the same way, so we have coarsened
#  each to a maximum of three categories to maintain comparability across studies:
#  age (18 to 39, 40 to 59, 60+), education (less than college, college, graduate
#  school), gender (men, women), ideology (liberal, moderate, conservative),
#  partisanship (Democrat, Independent, Republican), and race (nonwhite, white)."
categories_per_attribute <- cate_estimates |>
  summarize(levels = n_distinct(group), .by = covariate)
claim("methods_max_categories", max(categories_per_attribute$levels),
      str_glue("Largest number of categories any attribute is coarsened to ",
               "(levels per attribute: ",
               "{str_flatten_comma(paste0(categories_per_attribute$covariate, ' ', categories_per_attribute$levels))})"))

age_bounds <- cate_estimates |>
  filter(covariate == "age") |>
  distinct(group) |>
  arrange(group) |>
  pull(group) |>
  str_extract_all("\\d+") |>
  map(~ as.numeric(.x[-1]))

claim("methods_age_band_1_low", age_bounds[[1]][1],
      "Lower bound of the youngest age band, read off the subgroup labels")
claim("methods_age_band_1_high", age_bounds[[1]][2],
      "Upper bound of the youngest age band, read off the subgroup labels")
claim("methods_age_band_2_low", age_bounds[[2]][1],
      "Lower bound of the middle age band, read off the subgroup labels")
claim("methods_age_band_2_high", age_bounds[[2]][2],
      "Upper bound of the middle age band, read off the subgroup labels")
claim("methods_age_band_3_low", age_bounds[[3]][1],
      "Lower bound of the oldest age band, read off the subgroup labels")

# Results footnote ----

# "We estimate the standard error of the difference-in-CATEs as sqrt(SE_1^2 +
#  SE_2^2) and conduct hypothesis tests under a normal approximation. We deem a
#  difference statistically significant if the P value is less than 0.05."
claim(
  "footnote_se_formula",
  as.numeric(max(abs(cate_scatter$std.error_diff_in_cates -
                       sqrt(cate_scatter$std.error_mt^2 +
                              cate_scatter$std.error_original^2))) < 1e-12),
  str_glue("The stated formula reproduces every one of ",
           "{nrow(cate_scatter)} standard errors of the difference"),
  digits = 0)

claim(
  "footnote_normal_approximation",
  as.numeric(max(abs(cate_scatter$p_diff_in_cates -
                       2 * pnorm(-abs(cate_scatter$est_diff_in_cates /
                                        cate_scatter$std.error_diff_in_cates)))) < 1e-12),
  "Every difference p-value is the two-sided normal tail, not a t tail",
  digits = 0)

# The threshold is not printed anywhere in the pipeline's output, so it is
# recovered from where the classification changes: the largest p-value called
# significant and the smallest called not.
alpha_bracket <- c(max(cate_scatter$p_diff_in_cates[cate_scatter$diff_in_cates_significant]),
                   min(cate_scatter$p_diff_in_cates[!cate_scatter$diff_in_cates_significant]))
claim("footnote_alpha", mean(alpha_bracket),
      str_glue("Threshold implied by the classification, which brackets it between ",
               "{signif(alpha_bracket[1], 4)} and {signif(alpha_bracket[2], 4)}"))

# Results ----

# "Out of 393 opportunities, the difference-in-CATEs is significant 59 times, or
#  15% of the time."
claim("results_opportunities", nrow(figure_1),
      "Comparisons Figure 1 plots, read off that figure's own file")
claim("results_diff_significant", sum(figure_1$difference == "Significant"),
      "Comparisons whose difference-in-CATEs is significant")
claim("results_diff_share", 100 * mean(figure_1$difference == "Significant"),
      "Share of comparisons with a significant difference")

# "In 0 of 393 opportunities do the CATEs have different signs while both being
#  statistically distinguishable from 0."
claim("results_sign_disagreements",
      sum(sign(cate_scatter$estimate_mt) != sign(cate_scatter$estimate_original) &
           cate_scatter$significant_mt == 1 & cate_scatter$significant_original == 1),
      "Comparisons with opposite signs and both versions significant")
claim("results_sign_opportunities", nrow(cate_scatter),
      "Comparisons, as the same sentence counts them")

# "Of the 156 CATEs that were significantly different from no effect in the
#  original, 118 are significantly different from no effect in the MTurk
#  replication."
claim("results_significant_original", sum(cate_scatter$significant_original == 1),
      "CATEs significant in the original version")
claim("results_significant_both",
      sum(cate_scatter$significant_original == 1 & cate_scatter$significant_mt == 1),
      "Of those, also significant in the MTurk version")

# "Of the 237 CATEs that were statistically indistinguishable from no effect in
#  the original, 158 were statistically indistinguishable from 0 in the MTurk
#  version."
claim("results_null_original", sum(cate_scatter$significant_original == 0),
      "CATEs indistinguishable from zero in the original version")
claim("results_null_both",
      sum(cate_scatter$significant_original == 0 & cate_scatter$significant_mt == 0),
      "Of those, also indistinguishable in the MTurk version")

# "The overall 'significance match' rate is therefore 70%."
claim("results_match_rate",
      100 * mean(cate_scatter$significant_original == cate_scatter$significant_mt),
      "Share of comparisons agreeing on significance in both directions")

# "The estimated slopes across CATEs are shown in Table 2. The slopes are all
#  positive, ranging from 0.71 to 1.01."
claim("results_slopes_all_positive", as.numeric(all(table_2$slope > 0)),
      str_glue("All {nrow(table_2)} across-study slopes are above zero"), digits = 0)
claim("results_slope_min", min(table_2$slope), "Smallest across-study slope")
claim("results_slope_max", max(table_2$slope), "Largest across-study slope")

# "All but one of the 95% CIs include 1, but the intervals are sometimes quite
#  wide, so we resist 'accepting the null' of perfect correspondence. The CI for
#  the conservative group (just barely) excludes 1."
excludes_one <- table_2 |> filter(ci_low > 1 | ci_high < 1)
claim("results_intervals_excluding_one", nrow(excludes_one),
      "Across-study intervals that do not cover one")
claim("results_conservative_excludes_one",
      as.numeric(nrow(excludes_one) == 1 && excludes_one$covariate_class == "Conservative"),
      str_glue("The interval that excludes one belongs to ",
               "{str_flatten_comma(excludes_one$covariate_class)}, at ",
               "[{sprintf('%.2f', excludes_one$ci_low)}, ",
               "{sprintf('%.2f', excludes_one$ci_high)}]"),
      digits = 0)

# "Fig. 2 shows that the answer tends to be no. The CATEs in the original study
#  are mostly uncorrelated with the CATEs in the MTurk versions."
claim("results_mostly_uncorrelated", median(within_study_correlations$correlation),
      str_glue("{nrow(within_study_correlations)} within-study correlations run ",
               "{sprintf('%.2f', min(within_study_correlations$correlation))} to ",
               "{sprintf('%.2f', max(within_study_correlations$correlation))}, ",
               "median {sprintf('%.2f', median(within_study_correlations$correlation))}, ",
               "positive in {sum(within_study_correlations$correlation > 0)} pairs, ",
               "against {sprintf('%.2f', correlations$correlation[correlations$study == 'all'])} ",
               "pooled over all comparisons"),
      digits = 2, expect = "shape")

# "We see within-study slopes that are smaller than the across-study slopes and
#  slopes of both signs."
claim("results_within_smaller", median(table_1$slope),
      str_glue("Median within-study slope {sprintf('%.2f', median(table_1$slope))} ",
               "against a median across-study slope of ",
               "{sprintf('%.2f', median(table_2$slope))}; ",
               "{sum(table_1$slope < min(table_2$slope))} of {nrow(table_1)} ",
               "within-study slopes fall below every across-study slope and ",
               "{sum(table_1$slope > max(table_2$slope))} rise above every one"),
      digits = 2, expect = "shape")
claim("results_both_signs",
      as.numeric(any(table_1$slope < 0) && any(table_1$slope > 0)),
      str_glue("{sum(table_1$slope < 0)} within-study slopes are negative and ",
               "{sum(table_1$slope > 0)} positive"),
      digits = 0)

# "Most of the CATEs are tightly clustered around the overall average treatment
#  effect in each study version."
spread <- treatment_effects |>
  summarize(sd_cate = sd(estimate), .by = c(study, sample))
claim("results_cates_clustered", median(spread$sd_cate),
      str_glue("Within a study version the subgroup effects have a standard ",
               "deviation of {sprintf('%.2f', median(spread$sd_cate))} on the ",
               "standardized outcome at the median, running ",
               "{sprintf('%.2f', min(spread$sd_cate))} to ",
               "{sprintf('%.2f', max(spread$sd_cate))} across the ",
               "{nrow(spread)} study versions"),
      digits = 2, expect = "shape")

# "Consistent with Fig. 2, we fail to reject the null hypothesis most of the time
#  (25 of 27 opportunities), indicating that whatever heterogeneity in treatment
#  effects there may be, the patterns do not differ greatly across samples."
claim("results_f_fail", sum(f_tests$p_value > 0.05),
      "Joint F tests failing to reject at 0.05")
claim("results_f_opportunities", nrow(f_tests),
      "Study pairs offering a joint F test")

# Discussion ----

# "Drawing on a fine-grained analysis of 27 pairs of survey experiments conducted
#  on representative and nonrepresentative samples and various methods of
#  assessing the pattern of effect heterogeneity in each study, we have shown that
#  effect heterogeneity is typically limited."
claim("discussion_study_pairs", n_distinct(cate_estimates$study),
      "Pairs of survey experiments the discussion counts")

# "We find only limited evidence that such moderation occurs and, when it does,
#  the differences in effect sizes across groups are small."
claim("discussion_limited_moderation", median(abs(cate_scatter$est_diff_in_cates)),
      str_glue("{sum(cate_scatter$diff_in_cates_significant)} of ",
               "{nrow(cate_scatter)} subgroup comparisons differ significantly ",
               "across versions, and the absolute difference in CATEs has a ",
               "median of {sprintf('%.2f', median(abs(cate_scatter$est_diff_in_cates)))} ",
               "standard deviations of the outcome"),
      digits = 2, expect = "shape")

# Appendix ----

# "All p-values are two-sided and are not corrected for multiple comparisons."
# One term has no p-value to be one- or two-sided. pedullaS18 / pid_3_Independent /
# original holds five subjects, and its two control subjects share an outcome, so the
# intercept is a mean fit with exactly zero residuals and its HC2 variance is a sum of
# zero-squared terms. Which way that residue falls is a floating-point accident:
# estimatr 2.0 returns NaN, and 1.0.6 returned the positive root of a 2e-33 variance,
# a standard error of 5e-17 and a p-value of 1e-50. filter() drops the NA and the tiny
# root alike, so the excluded set is the same on every platform, and 1e-12 is the
# threshold the confidence-level block below already uses: the residue is 5e-17 and the
# smallest genuine standard error in these fits is 0.013. The count is pinned so that a
# second degenerate cell fails this claim rather than being absorbed into the exclusion.
p_determinate <- cate_estimates |> filter(std.error > 1e-12)
claim(
  "appendix_p_two_sided",
  as.numeric(nrow(p_determinate) == nrow(cate_estimates) - 1 &&
               max(abs(p_determinate$p.value -
                         2 * pt(-abs(p_determinate$statistic), p_determinate$df))) < 1e-12),
  str_glue("Every one of {nrow(p_determinate)} determinate p-values is the two-sided ",
           "t tail; the intercept of the five-subject pedullaS18 Independent cell has ",
           "no standard error"),
  digits = 0)

# "The Prop column describes what proportion of the subjects in a given experiment
#  belong to the associated covariate class."
published_prop_sums <- table_a |>
  mutate(attribute = str_split_i(covariate_class, ":", 1)) |>
  summarize(share = sum(prop_deposit_formula), .by = c(study_label, sample, attribute))
claim(
  "appendix_prop_definition",
  as.numeric(all(abs(published_prop_sums$share - 1) < 0.02)),
  str_glue("The printed shares of an attribute's classes sum to between ",
           "{sprintf('%.2f', min(published_prop_sums$share))} and ",
           "{sprintf('%.2f', max(published_prop_sums$share))} rather than to 1.00, ",
           "across {nrow(published_prop_sums)} attribute-by-study-version blocks; ",
           "each printed value is the class share divided by the number of terms in ",
           "the regression that produced the row"),
  digits = 0)

# The "95% CI" column heading, printed once in each of the 54 panels. One fit
# returns a zero standard error and a degenerate interval; it is excluded.
implied_level <- cate_estimates |>
  filter(std.error > 1e-12) |>
  mutate(level = 2 * pt((conf.high - estimate) / std.error, df) - 1)
claim("appendix_ci_level", 100 * max(implied_level$level),
      str_glue("Confidence level implied by the interval half widths, identical ",
               "across all {nrow(implied_level)} nondegenerate fits"))

# Gates ----
# assert_claims() reads the verdicts: no failing verdict, every claim a quantity erratum
# names printed as corrected, and the printed set exactly the set the extraction says
# needs a block. claim_summary() then prints the verdict counts.
assert_claims()
invisible(claim_summary())

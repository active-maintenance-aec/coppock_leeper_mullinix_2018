# coppock_leeper_mullinix_2018/maintained/f_tests.R
# Output: output/f_tests.csv
# Depends on: helpers.R, original/replication_archive/*_stacked.rds
# Description: For each study pair, a joint test against the null of no
#   difference in the pattern of treatment effect heterogeneity by sample. The
#   restricted model interacts treatment with the covariates and the sample
#   indicator with the covariates; the unrestricted model adds the three-way
#   interaction. The last column of the paper's Table 1.

source(here::here("maintained", "helpers.R"))

# Study specifications ----
# file, covariates entering the interactions, the analysis filter, and whether
# standard errors are clustered. The covariate set is the study's own, and
# differs from the set used for the subgroup estimates in a few cases: the
# omnibus test for the Brader immigration study drops race, which is constant
# in the original sample, and the Transue test uses the study's blocking factor
# alongside age and party identification.
f_test_specs <- tribble(
  ~study, ~file, ~covars, ~clustered,
  "superordinate_id", "superordinate_id_stacked.rds", c("Z_particularism", "age_3", "pid_3"), FALSE,
  "patriot_act", "patriot_act_stacked.rds", covars_no_ideo, FALSE,
  "elite_endorsements", "elite_endorsements_stacked.rds", covars_no_ideo, FALSE,
  "free_trade", "free_trade_stacked.rds", covars_all, FALSE,
  "frame_breadth", "frame_breadth_stacked.rds", covars_all, FALSE,
  "polarization", "polarization_stacked.rds", covars_all, FALSE,
  "immigration", "immigration_stacked.rds", c("age_3", "pid_3", "educ_3", "female"), FALSE,
  "system_threat", "system_threat_stacked.rds", covars_all, FALSE,
  "expert_economists", "expert_economists_stacked_long.rds", covars_all, TRUE,
  "mental_illness", "mental_illness_stacked.rds", covars_all, FALSE,
  "death_penalty", "death_penalty_stacked.rds", covars_all, FALSE,
  "berganS20", "berganS20_stacked.rds", covars_all, FALSE,
  "brandtS1", "brandtS1_stacked.rds", covars_no_ideo, FALSE,
  "caprarielloS2", "caprarielloS2_stacked.rds", covars_all, FALSE,
  "converseS16", "converseS16_stacked.rds", covars_no_pid_no_ideo, FALSE,
  "dennyS17", "dennyS17_stacked.rds", covars_all, FALSE,
  "flavinS4", "flavinS4_stacked.rds", covars_all, FALSE,
  "gashS5", "gashS5_stacked.rds", covars_all, FALSE,
  "jacobsenS7", "jacobsenS7_stacked.rds", covars_all, FALSE,
  "melloS6", "melloS6_stacked.rds", covars_no_pid_no_ideo, FALSE,
  "parmerS15", "parmerS15_stacked.rds", covars_all, FALSE,
  "pedullaS18", "pedullaS18_stacked.rds", covars_all, FALSE,
  "piazzaS8", "piazzaS8_stacked.rds", covars_all, FALSE,
  "shaferS9", "shaferS9_stacked.rds", covars_all, FALSE,
  "thompsonS10", "thompsonS10_stacked.rds", covars_all, FALSE,
  "turagaS11", "turagaS11_stacked.rds", covars_all, FALSE,
  "wallaceS12", "wallaceS12_stacked.rds", covars_all, FALSE
)

# The three studies whose deposits carry a GfK arm that the paper's analysis
# does not use, the two whose treatment arms include a control condition the
# comparison excludes, and the one with respondent-level missingness.
apply_filter <- function(study, d) {
  switch(study,
    frame_breadth = filter(d, sample != "gfk"),
    polarization = filter(d, sample != "gfk"),
    elite_endorsements = filter(d, Z_imm_match != "control"),
    death_penalty = filter(d, Z_CP_3 != "control"),
    expert_economists = filter(d, !is.na(Z), !is.na(Y_s)),
    d
  )
}

wald_by_study <- function(study, file, covars, clustered) {
  d <- apply_filter(study, read_rds(file.path(data_dir, file)))
  terms <- paste(covars, collapse = " + ")
  restricted <- as.formula(
    paste0("Y_s ~ Z * (", terms, ") + sample * (", terms, ")"))
  unrestricted <- as.formula(
    paste0("Y_s ~ Z * (", terms, ") * sample"))
  fit_restricted <- if (clustered) {
    lm_robust(restricted, clusters = anon_ID, data = d)
  } else {
    lm_robust(restricted, data = d)
  }
  fit_unrestricted <- if (clustered) {
    lm_robust(unrestricted, clusters = anon_ID, data = d)
  } else {
    lm_robust(unrestricted, data = d)
  }
  wt <- waldtest(fit_restricted, fit_unrestricted, test = "F")
  tibble(study = study, df_restricted = wt$Res.Df[1], df_unrestricted = wt$Res.Df[2],
         df_test = wt$Df[2], f_statistic = wt$F[2], p_value = wt$`Pr(>F)`[2])
}

f_tests <- pmap(f_test_specs, wald_by_study) |>
  list_rbind() |>
  left_join(study_labels_df, by = "study") |>
  select(study, study_label, df_restricted, df_unrestricted, df_test,
         f_statistic, p_value)

write_csv(f_tests, file.path(out_dir, "f_tests.csv"))

print(paste("F tests failing to reject at 0.05:",
            sum(f_tests$p_value > 0.05), "of", nrow(f_tests)))

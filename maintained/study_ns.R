# coppock_leeper_mullinix_2018/maintained/study_ns.R
# Output: output/study_ns.csv
# Depends on: helpers.R, original/replication_archive/*_stacked.rds
# Description: Sample sizes per study pair. Two counts are produced for each
#   version of each study. reported_n is the count the published Table 1 gives.
#   analysis_n is the number of observations that actually enter the subgroup
#   estimates. They differ for the sixteen studies drawn from Mullinix et al.
#   (2015), where the deposited file holds a whole survey wave and only part of
#   it was assigned to the experiment in question.

source(here::here("maintained", "helpers.R"))

read_study <- function(f) read_rds(file.path(data_dir, f))

# The eleven studies from Coppock (2018) ----
# Each is filtered to the rows its own analysis uses before counting.

reported_coppock <- bind_rows(
  superordinate_id = read_study("superordinate_id_stacked.rds") |>
    filter(!is.na(Y_s), !is.na(Z), !is.na(Z_particularism)),
  patriot_act = read_study("patriot_act_stacked.rds") |>
    filter(!is.na(Y_s), !is.na(Z)),
  elite_endorsements = read_study("elite_endorsements_stacked.rds") |>
    filter(Z_imm_match != "control", !is.na(Y_s), !is.na(Z)),
  free_trade = read_study("free_trade_stacked.rds") |>
    filter(sample != "gfk", !is.na(Y_s), !is.na(Z), !is.na(Z_Hiscox_valence)),
  frame_breadth = read_study("frame_breadth_stacked.rds") |>
    filter(sample != "gfk", !is.na(Y_s), !is.na(Z)),
  polarization = read_study("polarization_stacked.rds") |>
    filter(sample != "gfk", Z_Levendusky != "placebo", !is.na(Y_s), !is.na(Z)),
  immigration = read_study("immigration_stacked.rds") |>
    filter(Z_brader_pos_neg != "control", !is.na(Y_s), !is.na(Z)),
  system_threat = read_study("system_threat_stacked.rds") |>
    filter(!is.na(Y_s), !is.na(Z)),
  mental_illness = read_study("mental_illness_stacked.rds") |>
    filter(!is.na(Y_s), !is.na(Z)),
  death_penalty = read_study("death_penalty_stacked.rds") |>
    filter(Z_CP_3 != "control", !is.na(Y_s), !is.na(Z)),
  .id = "study"
) |>
  summarize(
    reported_mt_n = sum(sample == "mt"),
    reported_original_n = sum(sample == "original"),
    .by = study
  )

# The expert economists study asked each original respondent one of five
# questions and each replication respondent all five, so its rows are
# respondent-question pairs and the count is of distinct respondents.
reported_expert_economists <- read_study("expert_economists_stacked_long.rds") |>
  filter(!is.na(Y_s), !is.na(Z)) |>
  summarize(sample = unique(sample), .by = anon_ID) |>
  summarize(
    study = "expert_economists",
    reported_mt_n = sum(sample == "mt"),
    reported_original_n = sum(sample == "original")
  )

# The sixteen studies from Mullinix et al. (2015) ----
# Counted unfiltered, which is what the published Table 1 reports.
mullinix_files <- c(
  berganS20 = "berganS20_stacked.rds",
  brandtS1 = "brandtS1_stacked.rds",
  caprarielloS2 = "caprarielloS2_stacked.rds",
  converseS16 = "converseS16_stacked.rds",
  dennyS17 = "dennyS17_stacked.rds",
  flavinS4 = "flavinS4_stacked.rds",
  gashS5 = "gashS5_stacked.rds",
  jacobsenS7 = "jacobsenS7_stacked.rds",
  melloS6 = "melloS6_stacked.rds",
  parmerS15 = "parmerS15_stacked.rds",
  pedullaS18 = "pedullaS18_stacked.rds",
  piazzaS8 = "piazzaS8_stacked.rds",
  shaferS9 = "shaferS9_stacked.rds",
  thompsonS10 = "thompsonS10_stacked.rds",
  turagaS11 = "turagaS11_stacked.rds",
  wallaceS12 = "wallaceS12_stacked.rds"
)

reported_mullinix <- imap(mullinix_files, function(f, s) {
  d <- read_study(f)
  tibble(study = s,
         reported_mt_n = sum(d$sample == "mt"),
         reported_original_n = sum(d$sample == "gfk"))
}) |>
  list_rbind()

reported_ns <- bind_rows(reported_coppock, reported_expert_economists,
                         reported_mullinix)

# Observations actually used ----
# Read back off the subgroup fits. Each covariate partitions the sample, so the
# per-covariate total is the number of observations available for that
# partition, and the largest of them is the most generous count of what the
# study contributes. In the expert economists study a row is a
# respondent-question pair rather than a respondent.
analysis_ns <- read_csv(file.path(out_dir, "cate_estimates.csv"),
                        show_col_types = FALSE) |>
  filter(term == "Z") |>
  summarize(covariate_n = sum(nobs), .by = c(study, sample, covariate)) |>
  summarize(analysis_obs_n = max(covariate_n), .by = c(study, sample)) |>
  pivot_wider(names_from = sample, values_from = analysis_obs_n,
              names_glue = "analysis_obs_{sample}_n")

study_ns <- study_labels_df |>
  left_join(reported_ns, by = "study") |>
  left_join(analysis_ns, by = "study") |>
  mutate(
    reported_total_n = reported_original_n + reported_mt_n,
    analysis_obs_total_n = analysis_obs_original_n + analysis_obs_mt_n,
    analysis_share_of_reported = analysis_obs_total_n / reported_total_n
  ) |>
  arrange(study_label)

write_csv(study_ns, file.path(out_dir, "study_ns.csv"))

print(paste("Total reported responses:", sum(study_ns$reported_total_n)))
print(paste("Studies whose reported N exceeds the observations used:",
            sum(study_ns$analysis_obs_total_n < study_ns$reported_total_n),
            "of", nrow(study_ns)))
print(paste("Median share of the reported N that enters the estimates:",
            round(median(study_ns$analysis_share_of_reported), 3)))

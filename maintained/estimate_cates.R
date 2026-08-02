# coppock_leeper_mullinix_2018/maintained/estimate_cates.R
# Output: output/cate_estimates.csv, output/cate_scatter.csv
# Depends on: helpers.R, original/replication_archive/*_stacked.rds
# Description: Estimate every subgroup conditional average treatment effect in
#   both versions of all 27 study pairs, then reshape to one row per comparison.
#   The two outputs are the same estimates in long and paired form, so they are
#   written together.

source(here::here("maintained", "helpers.R"))

read_study <- function(f) read_rds(file.path(data_dir, f))

# Coppock (2018) studies ----
# Each study contributes the covariates it measured. Additional regressors in
# the formula are the study's own design features: a blocking factor, or party
# identification where the assignment probabilities differ by party.

superordinate_id <- read_study("superordinate_id_stacked.rds") |>
  run_cates(c("age_3", "pid_3", "white"),
            fml = Y_s ~ Z + Z_particularism, w_col = NULL)

patriot_act <- read_study("patriot_act_stacked.rds") |>
  run_cates(covars_no_ideo)

# The matched-endorsement treatments are defined only for partisans, and party
# identification must be conditioned on because assignment probabilities differ
# by party. Party subgroups therefore need no such control.
elite_endorsements_data <- read_study("elite_endorsements_stacked.rds") |>
  filter(Z_imm_match != "control")

elite_endorsements <- bind_rows(
  run_cates(elite_endorsements_data, covars_no_pid_no_ideo, fml = Y_s ~ Z + pid_3),
  run_cates(elite_endorsements_data, "pid_3")
)

free_trade <- read_study("free_trade_stacked.rds") |>
  filter(sample != "gfk") |>
  run_cates(covars_all, fml = Y_s ~ Z + Z_Hiscox_valence)

frame_breadth <- read_study("frame_breadth_stacked.rds") |>
  filter(sample != "gfk") |>
  run_cates(covars_all)

polarization <- read_study("polarization_stacked.rds") |>
  filter(Z_Levendusky != "placebo", sample != "gfk") |>
  run_cates(covars_all)

immigration <- read_study("immigration_stacked.rds") |>
  filter(Z_brader_pos_neg != "control") |>
  run_cates(c("age_3", "pid_3", "educ_3", "female", "white"))

system_threat <- read_study("system_threat_stacked.rds") |>
  run_cates(covars_all)

# The original version asked each respondent one of five questions; the
# replication asked all five. Rows are respondent-question pairs.
expert_economists <- read_study("expert_economists_stacked_long.rds") |>
  filter(!is.na(Z), !is.na(Y_s)) |>
  run_cates(covars_all)

mental_illness <- read_study("mental_illness_stacked.rds") |>
  run_cates(covars_all, fml = Y_s ~ Z + Z_mcginty_policy)

death_penalty <- read_study("death_penalty_stacked.rds") |>
  filter(Z_CP_3 != "control") |>
  run_cates(c("age_3", "white", "educ_3", "female", "ideo_3"))

# Mullinix, Leeper, Druckman and Freese (2015) studies ----
# Every one of these is an unadjusted difference-in-means within the subgroup.

berganS20 <- read_study("berganS20_stacked.rds") |> run_cates(covars_all)
brandtS1 <- read_study("brandtS1_stacked.rds") |> run_cates(covars_no_ideo)
caprarielloS2 <- read_study("caprarielloS2_stacked.rds") |> run_cates(covars_all)
converseS16 <- read_study("converseS16_stacked.rds") |> run_cates(covars_no_pid_no_ideo)
dennyS17 <- read_study("dennyS17_stacked.rds") |> run_cates(covars_all)
flavinS4 <- read_study("flavinS4_stacked.rds") |> run_cates(covars_all)
gashS5 <- read_study("gashS5_stacked.rds") |> run_cates(covars_all)
jacobsenS7 <- read_study("jacobsenS7_stacked.rds") |> run_cates(covars_all)
melloS6 <- read_study("melloS6_stacked.rds") |> run_cates(covars_no_pid_no_ideo)
parmerS15 <- read_study("parmerS15_stacked.rds") |> run_cates(covars_all)
piazzaS8 <- read_study("piazzaS8_stacked.rds") |> run_cates(covars_all)
shaferS9 <- read_study("shaferS9_stacked.rds") |> run_cates(covars_all)
thompsonS10 <- read_study("thompsonS10_stacked.rds") |> run_cates(covars_all)
turagaS11 <- read_study("turagaS11_stacked.rds") |> run_cates(covars_all)
wallaceS12 <- read_study("wallaceS12_stacked.rds") |> run_cates(covars_all)

# Every MTurk respondent over 60 in the Pedulla replication gives the same
# answer, so the replication CATE for that cell is exactly zero with a zero
# standard error and the comparison carries no information. The original
# version of that cell is well behaved; the pair is dropped, not the cell.
pedullaS18 <- read_study("pedullaS18_stacked.rds") |>
  run_cates(covars_all) |>
  filter(group != "age_3_More than 60")

# Assemble ----

cate_estimates <- bind_rows(
  superordinate_id = superordinate_id,
  patriot_act = patriot_act,
  elite_endorsements = elite_endorsements,
  free_trade = free_trade,
  frame_breadth = frame_breadth,
  polarization = polarization,
  immigration = immigration,
  system_threat = system_threat,
  expert_economists = expert_economists,
  mental_illness = mental_illness,
  death_penalty = death_penalty,
  berganS20 = berganS20,
  brandtS1 = brandtS1,
  caprarielloS2 = caprarielloS2,
  converseS16 = converseS16,
  dennyS17 = dennyS17,
  flavinS4 = flavinS4,
  gashS5 = gashS5,
  jacobsenS7 = jacobsenS7,
  melloS6 = melloS6,
  parmerS15 = parmerS15,
  pedullaS18 = pedullaS18,
  piazzaS8 = piazzaS8,
  shaferS9 = shaferS9,
  thompsonS10 = thompsonS10,
  turagaS11 = turagaS11,
  wallaceS12 = wallaceS12,
  .id = "study"
) |>
  mutate(
    covariate = str_split_i(group, "_", 1),
    sample = if_else(sample == "gfk", "original", sample),
    significant = as.numeric(p.value <= 0.05)
  ) |>
  left_join(study_labels_df, by = "study") |>
  select(study, study_label, covariate, group, sample, term, estimate,
         std.error, statistic, p.value, conf.low, conf.high, df, nobs,
         significant) |>
  arrange(study, group, sample, term)

write_csv(cate_estimates, file.path(out_dir, "cate_estimates.csv"))

# One row per comparison ----
# The treatment coefficient only, with the original and replication estimates
# side by side. The standard error of the difference treats the two versions as
# independent, and the test is a normal approximation, as the paper states.

cate_scatter <- cate_estimates |>
  filter(term == "Z") |>
  pivot_wider(
    id_cols = c(study, study_label, covariate, group),
    names_from = sample,
    values_from = c(estimate, std.error, conf.low, conf.high, p.value, significant)
  ) |>
  mutate(
    group_label = as.character(factor(group, levels = group_levels,
                                      labels = group_figure_labels)),
    est_diff_in_cates = estimate_mt - estimate_original,
    std.error_diff_in_cates = sqrt(std.error_mt^2 + std.error_original^2),
    li_diff_in_cates = est_diff_in_cates - 1.96 * std.error_diff_in_cates,
    ui_diff_in_cates = est_diff_in_cates + 1.96 * std.error_diff_in_cates,
    p_diff_in_cates = 2 * pnorm(abs(est_diff_in_cates / std.error_diff_in_cates),
                                lower.tail = FALSE),
    diff_in_cates_significant = p_diff_in_cates < 0.05
  ) |>
  filter(!is.na(diff_in_cates_significant)) |>
  arrange(study, group)

write_csv(cate_scatter, file.path(out_dir, "cate_scatter.csv"))

print(paste("CATE estimates:", nrow(cate_estimates), "rows;",
            "comparisons:", nrow(cate_scatter)))

# coppock_leeper_mullinix_2018/maintained/helpers.R
# Output: none
# Depends on: nothing
# Description: Packages, paths, study labels and the two helpers shared by every
#   analysis script.

library(here)
library(tidyverse)
library(estimatr)
library(broom)
library(deming)
library(lmtest)

here::i_am("maintained/helpers.R")

data_dir <- here::here("original", "replication_archive")
out_dir <- here::here("maintained", "output")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Subgroup CATE estimation ----
# One difference-in-means per (covariate level, sample) cell of one study.
# The covariate columns are mixed factor and numeric across studies, so they are
# stacked as character. fml carries any additional regressors the study needs
# (a blocking indicator, or party identification where assignment probabilities
# differ by party). w_col = NULL for the one study with no survey weights.
run_cates <- function(df, covars, fml = Y_s ~ Z, w_col = "weights") {
  df |>
    pivot_longer(all_of(covars), names_to = "col", values_to = "value",
                 values_transform = list(value = as.character)) |>
    mutate(group = paste0(col, "_", value)) |>
    group_by(group, sample) |>
    reframe({
      d <- pick(everything())
      fit <- if (is.null(w_col)) {
        lm_robust(fml, data = d)
      } else {
        lm_robust(fml, data = d, weights = d[[w_col]])
      }
      tidy(fit) |> mutate(nobs = fit$nobs)
    })
}

# Tidy a deming fit ----
# broom has no method for the class, and the jackknife confidence limits are in
# a matrix rather than named columns.
tidy_deming <- function(fit) {
  tibble(
    term = names(fit$coefficients),
    estimate = unname(fit$coefficients),
    std.error = unname(fit$se),
    conf.low = unname(fit$ci[, 1]),
    conf.high = unname(fit$ci[, 2])
  )
}

# Study labels ----
# The deposit's CLM_study_labels.R carries a 28th study, concealed_carry
# (Haider-Markel and Joslyn 2001), which no analysis uses and which the paper
# does not report. The 27 below are the study pairs the paper analyses.
study_labels_df <- tibble(
  study = c(
    "superordinate_id", "patriot_act", "elite_endorsements", "free_trade",
    "frame_breadth", "polarization", "immigration", "system_threat",
    "expert_economists", "mental_illness", "death_penalty",
    "berganS20", "brandtS1", "caprarielloS2", "converseS16", "dennyS17",
    "flavinS4", "gashS5", "jacobsenS7", "melloS6", "parmerS15", "pedullaS18",
    "piazzaS8", "shaferS9", "thompsonS10", "turagaS11", "wallaceS12"
  ),
  study_label = c(
    "Transue (2007)", "Chong and Druckman (2010)", "Nicholson (2012)",
    "Hiscox (2006)", "Hopkins and Mummolo (2017)",
    "Levendusky and Malhotra (2015)", "Brader (2005)",
    "Craig and Richeson (2014)", "Johnston and Ballard (2016)",
    "McGinty, Webster and Barry (2013)", "Peffley and Hurwitz (2007)",
    "Bergan (2012)", "Brandt (2013)", "Caprariello and Reis (2013)",
    "Epley et al. (2009)", "Denny (2012)", "Flavin (2011)",
    "Gash and Murakami (2009)", "Jacobsen, Snyder and Saultz (2014)",
    "Murtagh et al. (2012)", "Parmer (2011)", "Pedulla (2014)",
    "Piazza (2015)", "Shafer (2017)", "Thompson and Schlehofer (2014)",
    "Turaga (2010)", "Wallace (2011)"
  )
)

# Covariate sets ----
# Six pretreatment characteristics coarsened to at most three categories each.
# Studies that did not measure one of them drop it.
covars_all <- c("age_3", "pid_3", "white", "educ_3", "female", "ideo_3")
covars_no_ideo <- c("age_3", "pid_3", "white", "educ_3", "female")
covars_no_pid_no_ideo <- c("age_3", "white", "educ_3", "female")

# Subgroup labels ----
# Figure 1 panels, in the order the published figure lays them out. The two
# place-holder levels keep the two-level covariates (gender, race) aligned with
# the three-level ones in a six by three grid.
group_levels <- c(
  "age_3_18 - 39", "age_3_40 - 59", "age_3_More than 60",
  "educ_3_Less than College", "educ_3_College", "educ_3_Graduate School",
  "female_0", "female_1", "female_place_holder",
  "ideo_3_Liberal", "ideo_3_Moderate", "ideo_3_Conservative",
  "pid_3_Democrat", "pid_3_Independent", "pid_3_Republican",
  "white_0", "white_1", "white_place_holder"
)
group_figure_labels <- c(
  "Age: 18 - 39", "Age: 40 - 59", "Age: More than 60",
  "Less than College", "College", "Graduate School",
  "Men", "Women", "",
  "Liberal", "Moderate", "Conservative",
  "Democrat", "Independent", "Republican",
  "Nonwhite", "White", " "
)

# Appendix table row labels, in the order the published appendix tables use.
appendix_group_levels <- c(
  "age_3_18 - 39", "age_3_40 - 59", "age_3_More than 60",
  "pid_3_Democrat", "pid_3_Independent", "pid_3_Republican",
  "white_1", "white_0",
  "educ_3_Less than College", "educ_3_College", "educ_3_Graduate School",
  "female_0", "female_1",
  "ideo_3_Liberal", "ideo_3_Moderate", "ideo_3_Conservative"
)
appendix_group_labels <- c(
  "Age: 18 - 39", "Age: 40 - 59", "Age: More than 60",
  "Party ID: Democrat", "Party ID: Independent", "Party ID: Republican",
  "Race: White", "Race: Nonwhite",
  "Education: Less than College", "Education: College",
  "Education: Graduate School",
  "Gender: Male", "Gender: Female",
  "Ideology: Liberal", "Ideology: Moderate", "Ideology: Conservative"
)

# Blank a figure PDF's embedded timestamps ----
# R's pdf() device stamps /CreationDate and /ModDate with the wall clock, so an
# otherwise deterministic pipeline writes a different file on every run. The epoch
# string is the same width as what it replaces, which keeps the cross-reference byte
# offsets valid, and a file with no timestamp is left alone.
blank_pdf_timestamps <- function(path) {
  epoch <- charToRaw("D:19700101000000")
  raw_pdf <- readBin(path, "raw", file.size(path))
  hits <- grepRaw("D:[0-9]{14}", raw_pdf, all = TRUE)
  if (length(hits) == 0) return(invisible(path))
  for (h in hits) raw_pdf[h:(h + length(epoch) - 1L)] <- epoch
  writeBin(raw_pdf, path)
  invisible(path)
}

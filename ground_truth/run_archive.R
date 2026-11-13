# coppock_leeper_mullinix_2018/ground_truth/run_archive.R
# Output: ground_truth/archive_run_status.csv, ground_truth/archive_overwrites.csv
# Depends on: original/ (run download_original.R first)
# Description: Runs the eight deposited scripts, unmodified, in the order the
#   deposit's README gives, twice. The as_shipped pass runs them in a copy of the
#   whole deposit. The stripped pass runs them in a copy holding only the 27 data
#   files and the 8 scripts, with the 10 files the scripts themselves write
#   withheld, which is the only way to see whether a script passes on its own
#   work or on an intermediate the deposit ships. Neither pass writes to
#   original/: each runs in its own temporary directory.
#
#   archive_run_status.csv has one row per script per pass. archive_overwrites.csv
#   records what each pass did to the deposited files: under the stripped pass
#   every withheld file is either rebuilt or still withheld, and under the
#   as_shipped pass a file has a row only where the run wrote over it. Files a run
#   writes that the deposit does not carry are recorded as added.
#
#   This script is not part of run_all.R, because the deposit's own estimation
#   takes several minutes and its results do not change between runs. The five
#   one-line repairs the README describes are not applied here: these tables
#   record the deposit as it stands.

library(here)
library(tidyverse)

here::i_am("ground_truth/run_archive.R")

options(width = 200)

deposit_dir <- here::here("original", "replication_archive")

# The deposit's own order, from its README.txt ----
scripts <- tribble(
  ~script, ~role,
  "CLM_study_labels.R", "cleaning",
  "CLM_study_summaries.R", "analysis",
  "CLM_estimate_CATEs.R", "analysis",
  "CLM_f_tests.R", "analysis",
  "CLM_figures.R", "analysis",
  "CLM_tables.R", "analysis",
  "CLM_in_text_figures.R", "analysis",
  "CLM_appendix.R", "analysis"
)

# The ten files the scripts write, which the deposit also ships.
script_outputs <- c(
  "CLM_study_labels_df.rds", "CLM_study_ns_df.rds", "CLM_results.rds",
  "CLM_scatter_df.rds", "CLM_f_tests_df.rds", "CLM_PNAS_FINAL_FIGURE1.pdf",
  "CLM_PNAS_FINAL_FIGURE2.pdf", "group_table.tex", "study_table.tex",
  "appendix_tables.tex"
)

deposited_files <- list.files(deposit_dir)
stopifnot(all(scripts$script %in% deposited_files),
          all(script_outputs %in% deposited_files))

# Comparing a written file with the deposited copy ----
# A .tex file is compared line for line once the two comment lines xtable stamps with
# its own version and the wall-clock time are dropped. An .rds object is compared on its
# contents, since the serialisation records the R version that wrote it and the
# deposited CLM_results.rds is a grouped data frame in a format dplyr has since
# dropped. A PDF is compared byte for byte once its embedded timestamps are blanked.
blank_timestamps <- function(path) {
  raw_bytes <- read_file_raw(path)
  text <- rawToChar(raw_bytes[raw_bytes != as.raw(0)])
  str_remove_all(text, "/(CreationDate|ModDate) \\(D:[^)]*\\)")
}

same_contents <- function(written, deposited) {
  if (str_ends(written, "\\.rds")) {
    a <- read_rds(written)
    b <- read_rds(deposited)
    a <- if (is.data.frame(a)) as_tibble(ungroup(as_tibble(a))) else a
    b <- if (is.data.frame(b)) as_tibble(ungroup(as_tibble(b))) else b
    isTRUE(all.equal(a, b, tolerance = 1e-10, check.attributes = FALSE))
  } else if (str_ends(written, "\\.pdf")) {
    blank_timestamps(written) == blank_timestamps(deposited)
  } else {
    drop_comments <- function(path) str_subset(read_lines(path), "^%", negate = TRUE)
    identical(drop_comments(written), drop_comments(deposited))
  }
}

# The error a script stopped on: its first line and what follows, up to the backtrace.
first_error <- function(log_lines) {
  error_at <- str_which(log_lines, "^Error")
  if (length(error_at) == 0) return("No error message; see the exit status")
  tail_lines <- log_lines[error_at[1]:length(log_lines)]
  end_at <- str_which(tail_lines, "^(Backtrace|Execution halted|In addition)")
  keep <- head(tail_lines, min(c(end_at - 1, 4)))
  str_squish(str_flatten(keep, " "))
}

# One pass ----
# Every file in the run directory is given a fixed old modification time first,
# so a file a script writes, even with identical bytes, shows up as written.
run_pass <- function(pass, withheld) {
  run_dir <- file.path(tempdir(), paste0("archive_", pass))
  unlink(run_dir, recursive = TRUE)
  dir.create(run_dir)
  copied <- setdiff(deposited_files, withheld)
  file.copy(file.path(deposit_dir, copied), run_dir)
  Sys.setFileTime(file.path(run_dir, copied), as.POSIXct("2000-01-01", tz = "UTC"))

  status <- scripts |>
    mutate(result = map(script, function(s) {
      log_file <- file.path(run_dir, paste0(s, ".log"))
      exit_status <- withr::with_dir(
        run_dir,
        system2("Rscript", c("--vanilla", s), stdout = log_file, stderr = log_file,
                timeout = 1800)
      )
      tibble(
        exit_status = exit_status,
        outcome = case_when(exit_status == 0 ~ "clean", exit_status == 124 ~ "timeout",
                            .default = "error"),
        stopped_at = if (exit_status == 0) NA_character_ else first_error(read_lines(log_file))
      )
    })) |>
    unnest(result) |>
    mutate(pass = pass, .before = 1)

  after <- tibble(file = setdiff(list.files(run_dir), paste0(scripts$script, ".log"))) |>
    mutate(written = file.mtime(file.path(run_dir, file)) >
             as.POSIXct("2000-01-02", tz = "UTC"))

  effects <- bind_rows(
    tibble(file = withheld) |>
      mutate(effect = if_else(file %in% after$file, "rebuilt", "withheld")),
    after |>
      filter(file %in% copied, written) |>
      transmute(file, effect = "overwritten"),
    after |>
      filter(!file %in% deposited_files) |>
      transmute(file, effect = "added")
  ) |>
    mutate(
      matches_deposit = map2_lgl(file, effect, function(f, e) {
        if (e %in% c("added", "withheld")) return(NA)
        same_contents(file.path(run_dir, f), file.path(deposit_dir, f))
      }),
      pass = pass, .before = 1
    )

  list(status = status, effects = effects)
}

as_shipped <- run_pass("as_shipped", withheld = character(0))
stripped <- run_pass("stripped", withheld = script_outputs)

# Write ----
run_status <- bind_rows(as_shipped$status, stripped$status) |>
  select(pass, script, role, exit_status, outcome, stopped_at)
overwrites <- bind_rows(as_shipped$effects, stripped$effects) |>
  select(pass, file, effect, matches_deposit)

stopifnot(nrow(run_status) == 2 * nrow(scripts),
          all(run_status$outcome %in% c("clean", "error", "timeout", "not_run")),
          all(overwrites$effect %in% c("added", "overwritten", "rebuilt", "withheld", "deleted")))

write_csv(run_status, here::here("ground_truth", "archive_run_status.csv"))
write_csv(overwrites, here::here("ground_truth", "archive_overwrites.csv"))

print(run_status, n = Inf)
print(overwrites, n = Inf)

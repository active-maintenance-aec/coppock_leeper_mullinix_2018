# coppock_leeper_mullinix_2018/run_all.R
# Runs the whole reproduction in order: fetch and verify the deposited archive,
# then the estimates every table and figure rests on, then the published tables,
# the figures, the in-text quantities, and finally the ground truth.
# Every script is self-contained and can also be run on its own, in the order
# below, since each reads its inputs from maintained/output/.

library(here)
here::i_am("run_all.R")

# Deposited archive ----
# Downloads from Dataverse on a fresh clone; verifies checksums either way.
source(here::here("download_original.R"))

# Estimates ----
# Every published number rests on these two scripts. estimate_cates.R takes
# about a minute; f_tests.R about three.
source(here::here("maintained", "estimate_cates.R"))
source(here::here("maintained", "f_tests.R"))
source(here::here("maintained", "study_ns.R"))

# Tables ----
source(here::here("maintained", "table_1_within_study_correspondence.R"))
source(here::here("maintained", "table_2_across_study_correspondence.R"))
source(here::here("maintained", "table_a_cate_estimates.R"))

# Figures ----
source(here::here("maintained", "figure_1_across_study_correspondence.R"))
source(here::here("maintained", "figure_2_within_study_correspondence.R"))

# In-text quantities ----
source(here::here("maintained", "text_correspondence_summary.R"))
source(here::here("maintained", "text_within_study_correlation.R"))

# Figure timestamps ----
# R's pdf() device stamps a wall-clock /CreationDate and /ModDate into every figure it
# writes, and those two fields are the only reason two runs of this pipeline produce
# differing files. Blanking them lets the determinism check cover every file the
# pipeline writes rather than all but the figures.
source(here::here("maintained", "helpers.R"))
walk(
  list.files(here::here("maintained", "output"), pattern = "\\.pdf$", full.names = TRUE),
  blank_pdf_timestamps
)

# Ground truth ----
# Rebuilds the comparison table from the outputs above, so it cannot go stale.
# Its last step is the coverage gate, which runs maintained/in_text_claims.R as a
# program and halts unless the claims it prints are exactly the claims the
# extraction requires and agree with the ground truth at the article's precision.
source(here::here("ground_truth", "build_ground_truth.R"))

# In-text claims ----
# The second instrument, run again here for the human-readable log. Each line
# pairs a sentence from the article with the number the pipeline gives for it.
source(here::here("maintained", "in_text_claims.R"))

# Deposited archive, again ----
# The check at the top of this file is a precondition: it says original/ was intact
# before anything ran. Nothing above writes to original/, and this second pass is what
# demonstrates it rather than assuming it. Nothing is downloaded; the files are already
# present and are re-checked against the manifest on checksum, byte size and membership.
source(here::here("download_original.R"))

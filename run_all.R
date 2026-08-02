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

# Ground truth ----
# Rebuilds the comparison table from the outputs above, so it cannot go stale.
source(here::here("ground_truth", "build_ground_truth.R"))

# Deposited archive, again ----
# The check at the top of this file is a precondition: it says original/ was intact
# before anything ran. Nothing above writes to original/, and this second pass is what
# demonstrates it rather than assuming it. Nothing is downloaded; the files are already
# present and are re-checked against the manifest on checksum, byte size and membership.
source(here::here("download_original.R"))

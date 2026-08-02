# coppock_leeper_mullinix_2018/download_original.R
# Output: original/ (the deposited replication archive, not redistributed in this repo)
# Depends on: original_manifest.csv
# Description: Fetch the deposited archive from Harvard Dataverse and verify
#   every file. Run this once before running anything in maintained/.
#   Re-running is free: files already present with the right checksum are not
#   downloaded again.
#
#   The manifest carries two checksums per file. md5_served is the MD5 of the
#   bytes Dataverse returns for `?format=original`, which is what this code was
#   written against. md5_published is the checksum Dataverse displays. Here all
#   forty-six agree, but they do not always: other deposits carry published
#   checksums that verify neither the original nor the derived tabular file, so
#   verification runs against the served bytes and any disagreement is reported.
#
#   The deposit places every file in a replication_archive/ directory, which
#   the manifest records and this script recreates.

library(tidyverse)
library(here)

here::i_am("download_original.R")

dataset_doi <- "doi:10.7910/DVN/4WNGEJ"
base_url <- "https://dataverse.harvard.edu/api/access/datafile"

# Manifest ----
manifest <- read_csv(here::here("original_manifest.csv"), show_col_types = FALSE)

walk(unique(dirname(here::here("original", manifest$file))),
     function(d) dir.create(d, showWarnings = FALSE, recursive = TRUE))

# Download what is missing or wrong ----
# format=original asks for the deposited bytes rather than any representation
# Dataverse derives.
planned <- manifest |>
  mutate(
    path = here::here("original", file),
    url = str_glue("{base_url}/{dataverse_file_id}?format=original"),
    md5_local = unname(tools::md5sum(path)),
    needs_download = is.na(md5_local) | md5_local != md5_served
  )

walk2(
  planned$url[planned$needs_download],
  planned$path[planned$needs_download],
  function(url, path) download.file(url, destfile = path, mode = "wb", quiet = TRUE)
)

print(str_glue("Downloaded {sum(planned$needs_download)} of {nrow(planned)} files; ",
               "{sum(!planned$needs_download)} already present and verified."))

# Verify ----
verified <- planned |>
  mutate(
    md5_downloaded = unname(tools::md5sum(path)),
    match = md5_downloaded == md5_served,
    published_agrees = md5_served == md5_published
  ) |>
  select(file, bytes, md5_served, md5_downloaded, match, published_agrees)

print(verified, n = nrow(verified))

if (!all(verified$match)) {
  stop("Checksum mismatch: the downloaded archive does not match what Dataverse served when this code was written.")
}

# The deposit and nothing else ----
# A file in original/ that the manifest does not list means the directory has
# picked up something the archive did not ship, which is how running an
# archive's own scripts inside it silently overwrites deposited output.
present <- list.files(here::here("original"), recursive = TRUE)
extra <- setdiff(present, manifest$file)
if (length(extra) > 0) {
  print(str_glue("original/ holds {length(extra)} file(s) the deposit does not: ",
                 "{paste(extra, collapse = ', ')}"))
  stop("original/ must contain the deposit and nothing else.")
}

print(str_glue("All {nrow(verified)} files match, and original/ holds nothing else. ",
               "{sum(!verified$published_agrees)} carry a published checksum that disagrees."))
print(str_glue("Archive: {dataset_doi}"))

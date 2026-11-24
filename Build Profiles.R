# Standalone entry point for generating Word Locality Profiles.
# The reusable generator lives in R/build_profiles.R and can also be called
# by the future Shiny queue worker with an explicit locality selection.

rlang::check_installed(
  pkg = "phstemplates",
  reason = "v1.3.0 is needed to apply sensitivity labels",
  version = "1.3.0",
  action = \(pkg, ...) {
    remotes::install_github(paste0("Public-Health-Scotland/", pkg))
  }
)

source("Master RMarkdown Document & Render Code/Global Script.R")
source("R/build_profiles.R")

output_dir <- fs::path(lp_path, "Profiles Output")
lookup <- read_in_localities()

# Edit this selection for a standalone run:
# hscp_list <- unique(lookup$hscp2019name) # all HSCPs
hscp_list <- c("West Lothian", "Angus")

stopifnot(all(hscp_list %in% unique(lookup[["hscp2019name"]])))

localities <- unlist(
  lapply(hscp_list, function(hscp) {
    lookup$hscp_locality[lookup$hscp2019name == hscp]
  }),
  use.names = FALSE
)

result <- build_profiles(
  localities = localities,
  output_dir = output_dir,
  progress = function(event) {
    if (event$status %in% c("succeeded", "failed")) {
      message(sprintf(
        "[%d/%d] %s: %s",
        event$position, event$total, event$locality, event$status
      ))
    }
  }
)

print(result$results)
message(sprintf(
  "Profiles complete: %d succeeded, %d failed.",
  result$succeeded, result$failed
))
if (result$failed > 0L) {
  warning("Some Locality Profiles could not be generated.", call. = FALSE)
}

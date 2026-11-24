# Standalone entry point for generating Word Locality Profiles.

rlang::check_installed(
  pkg = "phstemplates",
  reason = "v1.3.0 is needed to apply sensitivity labels",
  version = "1.3.0",
  action = \(pkg, ...) {
    remotes::install_github(paste0("Public-Health-Scotland/", pkg))
  }
)

source("Master RMarkdown Document & Render Code/Global Script.R")

output_dir <- fs::path(lp_path, "Profiles Output")
lookup <- read_in_localities()

# Edit this selection for a standalone run:
# hscp_list <- unique(lookup$hscp2019name) # all HSCPs
hscp_list <- c("West Lothian", "Angus")

unknown_hscps <- setdiff(hscp_list, lookup$hscp2019name)
if (length(unknown_hscps) > 0) {
  stop("Unknown HSCPs: ", paste(unknown_hscps, collapse = ", "))
}

localities <- purrr::map(hscp_list, function(HSCP) {
  lookup |>
    filter(hscp2019name == HSCP) |>
    pull(hscp_locality)
}) |>
  purrr::list_c()

result <- build_profiles(
  localities = localities,
  output_dir = output_dir,
  progress = function(event) {
    if (event$status %in% c("succeeded", "failed")) {
      message(sprintf(
        "[%d/%d] %s: %s",
        event$position,
        event$total,
        event$locality,
        event$status
      ))
    }
  }
)

print(result$results)
message(sprintf(
  "Profiles complete: %d succeeded, %d failed.",
  result$succeeded,
  result$failed
))
if (result$failed > 0L) {
  warning("Some Locality Profiles could not be generated.", call. = FALSE)
}

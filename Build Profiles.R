source("Master RMarkdown Document & Render Code/Global Script.R")

output_dir <- fs::path(lp_path, "Profiles Output")
lookup <- read_in_localities()

hscp_list <- c("Orkney Islands")
create_sdc <- FALSE
create_data_workbook <- FALSE

unknown_hscps <- setdiff(hscp_list, lookup$hscp2019name)
if (length(unknown_hscps) > 0) {
  stop("Unknown HSCPs: ", toString(unknown_hscps), call. = FALSE)
}

# Keep localities in the selected HSCP order.
localities <- lookup |>
  filter(hscp2019name %in% hscp_list) |>
  arrange(match(hscp2019name, hscp_list)) |>
  pull(hscp_locality)

result <- build_profiles(
  localities = localities,
  output_dir = output_dir,
  create_sdc = create_sdc,
  create_data_workbook = create_data_workbook,
  progress = function(event) {
    if (
      event$type == "locality_profile" &&
        event$status %in% c("succeeded", "failed")
    ) {
      message(sprintf(
        "[%d/%d] %s: %s",
        event$position,
        event$total,
        event$locality,
        event$status
      ))
    } else if (event$type != "locality_profile") {
      message(sprintf("%s (%s): %s", event$hscp, event$type, event$status))
    }
  }
)

print(result$results)
if (create_sdc || create_data_workbook) {
  print(result$workbook_results)
}
message(sprintf(
  "Profiles complete: %d succeeded, %d failed.",
  result$succeeded,
  result$failed
))
if (result$failed > 0L) {
  warning("Some Locality Profiles could not be generated.", call. = FALSE)
}
if (result$workbook_failed + result$workbook_skipped > 0L) {
  warning("Some HSCP workbooks were not generated.", call. = FALSE)
}

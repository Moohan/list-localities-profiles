# Lightweight tests for the reusable generator helpers.
# Run from the repository root: Rscript tests/test-build-profiles.R
# These tests do not use server-only PHS input data or Word templates.

source("R/build_profiles.R")

lookup <- data.frame(
  hscp_locality = c("Area A", "Area B", "Area C"),
  hscp2019name = c("HSCP 1", "HSCP 1", "HSCP 2"),
  stringsAsFactors = FALSE
)

items <- validate_profile_selection(c("Area C", "Area A", "Area B"), lookup)
stopifnot(
  identical(items$locality, c("Area C", "Area A", "Area B")),
  identical(items$hscp, c("HSCP 2", "HSCP 1", "HSCP 1")),
  identical(
    profile_output_filename("Area/West"),
    "Area-West - Locality Profile.docx"
  )
)

expect_error <- function(expr) {
  inherits(tryCatch(force(expr), error = identity), "error")
}

stopifnot(
  expect_error(validate_profile_selection(character(), lookup)),
  expect_error(validate_profile_selection(c("Area A", "Area A"), lookup)),
  expect_error(validate_profile_selection("Unknown", lookup))
)

ambiguous <- rbind(
  lookup,
  data.frame(hscp_locality = "Area A", hscp2019name = "HSCP 3")
)
stopifnot(expect_error(validate_profile_selection("Area A", ambiguous)))

collision <- data.frame(
  hscp_locality = c("Area/West", "Area-West"),
  hscp2019name = c("HSCP 1", "HSCP 1")
)
stopifnot(expect_error(
  validate_profile_selection(c("Area/West", "Area-West"), collision)
))

events <- list()
result <- run_profile_items(
  items,
  processor = function(item) {
    if (item$locality == "Area A") stop("Illustrative analysis failure")
    paste0(item$locality, ".docx")
  },
  progress = function(event) {
    events[[length(events) + 1L]] <<- event
  }
)

stopifnot(
  identical(result$total, 3L),
  result$succeeded == 2L,
  result$failed == 1L,
  identical(result$results$status, c("succeeded", "failed", "succeeded")),
  grepl("Illustrative analysis failure", result$results$error[[2]]),
  length(events) == 6L,
  identical(vapply(events, \`[[\`, character(1), "status"),
            c("running", "succeeded", "running", "failed",
              "running", "succeeded"))
)

# On Linux the staging rename must replace an existing destination without
# first deleting it. Use group = NULL to avoid requiring server group access.
if (.Platform$OS.type == "unix") {
  temporary <- tempfile("lp-publish-test-")
  dir.create(temporary)
  on.exit <- NULL
  original <- file.path(temporary, "source.docx")
  target <- file.path(temporary, "existing.docx")
  writeLines("replacement", original)
  writeLines("original", target)
  publish_profile_document(original, target, group = NULL)
  stopifnot(
    identical(readLines(target), "replacement"),
    identical(readLines(original), "replacement")
  )
  unlink(temporary, recursive = TRUE)
}

message("All reusable profile-generator helper tests passed.")

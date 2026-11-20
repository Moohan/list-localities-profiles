# Publish checked locality profiles, SDC workbooks and background data.
# Run from the repository root in RStudio (Source) or an interactive R console.

source("Master RMarkdown Document & Render Code/Global Script.R")
library(cli)

default_release <- "2026-April"
output_group <- "LIST_analytics"

ask_text <- function(question, default = NULL) {
  cli::cli_text("{question}")
  answer <- if (interactive()) {
    readline("  > ")
  } else {
    cat("  > ")
    flush.console()
    readLines(stdin(), n = 1L, warn = FALSE)
  }
  if (!length(answer)) {
    cli::cli_abort(
      "No input received. Run this script in an interactive console."
    )
  }
  answer <- trimws(answer)
  if (!nzchar(answer) && !is.null(default)) default else answer
}

ask_yes_no <- function(question, default = FALSE) {
  hint <- if (default) "[Y/n]" else "[y/N]"
  repeat {
    answer <- tolower(ask_text(paste(question, hint), default = ""))
    if (!nzchar(answer)) {
      return(default)
    }
    if (answer %in% c("y", "yes")) {
      return(TRUE)
    }
    if (answer %in% c("n", "no")) {
      return(FALSE)
    }
    cli::cli_alert_warning("Please answer y or n.")
  }
}

choose_profile_dir <- function(root) {
  dirs <- fs::dir_ls(root, type = "directory", recurse = FALSE)
  dirs <- dirs[grepl("^Profiles Output($| )", fs::path_file(dirs))]
  dirs <- dirs[vapply(
    dirs,
    function(dir) {
      any(endsWith(
        fs::path_file(fs::dir_ls(dir, type = "file")),
        " - Locality Profile.docx"
      ))
    },
    logical(1)
  )]

  if (!length(dirs)) {
    cli::cli_abort(c(
      "No folder containing completed Word locality profiles was found.",
      "i" = "Expected a {.file Profiles Output} or {.file Profiles Output ...} folder under the locality profiles root."
    ))
  }
  if (length(dirs) == 1L) {
    return(dirs[[1L]])
  }

  cli::cli_h2("Choose the Word profile source folder")
  for (i in seq_along(dirs)) {
    folder <- fs::path_file(dirs[[i]])
    cli::cli_text("  {i}. {.file {folder}}")
  }
  repeat {
    choice <- ask_text("Enter the folder number:")
    if (grepl("^[0-9]+$", choice)) {
      i <- as.integer(choice)
      if (!is.na(i) && i >= 1L && i <= length(dirs)) return(dirs[[i]])
    }
    cli::cli_alert_warning("Enter a number between 1 and {length(dirs)}.")
  }
}

# Identify only expected output filenames, then associate each with an HSCP.
make_manifest <- function(
  source,
  suffix,
  kind,
  lookup,
  destination,
  by_locality
) {
  if (!fs::dir_exists(source)) {
    cli::cli_abort("Missing source directory for {kind}: {.path {source}}")
  }
  paths <- fs::dir_ls(source, type = "file", recurse = FALSE)
  names <- as.character(fs::path_file(paths))
  keep <- endsWith(names, suffix) & !startsWith(names, "~$")
  paths <- as.character(paths[keep])
  names <- names[keep]

  if (!length(paths)) {
    cli::cli_alert_warning("No {kind} files found in {.path {source}}")
    return(data.frame(
      kind = character(),
      source = character(),
      target = character()
    ))
  }

  area <- substr(names, 1L, nchar(names) - nchar(suffix))
  match_against <- if (by_locality) lookup$hscp_locality else unique(lookup$hscp2019name)
  hscp <- if (by_locality) {
    lookup$hscp2019name[match(area, lookup$hscp_locality)]
  } else {
    area[match(area, match_against)]
  }

  unmatched <- is.na(hscp)
  if (any(unmatched)) {
    cli::cli_alert_warning(
      "Skipping {sum(unmatched)} unrecognised {kind} filename{?s}."
    )
    for (name in head(names[unmatched], 8L)) {
      cli::cli_text("  - {.file {name}}")
    }
  }
  valid <- !unmatched
  data.frame(
    kind = rep(kind, sum(valid)),
    source = paths[valid],
    target = as.character(fs::path(destination, hscp[valid], names[valid])),
    stringsAsFactors = FALSE
  )
}

# Retain the original LIST group setting where the filesystem permits it.
set_group <- function(path) {
  if (.Platform$OS.type != "unix") {
    return(TRUE)
  }
  tryCatch(
    {
      fs::file_chown(path, group_id = output_group)
      TRUE
    },
    error = function(e) FALSE
  )
}

copy_stage <- function(files, label, overwrite) {
  cli::cli_h2("Copy {label}")
  if (!nrow(files)) {
    cli::cli_alert_warning("No matching files to copy.")
    return(c(
      copied = 0L,
      replaced = 0L,
      skipped = 0L,
      failed = 0L,
      group_warnings = 0L
    ))
  }

  results <- c(
    copied = 0L,
    replaced = 0L,
    skipped = 0L,
    failed = 0L,
    group_warnings = 0L
  )
  progress <- cli::cli_progress_bar(
    name = paste("Processing", label),
    total = nrow(files),
    clear = FALSE
  )
  for (i in seq_len(nrow(files))) {
    src <- files$source[[i]]
    dst <- files$target[[i]]
    existed <- fs::file_exists(dst)
    if (existed && !overwrite) {
      results[["skipped"]] <- results[["skipped"]] + 1L
      cli::cli_progress_update(id = progress)
      next
    }

    success <- tryCatch(
      {
        folder <- fs::path_dir(dst)
        new_folder <- !fs::dir_exists(folder)
        fs::dir_create(folder, recurse = TRUE, mode = "ug=rwx,o=rx")
        if (new_folder && !set_group(folder)) {
          results[["group_warnings"]] <- results[["group_warnings"]] + 1L
        }
        fs::file_copy(src, dst, overwrite = overwrite)
        if (!set_group(dst)) {
          results[["group_warnings"]] <- results[["group_warnings"]] + 1L
        }
        TRUE
      },
      error = function(e) {
        message <- conditionMessage(e)
        name <- fs::path_file(src)
        cli::cli_alert_danger("Failed {.file {name}}: {message}")
        FALSE
      }
    )
    if (success) {
      key <- if (existed) "replaced" else "copied"
      results[[key]] <- results[[key]] + 1L
    } else {
      results[["failed"]] <- results[["failed"]] + 1L
    }
    cli::cli_progress_update(id = progress)
  }
  cli::cli_progress_done(id = progress)
  cli::cli_alert_info(
    paste0(
      "{label}: {results[['copied']]} new, {results[['replaced']]} overwritten, ",
      "{results[['skipped']]} skipped, {results[['failed']]} failed."
    )
  )
  results
}

main <- function() {
  cli::cli_h1("Publish final locality profiles")
  old_progress <- options(cli.progress_show_after = 0)
  on.exit(options(old_progress), add = TRUE)
  if (!fs::dir_exists(lp_path)) {
    cli::cli_abort("Locality profiles root is unavailable: {.path {lp_path}}")
  }
  cli::cli_alert_info(
    "Copying checked files only; source files will not be moved."
  )

  release <- ask_text(
    paste0("Release label [", default_release, "]:"),
    default = default_release
  )
  if (!grepl("^[0-9]{4}-[A-Za-z]+$", release)) {
    cli::cli_abort("Release label must use YYYY-Month, e.g. 2026-April.")
  }
  final_dir <- fs::path(
    lp_path,
    "Final Profiles",
    paste0(release, " Final Profiles")
  )
  profile_dir <- choose_profile_dir(lp_path)
  background_dir <- fs::path(
    lp_path,
    "Master RMarkdown Document & Render Code",
    "Output",
    "background data"
  )

  cli::cli_h2("Discover files")
  lookup <- read_in_localities() |>
    dplyr::distinct(hscp_locality, hscp2019name) |>
    dplyr::mutate(
      hscp_locality = dplyr::case_match(
        hscp_locality,
        "Dumbarton/Alexandria" ~ "Dumbarton-Alexandria",
        .default = hscp_locality
      )
    )
  stages <- list(
    "Word profiles" = make_manifest(
      profile_dir,
      " - Locality Profile.docx",
      "Word profiles",
      lookup,
      final_dir,
      by_locality = TRUE
    ),
    "SDC workbooks" = make_manifest(
      background_dir,
      " - Locality Profile SDC highlight.xlsx",
      "SDC workbooks",
      lookup,
      final_dir,
      by_locality = FALSE
    ),
    "Background data workbooks" = make_manifest(
      background_dir,
      " - Locality Profile data.xlsx",
      "Background data workbooks",
      lookup,
      final_dir,
      by_locality = FALSE
    )
  )
  files <- do.call(rbind, stages)
  if (!nrow(files)) {
    cli::cli_abort("No eligible files were found; nothing was changed.")
  }
  if (anyDuplicated(files$target)) {
    cli::cli_abort(
      "Multiple sources would write to the same destination. Nothing was copied."
    )
  }

  cli::cli_h2("Review copy plan")
  cli::cli_text("Destination: {.path {final_dir}}")
  cli::cli_text("Word source: {.path {profile_dir}}")
  cli::cli_text("Excel source: {.path {background_dir}}")
  for (label in names(stages)) {
    count <- nrow(stages[[label]])
    cli::cli_text("  {label}: {count} file{?s}")
  }
  existing <- sum(fs::file_exists(files$target))
  cli::cli_text("Existing destination files: {existing}")
  overwrite <- ask_yes_no(
    "Overwrite existing destination files?",
    default = FALSE
  )
  pending <- if (overwrite) nrow(files) else nrow(files) - existing
  cli::cli_alert_info(
    "{pending} file{?s} to copy; {if (overwrite) existing else 0L} replacement{?s}."
  )
  if (!pending) {
    cli::cli_alert_success("Everything already exists. No files changed.")
    return(invisible(NULL))
  }
  if (!ask_yes_no("Proceed with copying?", default = FALSE)) {
    cli::cli_alert_warning("Cancelled; no files changed.")
    return(invisible(NULL))
  }

  totals <- c(
    copied = 0L,
    replaced = 0L,
    skipped = 0L,
    failed = 0L,
    group_warnings = 0L
  )
  for (label in names(stages)) {
    totals <- totals + copy_stage(stages[[label]], label, overwrite)
  }
  cli::cli_h2("Finished")
  cli::cli_alert_success(
    paste0(
      "{totals[['copied']]} new, {totals[['replaced']]} overwritten, ",
      "{totals[['skipped']]} skipped."
    )
  )
  if (totals[["group_warnings"]] > 0L) {
    cli::cli_alert_warning(
      "Could not set LIST_analytics ownership on {totals[['group_warnings']]} path{?s}; check permissions."
    )
  }
  if (totals[["failed"]] > 0L) {
    cli::cli_abort(
      "{totals[['failed']]} file{?s} failed to copy. See messages above."
    )
  }
  cli::cli_alert_success("Files are ready in {.path {final_dir}}")
  invisible(totals)
}

main()

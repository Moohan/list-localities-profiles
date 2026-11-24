# Reusable Word Locality Profile generator.
#
# This file deliberately has no Shiny or queue dependencies. Source it from the
# repository root. Each call builds profiles sequentially in an isolated analysis
# environment; the calling process is responsible for job-level isolation.

profile_output_filename <- function(locality) {
  if (!is.character(locality) || length(locality) != 1L ||
      is.na(locality) || !nzchar(trimws(locality))) {
    stop("A single non-empty locality name is required.", call. = FALSE)
  }

  # Preserve the naming convention used by Build Profiles.R.
  paste0(gsub("[/\\\\]", "-", locality), " - Locality Profile.docx")
}

validate_profile_selection <- function(localities, lookup) {
  if (!is.character(localities) || length(localities) == 0L ||
      anyNA(localities) || any(!nzchar(trimws(localities)))) {
    stop("localities must be a non-empty character vector.", call. = FALSE)
  }
  if (anyDuplicated(localities)) {
    stop("localities must not contain duplicates.", call. = FALSE)
  }
  if (!is.data.frame(lookup) ||
      !all(c("hscp_locality", "hscp2019name") %in% names(lookup))) {
    stop("lookup must contain hscp_locality and hscp2019name.", call. = FALSE)
  }

  areas <- unique(lookup[c("hscp_locality", "hscp2019name")])
  matches <- areas[areas$hscp_locality %in% localities, , drop = FALSE]
  counts <- table(factor(matches$hscp_locality, levels = localities))

  if (any(counts == 0L)) {
    stop(
      "Unknown localities: ",
      paste(localities[counts == 0L], collapse = ", "),
      call. = FALSE
    )
  }
  if (any(counts > 1L)) {
    stop(
      "Ambiguous locality names (more than one HSCP): ",
      paste(localities[counts > 1L], collapse = ", "),
      call. = FALSE
    )
  }

  matches <- matches[match(localities, matches$hscp_locality), , drop = FALSE]
  filenames <- vapply(localities, profile_output_filename, character(1))
  if (anyDuplicated(filenames)) {
    stop("Selected localities produce duplicate output filenames.", call. = FALSE)
  }

  data.frame(
    locality = localities,
    hscp = matches$hscp2019name,
    output_filename = filenames,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# Process all requested localities, recording failures without ending the job.
# progress(event) receives a named list with locality, hscp, position, total,
# status (running/succeeded/failed), output_path and error.
run_profile_items <- function(items, processor, progress = function(event) NULL) {
  stopifnot(is.data.frame(items), is.function(processor), is.function(progress))
  stopifnot(all(c("locality", "hscp", "output_filename") %in% names(items)))

  results <- data.frame(
    locality = items$locality,
    hscp = items$hscp,
    status = rep("pending", nrow(items)),
    output_path = rep(NA_character_, nrow(items)),
    error = rep(NA_character_, nrow(items)),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(items))) {
    event <- list(
      locality = items$locality[[i]],
      hscp = items$hscp[[i]],
      position = i,
      total = nrow(items),
      status = "running",
      output_path = NA_character_,
      error = NA_character_
    )
    progress(event)

    outcome <- tryCatch(
      {
        output_path <- processor(items[i, , drop = FALSE])
        if (!is.character(output_path) || length(output_path) != 1L ||
            is.na(output_path) || !nzchar(output_path)) {
          stop("Profile processor did not return an output path.")
        }
        list(status = "succeeded", output_path = output_path, error = NA_character_)
      },
      error = function(e) {
        list(
          status = "failed",
          output_path = NA_character_,
          error = conditionMessage(e)
        )
      }
    )

    results$status[[i]] <- outcome$status
    results$output_path[[i]] <- outcome$output_path
    results$error[[i]] <- outcome$error
    event$status <- outcome$status
    event$output_path <- outcome$output_path
    event$error <- outcome$error
    progress(event)

    # Release large per-locality objects before building the next profile.
    invisible(gc(verbose = FALSE))
  }

  list(
    total = nrow(results),
    succeeded = sum(results$status == "succeeded"),
    failed = sum(results$status == "failed"),
    results = results
  )
}

# Stage in the destination directory so a rename to the final path stays on
# the same filesystem. On the Linux server, file.rename() replaces an existing
# file atomically; if renaming fails, the old file is left untouched.
publish_profile_document <- function(source_path, destination_path,
                                     group = "LIST_analytics") {
  source_info <- file.info(source_path)
  if (!file.exists(source_path) || is.na(source_info$size) ||
      source_info$size == 0L) {
    stop("Completed Word document is missing or empty.", call. = FALSE)
  }

  destination_dir <- dirname(destination_path)
  if (!dir.exists(destination_dir) &&
      !dir.create(destination_dir, recursive = TRUE, showWarnings = FALSE)) {
    stop("Cannot create output directory: ", destination_dir, call. = FALSE)
  }

  staged_path <- tempfile(
    pattern = ".lp-profile-stage-",
    tmpdir = destination_dir,
    fileext = ".docx"
  )
  on.exit(unlink(staged_path), add = TRUE)

  if (!file.copy(source_path, staged_path, overwrite = FALSE)) {
    stop("Could not stage Word document in output directory.", call. = FALSE)
  }

  # Apply the ownership before publication, while the old file is untouched.
  if (!is.null(group)) {
    fs::file_chown(staged_path, group_id = group)
  }

  if (!file.rename(staged_path, destination_path)) {
    stop("Could not publish Word document; existing output was preserved.",
         call. = FALSE)
  }

  invisible(destination_path)
}

build_profiles <- function(localities, output_dir,
                           progress = function(event) NULL,
                           project_root = ".") {
  if (!is.character(output_dir) || length(output_dir) != 1L ||
      is.na(output_dir) || !nzchar(output_dir)) {
    stop("output_dir must be a single directory path.", call. = FALSE)
  }
  if (!is.function(progress)) {
    stop("progress must be a function accepting an event list.", call. = FALSE)
  }

  # Relative source paths and bookdown inputs are resolved at the project root.
  project_root <- normalizePath(project_root, mustWork = TRUE)
  old_wd <- getwd()
  setwd(project_root)
  on.exit(setwd(old_wd), add = TRUE)

  rlang::check_installed("phstemplates", version = "1.3.0")
  library(knitr)
  library(bookdown)
  library(phstemplates)

  # A parent environment holds functions and memoised readers across the job.
  # Each locality gets its own child environment to prevent stale indicators.
  shared_env <- new.env(parent = globalenv())
  sys.source(
    "Master RMarkdown Document & Render Code/Global Script.R",
    envir = shared_env
  )

  lookup <- shared_env$read_in_localities()
  items <- validate_profile_selection(localities, lookup)
  output_dir <- normalizePath(output_dir, mustWork = FALSE)

  # Retain variables expected by the existing analysis and RMarkdown scripts.
  shared_env$lookup <- lookup
  shared_env$output_dir <- output_dir
  shared_env$hscp_list <- unique(items$hscp)

  job_root <- tempfile(pattern = "lp-profile-build-")
  dir.create(job_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(job_root, recursive = TRUE, force = TRUE), add = TRUE)

  input_dir <- file.path(job_root, "inputs")
  doc_dir <- file.path(job_root, "docs")
  dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(doc_dir, recursive = TRUE, showWarnings = FALSE)

  local_lp_bookdown <- file.path(input_dir, "lp_bookdown")
  fs::dir_copy("lp_bookdown", local_lp_bookdown)

  cover_page <- file.path(
    shared_env$lp_path, "templates", "phs-mngtinfo-coverV2.docx"
  )
  local_cover_page <- file.path(input_dir, "phs-mngtinfo-coverV2.docx")
  if (!file.copy(cover_page, local_cover_page, overwrite = TRUE)) {
    stop("Could not copy the Word cover-page template.", call. = FALSE)
  }

  old_cfg <- httr::set_config(httr::config(connecttimeout = 15, timeout = 90))
  on.exit(httr::set_config(old_cfg, override = TRUE), add = TRUE)

  source_scripts <- c(
    "Demographics/1. Demographics - Population.R",
    "Demographics/2. Demographics - SIMD.R",
    "Households/Households Code.R",
    "Services/2. Services data manipulation & table.R",
    "Services/3. Service HSCP map.R",
    "Population Health/3. Population Health Outputs.R",
    "Lifestyle & Risk Factors/2. Lifestyle & Risk Factors Outputs.R",
    "Unscheduled Care/2. Unscheduled Care outputs.R",
    "Master RMarkdown Document & Render Code/Tables for Appendix.R"
  )

  process_locality <- function(item) {
    # A new environment replaces the old rm(list = setdiff(...)) cleanup.
    analysis_env <- new.env(parent = shared_env)
    analysis_env$HSCP <- item$hscp[[1]]
    analysis_env$LOCALITY <- item$locality[[1]]

    # One existing section script sources another with source()'s default
    # local = FALSE. Keep nested sources inside this locality's environment
    # rather than allowing them to write into .GlobalEnv.
    analysis_env$source <- function(file, local = FALSE, ...) {
      base::source(file, local = analysis_env, ...)
    }

    # Remove previous output artefacts without removing memoised source data.
    unlink(doc_dir, recursive = TRUE)
    dir.create(doc_dir, recursive = TRUE, showWarnings = FALSE)

    for (script in source_scripts) {
      sys.source(script, envir = analysis_env)
    }

    output_name <- item$output_filename[[1]]
    main_title <- sub("\\.docx$", "", output_name)
    temp_document <- file.path(doc_dir, output_name)
    final_document <- file.path(output_dir, output_name)

    bookdown::render_book(
      input = local_lp_bookdown,
      output_dir = doc_dir,
      output_file = output_name,
      new_session = FALSE,
      output_format = "bookdown::word_document2",
      config_file = "_bookdown.yaml",
      envir = analysis_env
    )

    shared_env$orient(temp_document)
    shared_env$add_cover_page(temp_document, local_cover_page, main_title)
    apply_sensitivity_label(temp_document, "OFFICIAL_SENSITIVE_VMO")

    publish_profile_document(temp_document, final_document)
  }

  run_profile_items(items, process_locality, progress)
}

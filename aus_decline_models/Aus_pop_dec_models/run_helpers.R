# Shared runner helpers for the AusDecline project.
find_ausdecline_root <- function() {
  candidates <- character(0)

  # Path when called with Rscript --file=...
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(script_arg)) {
    candidates <- c(candidates, dirname(sub("^--file=", "", script_arg[1])))
  }

  # Path when the current file is sourced.
  source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(source_file) && nzchar(source_file)) {
    candidates <- c(candidates, dirname(source_file))
  }

  # Active RStudio document, when available.
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    active_path <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(active_path)) candidates <- c(candidates, dirname(active_path))
  }

  candidates <- c(candidates, getwd())
  candidates <- unique(candidates[nzchar(candidates)])

  for (candidate in candidates) {
    here <- tryCatch(normalizePath(candidate, winslash = "/", mustWork = TRUE), error = function(e) NULL)
    if (is.null(here)) next

    repeat {
      if (file.exists(file.path(here, "config.yml")) &&
          dir.exists(file.path(here, "reports")) &&
          dir.exists(file.path(here, "R"))) {
        return(here)
      }
      parent <- dirname(here)
      if (identical(parent, here)) break
      here <- parent
    }
  }

  stop(
    "Could not locate the AusDecline project root. Extract the complete ZIP and run the script from inside that folder."
  )
}

render_ausdecline_report <- function(report, output_file, output_subdir, params = NULL,
                                     config = "config.yml", run_label = "run") {
  project_root <- find_ausdecline_root()
  report_path <- file.path(project_root, "reports", report)
  if (!file.exists(report_path)) stop("Report not found: ", report_path)

  output_dir <- file.path(project_root, "output", output_subdir)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  Sys.setenv(
    AUSDECLINE_CONFIG = file.path(project_root, config),
    AUSDECLINE_RUN_LABEL = run_label,
    AUSDECLINE_PROJECT_ROOT = project_root
  )

  rmarkdown::render(
    input = report_path,
    output_file = output_file,
    output_dir = output_dir,
    knit_root_dir = project_root,
    params = params,
    envir = new.env(parent = globalenv())
  )
}

# One-command integrated master report.
locate_runner_helper <- function() {
  candidates <- c(
    tryCatch(file.path(dirname(sys.frame(1)$ofile), "run_helpers.R"), error = function(e) NA_character_),
    file.path(getwd(), "run_helpers.R")
  )
  candidates <- candidates[!is.na(candidates) & file.exists(candidates)]
  if (!length(candidates)) stop("Cannot find run_helpers.R. Run this script from the extracted project folder.")
  normalizePath(candidates[1], winslash = "/", mustWork = TRUE)
}
source(locate_runner_helper())
render_ausdecline_report(
  report = "00_master_report.Rmd",
  output_file = "AusDecline_master_report.html",
  output_subdir = "master_report",
  config = "config.yml",
  run_label = "master_report"
)

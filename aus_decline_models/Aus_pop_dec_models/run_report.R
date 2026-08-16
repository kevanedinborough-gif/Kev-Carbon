# Render the main comparison report from any working directory.
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
  report = "00_all_models_comparison.Rmd",
  output_file = "all_models_comparison.html",
  output_subdir = "all_models",
  config = "config.yml",
  run_label = "all_models"
)

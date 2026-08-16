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
  report = "13_logistic_transition_model.Rmd",
  output_file = "logistic_transition_report.html",
  output_subdir = "logistic_transition",
  params = list(model_key = "logistic", report_title = "Logistic-transition model: detailed methods and results"),
  config = "configs/logistic.yml",
  run_label = "logistic_transition"
)

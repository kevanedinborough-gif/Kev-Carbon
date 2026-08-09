# Render the integrated master report and all three standalone model reports.
locate_root <- function() {
  sf <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(sf) && nzchar(sf)) return(dirname(normalizePath(sf, winslash = "/", mustWork = TRUE)))
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
runner_root <- locate_root()
for (runner in c("run_master_report.R", "run_single_slope.R", "run_change_point.R", "run_logistic_transition.R")) {
  source(file.path(runner_root, runner), chdir = TRUE)
}

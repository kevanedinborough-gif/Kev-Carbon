run_window_sensitivity <- function(starts = c(1000, 2000, 3000, 4000), end_bp_value = 0) {
  stop("Window sensitivity is intentionally isolated. Copy config.yml, set each analysis_start_calbp, knit reports/02_two_model_comparison.Rmd, and combine output/model_comparison_WAIC.csv and output/posterior_parameter_summary.csv. This prevents accidental multi-hour runs.")
}

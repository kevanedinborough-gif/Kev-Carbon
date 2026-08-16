# Run from the project folder.
required <- c("rmarkdown", "nimble", "nimbleCarbon", "rcarbon", "readxl",
              "dplyr", "ggplot2", "tibble", "tidyr", "coda", "knitr")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing)
}

rmarkdown::render(
  "AusData_simple_NIMBLE_slope_models.Rmd",
  params = list(run_mode = "test"),
  envir = new.env(parent = globalenv())
)


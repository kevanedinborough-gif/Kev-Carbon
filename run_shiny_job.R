args <- commandArgs(trailingOnly = TRUE)
run_mode <- if (length(args) >= 1) tolower(args[1]) else "test"

if (!run_mode %in% c("test", "medium", "long")) {
  stop("run_mode must be test, medium, or long")
}

required <- c("rmarkdown", "nimble", "nimbleCarbon", "rcarbon", "readxl",
              "dplyr", "ggplot2", "tibble", "tidyr", "coda", "knitr")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Missing packages: ", paste(missing, collapse = ", "))
}

dir.create("run_status", showWarnings = FALSE, recursive = TRUE)
writeLines(
  c(
    paste0("status=RUNNING"),
    paste0("mode=", run_mode),
    paste0("started=", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ),
  "run_status/latest.txt"
)

ok <- FALSE
err <- NULL

tryCatch({
  rmarkdown::render(
    "AusData_simple_NIMBLE_slope_models.Rmd",
    params = list(run_mode = run_mode),
    envir = new.env(parent = globalenv()),
    output_file = "AusData_simple_NIMBLE_slope_models.html"
  )
  ok <- TRUE
}, error = function(e) {
  err <<- conditionMessage(e)
})

if (ok) {
  writeLines(
    c(
      "status=FINISHED",
      paste0("mode=", run_mode),
      paste0("finished=", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      "report=AusData_simple_NIMBLE_slope_models.html"
    ),
    "run_status/latest.txt"
  )
} else {
  writeLines(
    c(
      "status=ERROR",
      paste0("mode=", run_mode),
      paste0("finished=", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      paste0("error=", err)
    ),
    "run_status/latest.txt"
  )
  stop(err)
}

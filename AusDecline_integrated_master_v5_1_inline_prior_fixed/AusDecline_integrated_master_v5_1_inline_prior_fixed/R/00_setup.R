WORKFLOW_VERSION <- "5.1-inline-prior-fixed"
message("Loading AusDecline workflow ", WORKFLOW_VERSION)

# Global setup and configuration -------------------------------------------
required <- c(
  "yaml", "readxl", "dplyr", "ggplot2", "tibble", "tidyr",
  "rcarbon", "nimble", "nimbleCarbon", "coda", "knitr"
)
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install missing packages: ", paste(missing, collapse = ", "))

suppressPackageStartupMessages({
  library(yaml); library(readxl); library(dplyr); library(ggplot2)
  library(tibble); library(tidyr); library(rcarbon); library(nimble)
  library(nimbleCarbon); library(coda); library(knitr)
})

if (!exists("PROJECT_ROOT", inherits = TRUE)) {
  PROJECT_ROOT <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
config_filename <- Sys.getenv("AUSDECLINE_CONFIG", unset = "config.yml")
config_path <- if (grepl("^([A-Za-z]:|/)", config_filename)) config_filename else file.path(PROJECT_ROOT, config_filename)
if (!file.exists(config_path)) stop("Configuration file not found: ", config_path)
config <- yaml::read_yaml(config_path)
run_label <- Sys.getenv("AUSDECLINE_RUN_LABEL", unset = "all_models")
OUTPUT_DIR <- file.path(PROJECT_ROOT, "output", run_label)
CHECKPOINT_DIR <- file.path(PROJECT_ROOT, "checkpoints")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(CHECKPOINT_DIR, showWarnings = FALSE, recursive = TRUE)

run_presets <- list(
  test   = list(niter = 3000L,  nburnin = 1000L,  thin = 4L,  nchains = 2L),
  medium = list(niter = 20000L, nburnin = 7000L,  thin = 10L, nchains = 3L),
  long   = list(niter = 80000L, nburnin = 30000L, thin = 20L, nchains = 4L)
)
run_mode <- tolower(config$run_mode)
if (!run_mode %in% names(run_presets)) stop("run_mode must be test, medium or long")
mcmc_cfg <- run_presets[[run_mode]]

resolve_integer <- function(value, default, label, allow_zero = FALSE) {
  if (is.null(value) || length(value) == 0 || is.na(value)) return(as.integer(default))
  out <- suppressWarnings(as.integer(value)); minimum <- if (allow_zero) 0L else 1L
  if (is.na(out) || out < minimum || out != as.numeric(value)) stop(label, " must be a whole number")
  out
}
mcmc_cfg$nchains <- resolve_integer(config$mcmc$chains, mcmc_cfg$nchains, "mcmc chains")
mcmc_cfg$niter <- resolve_integer(config$mcmc$iterations, mcmc_cfg$niter, "mcmc iterations")
mcmc_cfg$nburnin <- resolve_integer(config$mcmc$burnin, mcmc_cfg$nburnin, "mcmc burnin", TRUE)
mcmc_cfg$thin <- resolve_integer(config$mcmc$thin, mcmc_cfg$thin, "mcmc thin")
mcmc_cfg$seed <- resolve_integer(config$mcmc$seed, 73021L, "mcmc seed")
if (mcmc_cfg$nburnin >= mcmc_cfg$niter) stop("burnin must be smaller than iterations")

start_bp <- as.numeric(config$analysis_start_calbp)
end_bp <- as.numeric(config$analysis_end_calbp)
if (!is.finite(start_bp) || !is.finite(end_bp) || start_bp <= end_bp) stop("analysis start must be older/larger than end")
calendar_display <- tolower(config$calendar_display)
if (!calendar_display %in% c("calad", "calbp")) stop("calendar_display must be calAD or calBP")
bp_to_ad <- function(x) 1950 - x
calendar_values <- function(x) if (calendar_display == "calad") bp_to_ad(x) else x
calendar_axis_label <- if (calendar_display == "calad") "Calendar year AD" else "Calendar years BP"
calendar_scale <- if (calendar_display == "calbp") {
  ggplot2::scale_x_reverse(limits = c(start_bp, end_bp))
} else ggplot2::scale_x_continuous(limits = sort(c(bp_to_ad(start_bp), bp_to_ad(end_bp))))

available_cores <- parallel::detectCores(logical = TRUE)
if (is.na(available_cores) || available_cores < 1) available_cores <- 1L
requested_cores <- resolve_integer(config$parallel_cores, 1L, "parallel_cores")
model_switches <- c(
  single = isTRUE(config$models$single_slope),
  change = isTRUE(config$models$change_point),
  logistic = isTRUE(config$models$logistic_transition)
)
enabled_model_names <- names(model_switches)[model_switches]
active_models <- length(enabled_model_names)
if (active_models < 1L) stop("Enable at least one model in the active configuration.")
parallel_cores <- min(requested_cores, available_cores, max(1L, active_models))
parallel_enabled <- parallel_cores > 1L
set.seed(mcmc_cfg$seed)

# Build the configuration table from a named list. Converting each value to
# character avoids vctrs type conflicts when text, numeric and logical settings
# are shown together in one report column.
configuration_values <- list(
  "Run mode" = run_mode,
  "Analysis start cal BP" = start_bp,
  "Analysis end cal BP" = end_bp,
  "Displayed start" = calendar_values(start_bp),
  "Displayed end" = calendar_values(end_bp),
  "Calendar display" = config$calendar_display,
  "Calibration curve" = config$calibration_curve,
  "Site-bin width" = config$site_bin_width_years,
  "Iterations" = mcmc_cfg$niter,
  "Burn-in" = mcmc_cfg$nburnin,
  "Thin" = mcmc_cfg$thin,
  "Chains" = mcmc_cfg$nchains,
  "Retained samples per chain" = (mcmc_cfg$niter - mcmc_cfg$nburnin) %/% mcmc_cfg$thin,
  "Seed" = mcmc_cfg$seed,
  "Processors detected" = available_cores,
  "Processors requested" = requested_cores,
  "Processors used" = parallel_cores,
  "Parallel fitting" = parallel_enabled,
  "Configuration file" = basename(config_path),
  "Run label" = run_label,
  "Enabled models" = paste(enabled_model_names, collapse = ", ")
)

format_configuration_value <- function(x) {
  if (is.null(x) || length(x) == 0L) return("<not set>")
  if (length(x) > 1L) return(paste(as.character(x), collapse = ", "))
  as.character(x)
}

configuration <- tibble::tibble(
  setting = names(configuration_values),
  value = vapply(configuration_values, format_configuration_value, character(1))
)

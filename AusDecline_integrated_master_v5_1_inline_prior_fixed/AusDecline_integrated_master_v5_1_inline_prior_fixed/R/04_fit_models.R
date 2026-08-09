extract_numeric_waic <- function(x, depth = 0L) {
  if (is.null(x) || depth > 8L) return(NA_real_)
  if (is.numeric(x)) {
    out <- suppressWarnings(as.numeric(x)); out <- out[is.finite(out)]
    return(if (length(out)) out[1] else NA_real_)
  }
  candidate_names <- c("WAIC", "waic", "value", "totalWAIC", "WAICvalue",
                       "waicValue", "WAIC_total", "total")
  if (is.list(x)) {
    for (nm in intersect(candidate_names, names(x))) {
      out <- extract_numeric_waic(x[[nm]], depth + 1L); if (is.finite(out)) return(out)
    }
    for (element in x) {
      out <- extract_numeric_waic(element, depth + 1L); if (is.finite(out)) return(out)
    }
  }
  if (isS4(x)) {
    slots <- methods::slotNames(x)
    for (nm in intersect(candidate_names, slots)) {
      out <- extract_numeric_waic(methods::slot(x, nm), depth + 1L); if (is.finite(out)) return(out)
    }
    for (nm in slots) {
      value <- tryCatch(methods::slot(x, nm), error = function(e) NULL)
      out <- extract_numeric_waic(value, depth + 1L); if (is.finite(out)) return(out)
    }
  }
  if (is.environment(x)) {
    object_names <- tryCatch(ls(x, all.names = TRUE), error = function(e) character())
    for (nm in intersect(candidate_names, object_names)) {
      value <- tryCatch(get(nm, envir = x, inherits = FALSE), error = function(e) NULL)
      out <- extract_numeric_waic(value, depth + 1L); if (is.finite(out)) return(out)
    }
  }
  out <- tryCatch(suppressWarnings(as.numeric(x)), error = function(e) numeric(0))
  out <- out[is.finite(out)]
  if (length(out)) out[1] else NA_real_
}

extract_fit <- function(x) {
  if (is.list(x) && !is.null(x$samples)) {
    samples <- x$samples
    waic_obj <- if (!is.null(x$WAIC)) x$WAIC else x$waic
  } else {
    samples <- x
    waic_obj <- attr(x, "WAIC", exact = TRUE)
    if (is.null(waic_obj)) waic_obj <- attr(x, "waic", exact = TRUE)
  }
  list(samples = samples, WAIC = extract_numeric_waic(waic_obj), WAIC_raw = waic_obj)
}

fit_one_model <- function(model_name, theta, start_bp, end_bp, mcmc_cfg) {
  suppressPackageStartupMessages({
    library(nimble); library(nimbleCarbon); library(coda)
  })
  N <- length(theta)
  constants_common <- list(N = N, startBP = start_bp, endBP = end_bp)
  data_common <- list(theta = theta)
  change_low <- end_bp + max(50, 0.05 * (start_bp - end_bp))
  change_high <- start_bp - max(50, 0.05 * (start_bp - end_bp))

  if (identical(model_name, "single")) {
    code <- nimble::nimbleCode({
      r ~ dnorm(0, sd = 0.02)
      for (i in 1:N) theta[i] ~ dExponentialGrowth(a = startBP, b = endBP, r = r)
    })
    inits <- lapply(seq_len(mcmc_cfg$nchains), function(ch) {
      set.seed(mcmc_cfg$seed + 100L + ch); list(r = rnorm(1, 0, 0.002))
    })
    monitors <- "r"; constants <- constants_common; seed_offset <- 1000L
  } else if (identical(model_name, "change")) {
    code <- nimble::nimbleCode({
      r_early ~ dnorm(0, sd = 0.02)
      r_late ~ dnorm(0, sd = 0.02)
      mu ~ dunif(changeLow, changeHigh)
      for (i in 1:N) theta[i] ~ dDoubleExponentialGrowth(
        a = startBP, b = endBP, r1 = r_early, r2 = r_late, mu = mu)
    })
    inits <- lapply(seq_len(mcmc_cfg$nchains), function(ch) {
      set.seed(mcmc_cfg$seed + 200L + ch)
      list(r_early = rnorm(1, 0, 0.002), r_late = rnorm(1, 0, 0.002),
           mu = runif(1, change_low, change_high))
    })
    monitors <- c("r_early", "r_late", "mu")
    constants <- c(constants_common, list(changeLow = change_low, changeHigh = change_high))
    seed_offset <- 2000L
  } else if (identical(model_name, "logistic")) {
    code <- nimble::nimbleCode({
      r_logistic ~ dnorm(0, sd = 0.02)
      m ~ dunif(changeLow, changeHigh)
      for (i in 1:N) theta[i] ~ dLogisticGrowth2(
        a = startBP, b = endBP, m = m, r = r_logistic)
    })
    inits <- lapply(seq_len(mcmc_cfg$nchains), function(ch) {
      set.seed(mcmc_cfg$seed + 300L + ch)
      list(r_logistic = rnorm(1, 0, 0.004), m = runif(1, change_low, change_high))
    })
    monitors <- c("r_logistic", "m")
    constants <- c(constants_common, list(changeLow = change_low, changeHigh = change_high))
    seed_offset <- 3000L
  } else stop("Unknown model name: ", model_name)

  nimble::nimbleMCMC(
    code = code, constants = constants, data = data_common,
    inits = inits, monitors = monitors,
    niter = mcmc_cfg$niter, nburnin = mcmc_cfg$nburnin,
    thin = mcmc_cfg$thin, nchains = mcmc_cfg$nchains,
    setSeed = mcmc_cfg$seed + seed_offset + seq_len(mcmc_cfg$nchains),
    samples = TRUE, samplesAsCodaMCMC = TRUE, summary = FALSE,
    WAIC = TRUE, progressBar = FALSE
  )
}

model_names <- enabled_model_names
if (parallel_enabled) {
  cat("Fitting", length(model_names), "models concurrently on", parallel_cores, "processors.\n")
  cl <- parallel::makeCluster(parallel_cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterSetRNGStream(cl, iseed = mcmc_cfg$seed + 4000L)
  fits_raw <- parallel::parLapply(
    cl, model_names, fit_one_model,
    theta = theta, start_bp = start_bp, end_bp = end_bp, mcmc_cfg = mcmc_cfg
  )
  parallel::stopCluster(cl); on.exit(NULL, add = FALSE)
} else {
  cat("Fitting", length(model_names), "models sequentially.\n")
  fits_raw <- lapply(model_names, fit_one_model,
                     theta = theta, start_bp = start_bp, end_bp = end_bp, mcmc_cfg = mcmc_cfg)
}
names(fits_raw) <- model_names
fits <- lapply(fits_raw, extract_fit)

model_labels <- c(
  single = "Single slope",
  change = "Change point: two slopes",
  logistic = "Smooth logistic transition"
)

for (nm in names(fits)) cat(model_labels[[nm]], "WAIC:", fits[[nm]]$WAIC, "\n")
waic_available <- length(fits) >= 2L && all(vapply(fits, function(x) is.finite(x$WAIC), logical(1)))
if (length(fits) >= 2L && !waic_available) warning(
  "WAIC could not be extracted for every fitted model. Ranking and winner-only graphics will be skipped."
)

# Compatibility objects retained for downstream code and user inspection.
fit_single <- fits$single
fit_change <- fits$change
fit_logistic <- fits$logistic
if (!is.null(fit_single)) saveRDS(fit_single, file.path(OUTPUT_DIR, "fit_single_slope.rds"))
if (!is.null(fit_change)) saveRDS(fit_change, file.path(OUTPUT_DIR, "fit_change_point.rds"))
if (!is.null(fit_logistic)) saveRDS(fit_logistic, file.path(OUTPUT_DIR, "fit_logistic_transition.rds"))
saveRDS(fits, file.path(OUTPUT_DIR, "all_model_fits.rds"))

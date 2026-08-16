as_mcmc_list <- function(x) {
  if (inherits(x, "mcmc.list")) return(x)
  if (inherits(x, "mcmc")) return(coda::mcmc.list(x))
  if (is.list(x) && length(x) && all(vapply(x, inherits, logical(1), "mcmc"))) return(coda::mcmc.list(x))
  stop("Posterior samples are not a coda mcmc or mcmc.list object.")
}

samples_by_model <- lapply(fits, function(x) as_mcmc_list(x$samples))
matrices_by_model <- lapply(samples_by_model, as.matrix)

summarise_parameter <- function(samples, model_name) {
  mat <- as.matrix(samples)
  qs <- apply(mat, 2, quantile, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
  tibble(model = model_name, parameter = colnames(mat), mean = colMeans(mat),
         median = qs[2, ], lower_95 = qs[1, ], upper_95 = qs[3, ],
         effective_sample_size = as.numeric(coda::effectiveSize(samples)))
}
posterior_summary <- bind_rows(lapply(names(samples_by_model), function(nm) {
  summarise_parameter(samples_by_model[[nm]], model_labels[[nm]])
}))
write.csv(posterior_summary, file.path(OUTPUT_DIR, "posterior_parameter_summary.csv"), row.names = FALSE)

rhat_table <- function(samples, model_name) {
  mat <- as.matrix(samples)
  if (length(samples) < 2) return(tibble(model = model_name, parameter = colnames(mat), Rhat = NA_real_))
  gd <- tryCatch(coda::gelman.diag(samples, autoburnin = FALSE, multivariate = FALSE)$psrf[, 1],
                 error = function(e) rep(NA_real_, ncol(mat)))
  tibble(model = model_name, parameter = colnames(mat), Rhat = as.numeric(gd))
}
convergence <- bind_rows(lapply(names(samples_by_model), function(nm) {
  rhat_table(samples_by_model[[nm]], model_labels[[nm]])
}))
write.csv(convergence, file.path(OUTPUT_DIR, "convergence_Rhat.csv"), row.names = FALSE)

# Compatibility matrices.
single_samples <- samples_by_model$single; change_samples <- samples_by_model$change
logistic_samples <- samples_by_model$logistic
single_mat <- matrices_by_model$single; change_mat <- matrices_by_model$change
logistic_mat <- matrices_by_model$logistic

questions <- list()
if (!is.null(single_mat)) questions[["P(single rate < 0)"]] <- mean(single_mat[, "r"] < 0)
if (!is.null(change_mat)) {
  questions[["P(early rate < 0)"]] <- mean(change_mat[, "r_early"] < 0)
  questions[["P(late rate < 0)"]] <- mean(change_mat[, "r_late"] < 0)
  questions[["P(late rate < early rate)"]] <- mean(change_mat[, "r_late"] < change_mat[, "r_early"])
  questions[["Median change point cal BP"]] <- median(change_mat[, "mu"])
  questions[["Median change point cal AD"]] <- bp_to_ad(median(change_mat[, "mu"]))
}
if (!is.null(logistic_mat)) {
  questions[["P(logistic rate < 0)"]] <- mean(logistic_mat[, "r_logistic"] < 0)
  questions[["Median logistic inflection cal BP"]] <- median(logistic_mat[, "m"])
  questions[["Median logistic inflection cal AD"]] <- bp_to_ad(median(logistic_mat[, "m"]))
}
slope_questions <- tibble(quantity = names(questions), value = as.numeric(unlist(questions)))
write.csv(slope_questions, file.path(OUTPUT_DIR, "slope_probabilities.csv"), row.names = FALSE)

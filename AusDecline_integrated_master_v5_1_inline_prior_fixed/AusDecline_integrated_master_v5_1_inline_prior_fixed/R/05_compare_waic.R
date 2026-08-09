comparison <- tibble(
  model_key = names(fits),
  model = unname(model_labels[names(fits)]),
  WAIC = vapply(fits, function(x) x$WAIC, numeric(1))
)

if (waic_available) {
  comparison <- comparison |>
    arrange(WAIC) |>
    mutate(
      delta_WAIC = WAIC - min(WAIC),
      relative_likelihood = exp(-0.5 * delta_WAIC),
      WAIC_weight = relative_likelihood / sum(relative_likelihood),
      rank = row_number()
    )
} else {
  comparison <- comparison |>
    mutate(delta_WAIC = NA_real_, relative_likelihood = NA_real_,
           WAIC_weight = NA_real_, rank = NA_integer_)
}
write.csv(comparison, file.path(OUTPUT_DIR, "model_comparison_WAIC.csv"), row.names = FALSE)
if (length(fits) == 1L) {
  cat("\n**This is a single-model run. WAIC is reported descriptively, but model ranking requires two or more models.**\n")
} else if (!waic_available) {
  cat("\n**WAIC comparison is unavailable in this run. No winning model is declared.**\n")
}

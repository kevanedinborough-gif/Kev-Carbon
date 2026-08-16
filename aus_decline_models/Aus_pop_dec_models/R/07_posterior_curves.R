grid_bp <- seq(start_bp, end_bp, by = -5)
years_toward_present <- start_bp - grid_bp
thin_draws <- function(mat, maximum = 1000) {
  if (nrow(mat) <= maximum) return(mat)
  mat[round(seq(1, nrow(mat), length.out = maximum)), , drop = FALSE]
}
curve_summary <- function(curves, model_name, model_key) {
  tibble(calBP = grid_bp, years_toward_present = years_toward_present,
         model = model_name, model_key = model_key,
         median = apply(curves, 2, median, na.rm = TRUE),
         lower = apply(curves, 2, quantile, 0.025, na.rm = TRUE),
         upper = apply(curves, 2, quantile, 0.975, na.rm = TRUE))
}
curve_parts <- list()
if (!is.null(single_mat)) {
  draws <- thin_draws(single_mat)
  curves <- t(apply(draws, 1, function(z) nimbleCarbon::dExponentialGrowth(
    x = grid_bp, a = start_bp, b = end_bp, r = z["r"], log = FALSE)))
  curve_parts$single <- curve_summary(curves, model_labels[["single"]], "single")
}
if (!is.null(change_mat)) {
  draws <- thin_draws(change_mat)
  curves <- t(apply(draws, 1, function(z) nimbleCarbon::dDoubleExponentialGrowth(
    x = grid_bp, a = start_bp, b = end_bp, r1 = z["r_early"],
    r2 = z["r_late"], mu = z["mu"], log = FALSE)))
  curve_parts$change <- curve_summary(curves, model_labels[["change"]], "change")
}
if (!is.null(logistic_mat)) {
  draws <- thin_draws(logistic_mat)
  curves <- t(apply(draws, 1, function(z) nimbleCarbon::dLogisticGrowth2(
    x = grid_bp, a = start_bp, b = end_bp, m = z["m"],
    r = z["r_logistic"], log = FALSE)))
  curve_parts$logistic <- curve_summary(curves, model_labels[["logistic"]], "logistic")
}
curve_data <- bind_rows(curve_parts) |> mutate(calendar_display_value = calendar_values(calBP))
write.csv(curve_data, file.path(OUTPUT_DIR, "posterior_model_curves.csv"), row.names = FALSE)

p_curve <- ggplot(curve_data, aes(calendar_display_value, median, linetype = model)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, group = model), alpha = 0.13) +
  geom_line(linewidth = 0.95) + calendar_scale +
  labs(title = "Posterior demographic decline shapes",
       subtitle = "Single rate, abrupt change point, and smooth logistic transition",
       x = calendar_axis_label, y = "Relative date-density", linetype = "Model") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
if (isTRUE(config$output$save_png)) ggsave(file.path(OUTPUT_DIR, "posterior_slope_shapes.png"), p_curve,
                                            width = 9, height = 5.5, dpi = config$output$dpi)

curve_slopes <- curve_data |> group_by(model, model_key) |>
  arrange(years_toward_present, .by_group = TRUE) |>
  mutate(slope_toward_present = c(NA_real_, diff(median) / diff(years_toward_present))) |> ungroup()
p_slope <- ggplot(curve_slopes, aes(calendar_display_value, slope_toward_present, linetype = model)) +
  geom_hline(yintercept = 0, linewidth = 0.4) + geom_line(linewidth = 0.9, na.rm = TRUE) +
  calendar_scale + labs(title = "Estimated slope toward the present", x = calendar_axis_label,
                        y = "Change in relative density per year", linetype = "Model") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
if (isTRUE(config$output$save_png)) ggsave(file.path(OUTPUT_DIR, "slope_toward_present.png"), p_slope,
                                            width = 9, height = 5.5, dpi = config$output$dpi)
write.csv(curve_slopes, file.path(OUTPUT_DIR, "numerical_slopes.csv"), row.names = FALSE)

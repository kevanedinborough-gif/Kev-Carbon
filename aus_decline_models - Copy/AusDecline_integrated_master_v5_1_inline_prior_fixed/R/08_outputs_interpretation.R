if (!waic_available) {
  cat("**WAIC model comparison is unavailable.** The posterior models fitted, but no best model is declared.")
} else {
  best_model <- comparison$model[1]
  best_key <- comparison$model_key[1]
  best_weight <- comparison$WAIC_weight[1]
  best_delta <- if (nrow(comparison) > 1) comparison$delta_WAIC[2] else NA_real_

  interpretation <- switch(best_key,
    single = paste0(
      "One proportional rate is sufficient across the selected interval. ",
      "The data do not require either an abrupt breakpoint or a gradual logistic bend."),
    change = paste0(
      "The trajectory is better represented by different early and late rates separated by an estimated change point. ",
      "This is the most abrupt of the three candidate histories."),
    logistic = paste0(
      "The trajectory is better represented by a smooth transition centred on an estimated inflection date. ",
      "This suggests progressive decline and stabilisation rather than an instantaneous break."),
    ""
  )

  cat("**Best-supported model by WAIC:** ", best_model, ".  ", sep = "")
  cat("**WAIC separation from the runner-up:** ", round(best_delta, 2), ".  ", sep = "")
  cat(interpretation)

  verdict_blue <- "#176B87"; verdict_dark <- "#183B4E"; verdict_gold <- "#D89B2B"
  verdict_grey <- "#D9E1E5"; verdict_red <- "#A63D40"
  support_label <- dplyr::case_when(
    best_weight >= 0.90 ~ "Very strong relative support",
    best_weight >= 0.70 ~ "Strong relative support",
    best_weight >= 0.55 ~ "Moderate relative support",
    TRUE ~ "Candidate models are difficult to distinguish"
  )

  weight_plot_data <- comparison |>
    mutate(model = factor(model, levels = rev(model)), selected = as.character(model) == best_model,
           weight_percent = 100 * WAIC_weight)
  p_weights <- ggplot(weight_plot_data, aes(WAIC_weight, model, fill = selected)) +
    geom_col(width = 0.62) +
    geom_text(aes(label = sprintf("%.1f%%", weight_percent)), hjust = -0.12,
              size = 4.1, fontface = "bold") +
    scale_fill_manual(values = c(`TRUE` = verdict_blue, `FALSE` = verdict_grey), guide = "none") +
    scale_x_continuous(limits = c(0, 1.15), breaks = seq(0, 1, 0.2),
                       labels = function(x) paste0(round(100*x), "%")) +
    labs(title = "Which decline model is best supported?", subtitle = support_label,
         x = "Relative WAIC weight", y = NULL) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", colour = verdict_dark, size = 17),
          panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
          axis.text.y = element_text(face = "bold", colour = verdict_dark),
          plot.margin = margin(12, 45, 12, 12))
  print(p_weights)
  if (isTRUE(config$output$save_png)) ggsave(file.path(OUTPUT_DIR, "model_support_weights.png"), p_weights,
                                              width = 9, height = 5.2, dpi = config$output$dpi, bg = "white")

  winner_curve <- curve_data |> filter(model_key == best_key)
  winner_slope <- curve_slopes |> filter(model_key == best_key)
  event_layer <- NULL; event_line <- NULL; event_text <- NULL

  if (best_key == "change") {
    q <- quantile(change_mat[, "mu"], c(0.025, 0.5, 0.975), na.rm = TRUE)
    qd <- calendar_values(q)
    event_layer <- annotate("rect", xmin = min(qd[c(1,3)]), xmax = max(qd[c(1,3)]),
                            ymin = -Inf, ymax = Inf, fill = verdict_gold, alpha = 0.13)
    event_line <- geom_vline(xintercept = qd[2], colour = verdict_gold, linewidth = 0.9, linetype = "dashed")
    event_text <- paste0("Estimated abrupt change: ",
                         if (calendar_display == "calad") paste0("AD ", round(qd[2])) else paste0(round(qd[2]), " cal BP"))
  } else if (best_key == "logistic") {
    q <- quantile(logistic_mat[, "m"], c(0.025, 0.5, 0.975), na.rm = TRUE)
    qd <- calendar_values(q)
    event_layer <- annotate("rect", xmin = min(qd[c(1,3)]), xmax = max(qd[c(1,3)]),
                            ymin = -Inf, ymax = Inf, fill = verdict_gold, alpha = 0.13)
    event_line <- geom_vline(xintercept = qd[2], colour = verdict_gold, linewidth = 0.9, linetype = "dashed")
    event_text <- paste0("Estimated smooth-transition inflection: ",
                         if (calendar_display == "calad") paste0("AD ", round(qd[2])) else paste0(round(qd[2]), " cal BP"))
  } else event_text <- "No transition date is required by the single-rate model"

  p_winner <- ggplot(winner_curve, aes(calendar_display_value, median)) + event_layer +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = verdict_blue, alpha = 0.20) +
    geom_line(colour = verdict_blue, linewidth = 1.25) +
    geom_rug(data = binned_plot, aes(x = calendar_display_value), inherit.aes = FALSE,
             sides = "b", alpha = 0.22, colour = verdict_dark) + event_line + calendar_scale +
    labs(title = paste0("Best-supported decline shape: ", best_model),
         subtitle = paste0(event_text, " · shaded band is the 95% credible interval"),
         x = calendar_axis_label, y = "Relative demographic density",
         caption = "Rug marks show independent site-time bins.") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", colour = verdict_dark, size = 17),
          panel.grid.minor = element_blank(), axis.title = element_text(face = "bold"))
  print(p_winner)
  if (isTRUE(config$output$save_png)) ggsave(file.path(OUTPUT_DIR, "best_model_posterior_curve.png"), p_winner,
                                              width = 10, height = 5.8, dpi = config$output$dpi, bg = "white")

  steepest_row <- winner_slope |> filter(is.finite(slope_toward_present)) |>
    slice_min(slope_toward_present, n = 1, with_ties = FALSE)
  p_winner_slope <- ggplot(winner_slope, aes(calendar_display_value, slope_toward_present)) +
    geom_hline(yintercept = 0, colour = "#7C8B92", linewidth = 0.45) +
    geom_area(aes(y = ifelse(slope_toward_present < 0, slope_toward_present, 0)),
              fill = verdict_red, alpha = 0.18, na.rm = TRUE) +
    geom_line(colour = verdict_dark, linewidth = 1.05, na.rm = TRUE) +
    geom_point(data = steepest_row, colour = verdict_red, size = 3.2) + calendar_scale +
    labs(title = "Where is the fitted decline steepest?",
         subtitle = "Values below zero indicate decline toward the present",
         x = calendar_axis_label, y = "Annual change in relative density") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", colour = verdict_dark, size = 17),
          panel.grid.minor = element_blank(), axis.title = element_text(face = "bold"))
  print(p_winner_slope)
  if (isTRUE(config$output$save_png)) ggsave(file.path(OUTPUT_DIR, "best_model_decline_slope.png"), p_winner_slope,
                                              width = 10, height = 5.3, dpi = config$output$dpi, bg = "white")

  cat(sprintf(
    '<div style="border-left:8px solid %s;background:#F4F8FA;padding:18px 22px;margin:20px 0;border-radius:7px;box-shadow:0 2px 7px rgba(0,0,0,0.08);"><h3 style="margin-top:0;color:%s;">Three-model verdict</h3><p style="font-size:1.12em;"><strong>%s</strong></p><p><strong>Relative WAIC weight:</strong> %.1f%% &nbsp; | &nbsp; <strong>WAIC separation:</strong> %.2f</p><p>%s</p><p style="margin-bottom:0;color:#596970;"><em>%s</em></p></div>',
    verdict_blue, verdict_dark, best_model, 100 * best_weight, best_delta, interpretation, support_label
  ))
}

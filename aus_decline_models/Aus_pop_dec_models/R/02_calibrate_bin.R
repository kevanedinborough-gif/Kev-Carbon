cal <- rcarbon::calibrate(
  x = AUD$c14_age,
  errors = AUD$c14_error,
  ids = AUD$calibration_id,
  calCurves = config$calibration_curve,
  normalised = isTRUE(config$calibration_normalised),
  verbose = TRUE
)

cal_summary <- as.data.frame(summary(cal))
if (!"MedianBP" %in% names(cal_summary)) {
  stop("The rcarbon calibration summary did not contain MedianBP.")
}

AUD$median_calbp <- suppressWarnings(as.numeric(cal_summary$MedianBP))

# Group same-site determinations that are close in conventional radiocarbon age.
AUD$bin_id <- rcarbon::binPrep(
  sites = AUD$site_id,
  ages = AUD$c14_age,
  h = config$site_bin_width_years
)

# One representative calendar estimate per site-time bin.
binned <- AUD |>
  filter(
    is.finite(median_calbp),
    median_calbp >= end_bp,
    median_calbp <= start_bp
  ) |>
  group_by(bin_id) |>
  summarise(
    theta = median(median_calbp),
    site_id = first(site_id),
    dates_in_bin = n(),
    .groups = "drop"
  ) |>
  arrange(desc(theta))

if (nrow(binned) < 20) {
  stop("Fewer than 20 site-time bins fall inside the selected analysis window.")
}

cat("Independent site-time bins used:", nrow(binned), "\n")
cat("Original determinations represented:", sum(binned$dates_in_bin), "\n")

write.csv(binned, file.path(OUTPUT_DIR, "binned_calendar_dates.csv"), row.names = FALSE)

binned_plot <- binned |> mutate(calendar_display_value = calendar_values(theta))

 ggplot(binned_plot, aes(calendar_display_value)) +
  geom_histogram(binwidth = 25, boundary = 0) +
  calendar_scale +
  labs(
    title = "Binned calibrated dates used by both models",
    x = calendar_axis_label,
    y = "Site-time bins"
  ) +
  theme_minimal(base_size = 12)

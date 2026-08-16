input_file <- file.path(PROJECT_ROOT, config$data_file)
if (!file.exists(input_file)) {
  stop("AusData.xlsx is not in the project folder.")
}

raw <- readxl::read_excel(input_file, sheet = config$sheet)

needed <- c("LabID", "SiteID", "C14Age", "C14Error")
missing_cols <- setdiff(needed, names(raw))
if (length(missing_cols)) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

AUD <- raw |>
  transmute(
    lab_id = as.character(LabID),
    site_id = as.character(SiteID),
    c14_age = suppressWarnings(as.numeric(C14Age)),
    c14_error = suppressWarnings(as.numeric(C14Error))
  ) |>
  filter(
    !is.na(lab_id),
    !is.na(site_id),
    is.finite(c14_age),
    is.finite(c14_error),
    c14_error > 0
  ) |>
  mutate(
    # rcarbon requires every calibration ID to be unique. Laboratory IDs are
    # retained separately because replicate determinations can share a LabID.
    calibration_id = sprintf("AusDate_%05d", row_number())
  )

audit <- tibble(
  rows_in_workbook = nrow(raw),
  valid_dates = nrow(AUD),
  sites = n_distinct(AUD$site_id),
  median_error = median(AUD$c14_error)
)
knitr::kable(audit, digits = 1)

# ============================================================
# REPAIR KEV CARBON SHINY APP STRUCTURE
# Run this once from:
# C:/Users/Kevan/Documents/Kev Carbon
# ============================================================

project_root <- "C:/Users/Kevan/Documents/Kev Carbon"

setwd(project_root)

# Create correct app folder
dir.create("app", showWarnings = FALSE)

# If an accidental nested app/app exists, remove it
nested <- file.path(project_root, "app", "app")
if (dir.exists(nested)) {
  unlink(nested, recursive = TRUE, force = TRUE)
}

# Copy the new phone app into the canonical Shiny location
source_file <- file.path(project_root, "KevCarbon_phone_app_fixed.R")
target_file <- file.path(project_root, "app", "app.R")

if (!file.exists(source_file)) {
  stop("Cannot find KevCarbon_phone_app_fixed.R in the Kev Carbon folder.")
}

ok <- file.copy(source_file, target_file, overwrite = TRUE)

if (!ok) {
  stop("Could not copy the fixed app into app/app.R")
}

cat("\nRepair complete.\n")
cat("App file:", target_file, "\n")
cat("Data file exists:", file.exists(file.path(project_root, "data", "AusData.csv")), "\n")
cat("\nRun with:\n")
cat('shiny::runApp("app")\n')

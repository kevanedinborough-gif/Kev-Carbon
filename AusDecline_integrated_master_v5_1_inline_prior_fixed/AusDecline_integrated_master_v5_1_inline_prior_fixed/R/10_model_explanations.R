model_plain_english <- list(
  single = paste(
    "This model assumes that the radiocarbon-derived trajectory changes at one proportional rate",
    "throughout the selected interval. In colonial terms, it asks whether the record can be",
    "described without a distinct break associated with European occupation."
  ),
  change = paste(
    "This model allows one rate before an unknown date and a different rate afterwards.",
    "It asks whether the trajectory changed abruptly, and whether that estimated date and",
    "its uncertainty overlap historically relevant phases of European colonisation."
  ),
  logistic = paste(
    "This model represents a gradual curved transition rather than an instantaneous break.",
    "It asks whether disruption accumulated progressively and then approached a different",
    "level, as might occur through spreading settlement, disease, dispossession and reorganisation."
  )
)

model_method_details <- list(
  single = paste(
    "Latent binned calendar ages are modelled with nimbleCarbon::dExponentialGrowth over",
    "the configured calendar interval. The parameter r controls the direction and strength",
    "of the proportional temporal trend."
  ),
  change = paste(
    "Latent binned calendar ages are modelled with nimbleCarbon::dDoubleExponentialGrowth.",
    "The model estimates an early rate, a late rate and a change point mu constrained away",
    "from the interval boundaries."
  ),
  logistic = paste(
    "Latent binned calendar ages are modelled with nimbleCarbon::dLogisticGrowth2.",
    "The parameter m is the inflection date and r_logistic controls the direction and",
    "steepness of the smooth transition."
  )
)

model_result_text <- function(model_key, matrices_by_model, calendar_display, bp_to_ad) {
  mat <- matrices_by_model[[model_key]]
  if (is.null(mat)) return("No posterior samples were available for this model.")
  if (model_key == "single") {
    med <- median(mat[, "r"], na.rm = TRUE)
    pneg <- mean(mat[, "r"] < 0, na.rm = TRUE)
    return(sprintf("The posterior median rate is %.5f, with P(rate < 0) = %.3f.", med, pneg))
  }
  if (model_key == "change") {
    mu <- median(mat[, "mu"], na.rm = TRUE)
    psteep <- mean(mat[, "r_late"] < mat[, "r_early"], na.rm = TRUE)
    when <- if (calendar_display == "calad") paste0("AD ", round(bp_to_ad(mu))) else paste0(round(mu), " cal BP")
    return(sprintf("The median change point is %s, with P(late rate < early rate) = %.3f.", when, psteep))
  }
  if (model_key == "logistic") {
    m <- median(mat[, "m"], na.rm = TRUE)
    pneg <- mean(mat[, "r_logistic"] < 0, na.rm = TRUE)
    when <- if (calendar_display == "calad") paste0("AD ", round(bp_to_ad(m))) else paste0(round(m), " cal BP")
    return(sprintf("The median inflection date is %s, with P(logistic rate < 0) = %.3f.", when, pneg))
  }
  "No interpretation is defined."
}

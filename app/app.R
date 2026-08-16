library(shiny)
library(bslib)
library(ggplot2)
library(readr)
library(dplyr)

# ============================================================
# KEV CARBON — ELEGANT MOBILE BAYES DASHBOARD
# Uses saved graphics and results from AusData_NIMBLE_results.rds
# ============================================================

# ---------- Find files ----------
first_existing <- function(paths) {
  z <- paths[file.exists(paths)]
  if (length(z)) z[1] else NA_character_
}

data_file <- first_existing(c(
  "C:/Users/Kevan/Documents/Kev Carbon/data/AusData.csv",
  "C:/Users/Kevan/Documents/Kev Carbon/AusData.csv",
  "data/AusData.csv", "../data/AusData.csv",
  "AusData.csv", "../AusData.csv"
))

results_file <- first_existing(c(
  "C:/Users/Kevan/Documents/Kev Carbon/data/AusData_NIMBLE_results.rds",
  "data/AusData_NIMBLE_results.rds",
  "../data/AusData_NIMBLE_results.rds"
))

if (is.na(data_file)) stop("Cannot find AusData.csv.")
if (is.na(results_file)) stop("Cannot find AusData_NIMBLE_results.rds.")

aus <- read_csv(data_file, show_col_types = FALSE)
res <- readRDS(results_file)

# ---------- Helpers ----------
scalar_text <- function(x, default = "—") {
  if (is.null(x) || length(x) == 0) return(default)
  out <- tryCatch(as.character(x[[1]]), error = function(e) default)
  if (!length(out) || is.na(out) || !nzchar(out)) default else out
}

fmt_weight <- function(x) {
  if (is.null(x) || length(x) == 0) return("—")
  v <- suppressWarnings(as.numeric(x[[1]]))
  if (!is.finite(v)) return(scalar_text(x))
  if (v <= 1) sprintf("%.1f%%", 100 * v) else format(round(v, 3), trim = TRUE)
}

display_table <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) return(x)
  if (is.matrix(x)) return(as.data.frame(x))
  if (is.atomic(x) && !is.null(names(x))) {
    return(data.frame(Item = names(x), Value = unname(x), check.names = FALSE))
  }
  NULL
}

is_plot <- function(x) inherits(x, "ggplot")

# ---------- Theme ----------
theme_kevc <- bs_theme(
  version = 5,
  bg = "#F7F8FA",
  fg = "#183B4E",
  primary = "#176B87",
  secondary = "#D89B2B",
  base_font = font_google("Source Sans 3"),
  heading_font = font_google("Source Sans 3")
)

custom_css <- "
body { background: #F7F8FA; }
.navbar { box-shadow: 0 2px 12px rgba(0,0,0,.08); }
.card { border: 0; border-radius: 18px; box-shadow: 0 4px 18px rgba(24,59,78,.08); }
.card-header { font-weight: 700; background: transparent; border-bottom: 1px solid rgba(0,0,0,.06); }
.bslib-value-box { border-radius: 18px; }
.hero-note { font-size: 1.05rem; line-height: 1.5; }
.result-callout { border-left: 5px solid #D89B2B; padding: 14px 16px; background: #FFF9ED; border-radius: 12px; }
.small-muted { color: #6C7A83; font-size: .92rem; }
@media (max-width: 600px) {
  .container-fluid { padding-left: 10px; padding-right: 10px; }
  .card { border-radius: 14px; }
  .bslib-value-box { min-height: 120px; }
}
"

# ---------- UI ----------
ui <- page_navbar(
  title = "Kev Carbon",
  theme = theme_kevc,
  header = tags$head(tags$style(HTML(custom_css))),

  nav_panel(
    "Overview",

    div(
      class = "hero-note mb-3",
      h2("Australian radiocarbon decline"),
      p("Saved Bayesian NIMBLE results — no model rerun required.")
    ),

    layout_column_wrap(
      width = 1/2,

      value_box(
        title = "Best model",
        value = textOutput("best_model"),
        showcase = bsicons::bs_icon("diagram-3"),
        theme = "primary"
      ),

      value_box(
        title = "Model support",
        value = textOutput("best_weight"),
        showcase = bsicons::bs_icon("bar-chart"),
        theme = "warning"
      ),

      value_box(
        title = "Slope change",
        value = textOutput("change_point"),
        showcase = bsicons::bs_icon("signpost-split"),
        theme = "success"
      ),

      value_box(
        title = "Run mode",
        value = textOutput("run_mode"),
        showcase = bsicons::bs_icon("cpu"),
        theme = "secondary"
      )
    ),

    card(
      card_header("Interpretation"),
      div(
        class = "result-callout",
        uiOutput("decline_statement")
      )
    ),

    card(
      card_header("Which decline model is best supported?"),
      plotOutput("weights_plot", height = "340px")
    ),

    card(
      card_header("Where is the fitted decline steepest?"),
      plotOutput("winner_slope_plot", height = "400px")
    )
  ),

  nav_panel(
    "Bayes",

    card(
      card_header("Model comparison"),
      tableOutput("comparison")
    ),

    card(
      card_header("Posterior parameter summary"),
      tableOutput("posterior")
    ),

    layout_columns(
      col_widths = c(6, 6),

      card(
        card_header("Change point"),
        h3(textOutput("change_point_big")),
        p(class = "small-muted",
          "Posterior estimate saved from the completed long NIMBLE run.")
      ),

      card(
        card_header("Support"),
        h3(textOutput("support_label")),
        p(class = "small-muted",
          "Relative model support based on the saved comparison.")
      )
    ),

    card(
      card_header("Fitted trajectory"),
      plotOutput("curve_plot", height = "430px")
    ),

    card(
      card_header("Slope through time"),
      plotOutput("slope_plot", height = "430px")
    )
  ),

  nav_panel(
    "Diagnostics",

    layout_column_wrap(
      width = 1/2,

      value_box(
        title = "Iterations",
        value = textOutput("niter"),
        showcase = bsicons::bs_icon("repeat")
      ),

      value_box(
        title = "Chains",
        value = textOutput("nchains"),
        showcase = bsicons::bs_icon("layers")
      ),

      value_box(
        title = "Burn-in",
        value = textOutput("burnin"),
        showcase = bsicons::bs_icon("fire")
      ),

      value_box(
        title = "Thin",
        value = textOutput("thin"),
        showcase = bsicons::bs_icon("funnel")
      )
    ),

    card(
      card_header("Convergence"),
      tableOutput("convergence_table")
    ),

    accordion(
      accordion_panel(
        "Technical details",
        verbatimTextOutput("technical")
      )
    )
  ),

  nav_panel(
    "Data",

    layout_column_wrap(
      width = 1/2,
      value_box(title = "Dates", value = nrow(aus)),
      value_box(title = "Variables", value = ncol(aus))
    ),

    card(
      card_header("Radiocarbon determinations"),
      plotOutput("age_plot", height = "380px")
    ),

    card(
      card_header("Preview"),
      tableOutput("preview")
    )
  ),

  nav_panel(
    "About",
    card(
      card_header("Kev Carbon"),
      h3("Bayesian Australian radiocarbon modelling"),
      p("Mobile research dashboard for saved NIMBLE outputs."),
      p(class = "small-muted",
        paste("Results file:", basename(results_file)))
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {

  output$best_model <- renderText({
    if (!is.null(res$best)) scalar_text(res$best)
    else if (!is.null(res$best_model)) scalar_text(res$best_model)
    else "—"
  })

  output$best_weight <- renderText(fmt_weight(res$best_weight))

  output$change_point <- renderText({
    if (!is.null(res$change_point_text)) {
      sub("^Estimated slope change:\\s*", "", scalar_text(res$change_point_text))
    } else if (!is.null(res$mu_median)) {
      paste("AD", round(as.numeric(res$mu_median)))
    } else "—"
  })

  output$run_mode <- renderText({
    toupper(scalar_text(res$run_mode))
  })

  output$decline_statement <- renderUI({
    txt <- scalar_text(
      res$decline_statement,
      "No saved interpretation statement found."
    )
    tagList(
      h4(txt),
      if (!is.null(res$support_label))
        p(class = "small-muted", scalar_text(res$support_label))
    )
  })

  output$weights_plot <- renderPlot({
    validate(need(is_plot(res$p_weights),
                  "Saved model-weight plot not available."))
    res$p_weights + theme(legend.position = "none")
  })

  output$winner_slope_plot <- renderPlot({
    validate(need(is_plot(res$p_winner_slope),
                  "Saved steepest-slope plot not available."))
    res$p_winner_slope
  })

  output$comparison <- renderTable({
    tab <- display_table(res$comparison)
    validate(need(!is.null(tab), "No model comparison table available."))
    tab
  }, striped = TRUE, hover = TRUE, bordered = FALSE, spacing = "s")

  output$posterior <- renderTable({
    tab <- display_table(res$posterior_summary)
    validate(need(!is.null(tab), "No posterior summary table available."))
    tab
  }, striped = TRUE, hover = TRUE, bordered = FALSE, spacing = "s")

  output$change_point_big <- renderText({
    scalar_text(res$change_point_text)
  })

  output$support_label <- renderText({
    scalar_text(res$support_label)
  })

  output$curve_plot <- renderPlot({
    if (is_plot(res$p_winner)) {
      print(res$p_winner)
      return()
    }
    if (is_plot(res$p_curve)) {
      print(res$p_curve)
      return()
    }

    tab <- display_table(res$curve_data)
    validate(need(!is.null(tab), "No saved fitted-curve graphic available."))

    nums <- names(tab)[vapply(tab, is.numeric, logical(1))]
    validate(need(length(nums) >= 2, "Curve data has insufficient numeric columns."))

    ggplot(tab, aes(x = .data[[nums[1]]], y = .data[[nums[2]]])) +
      geom_line(linewidth = 1.1) +
      theme_minimal(base_size = 15) +
      labs(title = "Posterior fitted trajectory",
           x = nums[1], y = nums[2])
  })

  output$slope_plot <- renderPlot({
    if (is_plot(res$p_slope)) {
      print(res$p_slope)
      return()
    }
    if (is_plot(res$p_winner_slope)) {
      print(res$p_winner_slope)
      return()
    }
    validate("No saved slope plot available.")
  })

  output$niter <- renderText({
    if (!is.null(res$mcmc_cfg$niter)) format(res$mcmc_cfg$niter, big.mark = ",") else "—"
  })

  output$nchains <- renderText({
    if (!is.null(res$mcmc_cfg$nchains)) res$mcmc_cfg$nchains else "—"
  })

  output$burnin <- renderText({
    if (!is.null(res$mcmc_cfg$nburnin)) format(res$mcmc_cfg$nburnin, big.mark = ",") else "—"
  })

  output$thin <- renderText({
    if (!is.null(res$mcmc_cfg$thin)) res$mcmc_cfg$thin else "—"
  })

  output$convergence_table <- renderTable({
    if (is.data.frame(res$convergence) || is.matrix(res$convergence)) {
      return(as.data.frame(res$convergence))
    }

    # rhat_table is a function in this saved environment, so don't try to print it.
    data.frame(
      Diagnostic = c("Saved convergence object", "WAIC available"),
      Value = c(
        scalar_text(res$convergence),
        scalar_text(res$waic_available)
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$technical <- renderPrint({
    cat("Results:", normalizePath(results_file), "\n")
    cat("Data:", normalizePath(data_file), "\n\n")

    cat("Analysis window:\n")
    print(c(
      start_AD = res$window_start_ad,
      end_AD = res$window_end_ad
    ))

    cat("\nMCMC configuration:\n")
    print(res$mcmc_cfg)

    cat("\nBest delta:\n")
    print(res$best_delta)

    cat("\nBest weight:\n")
    print(res$best_weight)

    cat("\nObjects available:\n")
    print(names(res))
  })

  output$age_plot <- renderPlot({
    numeric_cols <- names(aus)[vapply(aus, is.numeric, logical(1))]
    validate(need(length(numeric_cols), "No numeric columns found."))

    age_col <- if ("C14Age" %in% names(aus)) "C14Age" else numeric_cols[1]

    ggplot(aus, aes(x = .data[[age_col]])) +
      geom_histogram(bins = 45) +
      scale_x_reverse() +
      theme_minimal(base_size = 15) +
      labs(
        x = "Radiocarbon age BP",
        y = "Determinations",
        title = "Australian radiocarbon dataset"
      )
  })

  output$preview <- renderTable({
    head(aus, 20)
  }, striped = TRUE, hover = TRUE)
}

shinyApp(ui, server)

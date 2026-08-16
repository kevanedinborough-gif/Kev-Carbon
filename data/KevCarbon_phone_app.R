library(shiny)
library(bslib)
library(ggplot2)
library(readr)
library(dplyr)

# ============================================================
# KEV CARBON — PHONE SHINY APP
# Real AusData + calibrated SPD + optional saved NIMBLE results
# ============================================================

# ---- Find the project/data file robustly ----
possible_data <- c(
  "data/AusData.csv",
  "../data/AusData.csv",
  "AusData.csv",
  "../AusData.csv"
)

data_path <- possible_data[file.exists(possible_data)][1]

if (is.na(data_path)) {
  stop(
    paste0(
      "Cannot find AusData.csv.\nWorking directory: ", getwd(), "\n",
      "Checked:\n", paste(possible_data, collapse = "\n")
    )
  )
}

aus <- read_csv(data_path, show_col_types = FALSE)

required <- c("C14Age", "C14SD", "LabCode", "SiteID")
missing_required <- setdiff(required, names(aus))

if (length(missing_required) > 0) {
  stop(
    "AusData.csv is missing required columns: ",
    paste(missing_required, collapse = ", ")
  )
}

# Clean the core radiocarbon fields used by the existing workflow.
rc_data <- aus |>
  mutate(
    C14Age = suppressWarnings(as.numeric(C14Age)),
    C14SD  = suppressWarnings(as.numeric(C14SD)),
    LabCode = as.character(LabCode),
    SiteID  = as.character(SiteID)
  ) |>
  filter(
    is.finite(C14Age),
    is.finite(C14SD),
    C14Age > 0,
    C14SD > 0
  )

# ---- Optional precomputed SPD ----
possible_spd <- c(
  "data/spd.csv",
  "../data/spd.csv",
  "spd.csv",
  "../spd.csv"
)
spd_path <- possible_spd[file.exists(possible_spd)][1]

precomputed_spd <- NULL
if (!is.na(spd_path)) {
  tmp <- try(read_csv(spd_path, show_col_types = FALSE), silent = TRUE)
  if (!inherits(tmp, "try-error")) {
    precomputed_spd <- tmp
  }
}

# ---- Optional saved NIMBLE result file ----
possible_results <- c(
  "data/AusData_decline_model_fits_long.rds",
  "../data/AusData_decline_model_fits_long.rds",
  "data/AusData_decline_model_fits_medium.rds",
  "../data/AusData_decline_model_fits_medium.rds",
  "data/AusData_decline_model_fits_test.rds",
  "../data/AusData_decline_model_fits_test.rds"
)
result_path <- possible_results[file.exists(possible_results)][1]

saved_results <- NULL
if (!is.na(result_path)) {
  saved_results <- try(readRDS(result_path), silent = TRUE)
  if (inherits(saved_results, "try-error")) saved_results <- NULL
}

# ---- UI ----
ui <- page_navbar(
  title = "Kev Carbon",
  theme = bs_theme(version = 5),

  nav_panel(
    "Data",

    layout_column_wrap(
      width = 1/2,

      value_box(
        title = "Valid radiocarbon dates",
        value = nrow(rc_data)
      ),

      value_box(
        title = "Sites",
        value = length(unique(rc_data$SiteID))
      )
    ),

    card(
      card_header("Radiocarbon ages"),
      plotOutput("age_plot", height = "390px")
    ),

    card(
      card_header("Data preview"),
      tableOutput("data_preview")
    )
  ),

  nav_panel(
    "SPD",

    card(
      card_header("Calibrated summed probability distribution"),

      p(
        "If data/spd.csv exists, it is loaded immediately. ",
        "Otherwise press Calculate SPD. Calibration uses C14Age and C14SD."
      ),

      layout_columns(
        col_widths = c(6, 6),

        numericInput(
          "spd_start",
          "Oldest age (cal BP)",
          value = 10000,
          min = 500,
          max = 50000,
          step = 500
        ),

        numericInput(
          "spd_end",
          "Youngest age (cal BP)",
          value = 0,
          min = 0,
          max = 10000,
          step = 100
        )
      ),

      actionButton(
        "calculate_spd",
        "Calculate SPD",
        class = "btn-primary"
      ),

      br(), br(),

      textOutput("spd_status"),

      plotOutput("spd_plot", height = "430px")
    )
  ),

  nav_panel(
    "Bayes",

    layout_column_wrap(
      width = 1/2,

      value_box(
        title = "Saved NIMBLE output",
        value = if (is.null(saved_results)) "Not loaded" else "Loaded"
      ),

      value_box(
        title = "Mode",
        value = "Read-only"
      )
    ),

    card(
      card_header("Bayesian model results"),
      p(
        "The phone app reads saved NIMBLE output rather than compiling ",
        "and running MCMC on the phone-facing server."
      ),
      verbatimTextOutput("nimble_summary")
    )
  ),

  nav_panel(
    "About",

    card(
      card_header("Kev Carbon"),
      h4("Bayesian Australian radiocarbon modelling"),
      p(
        "Interactive companion to the Australian radiocarbon decline-slope workflow."
      ),
      tags$ul(
        tags$li("Radiocarbon ages: C14Age"),
        tags$li("One-sigma error: C14SD"),
        tags$li("Laboratory ID: LabCode"),
        tags$li("Site grouping: SiteID")
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {

  output$age_plot <- renderPlot({
    ggplot(rc_data, aes(x = C14Age)) +
      geom_histogram(bins = 45) +
      scale_x_reverse() +
      labs(
        x = "Radiocarbon age BP",
        y = "Number of determinations",
        title = "Australian radiocarbon determinations"
      ) +
      theme_minimal(base_size = 16)
  })

  output$data_preview <- renderTable({
    rc_data |>
      select(LabCode, SiteID, C14Age, C14SD) |>
      head(20)
  })

  spd_store <- reactiveVal(precomputed_spd)

  observeEvent(input$calculate_spd, {

    if (!requireNamespace("rcarbon", quietly = TRUE)) {
      showNotification(
        "Install rcarbon first: install.packages('rcarbon')",
        type = "error",
        duration = NULL
      )
      return()
    }

    showNotification(
      "Calibrating dates and calculating SPD...",
      type = "message",
      duration = 4
    )

    tryCatch({

      calibrated <- rcarbon::calibrate(
        x = rc_data$C14Age,
        errors = rc_data$C14SD,
        calCurves = "intcal20"
      )

      bins <- rcarbon::binPrep(
        sites = rc_data$SiteID,
        ages = rc_data$C14Age,
        h = 100
      )

      spd_obj <- rcarbon::spd(
        calibrated,
        bins = bins,
        timeRange = c(input$spd_start, input$spd_end)
      )

      # rcarbon SPD objects have changed representation across versions.
      # Try the common extraction routes.
      candidate <- NULL

      if (is.data.frame(spd_obj)) {
        candidate <- spd_obj
      }

      if (is.null(candidate) && !is.null(spd_obj$grid)) {
        candidate <- as.data.frame(spd_obj$grid)
      }

      if (is.null(candidate) && !is.null(spd_obj$spd)) {
        candidate <- as.data.frame(spd_obj$spd)
      }

      if (is.null(candidate)) {
        # Fall back to a plot-friendly extraction if as.data.frame works.
        candidate <- try(as.data.frame(spd_obj), silent = TRUE)
        if (inherits(candidate, "try-error")) candidate <- NULL
      }

      if (is.null(candidate) || ncol(candidate) < 2) {
        stop(
          "SPD calculated, but this rcarbon version returned an ",
          "unrecognised SPD object. Send me str(spd_obj) and I will map it."
        )
      }

      # Identify likely time and density columns.
      nm <- names(candidate)

      time_col <- grep(
        "calBP|time|year|age",
        nm,
        ignore.case = TRUE,
        value = TRUE
      )[1]

      dens_col <- grep(
        "PrDens|density|spd|prob",
        nm,
        ignore.case = TRUE,
        value = TRUE
      )[1]

      if (is.na(time_col)) time_col <- nm[1]
      if (is.na(dens_col)) dens_col <- nm[2]

      out <- data.frame(
        calBP = as.numeric(candidate[[time_col]]),
        PrDens = as.numeric(candidate[[dens_col]])
      ) |>
        filter(is.finite(calBP), is.finite(PrDens))

      spd_store(out)

      showNotification(
        "SPD ready.",
        type = "message",
        duration = 3
      )

    }, error = function(e) {
      showNotification(
        paste("SPD error:", conditionMessage(e)),
        type = "error",
        duration = NULL
      )
    })
  })

  output$spd_status <- renderText({
    x <- spd_store()

    if (is.null(x)) {
      "No precomputed SPD loaded."
    } else {
      paste(format(nrow(x), big.mark = ","), "SPD time points loaded.")
    }
  })

  output$spd_plot <- renderPlot({
    x <- spd_store()

    validate(
      need(
        !is.null(x),
        "Press Calculate SPD, or place spd.csv in the data folder."
      )
    )

    # Accept either our canonical names or a compatible two-column file.
    if (!all(c("calBP", "PrDens") %in% names(x))) {
      validate(
        need(
          ncol(x) >= 2,
          "spd.csv needs at least two columns."
        )
      )
      x <- data.frame(
        calBP = as.numeric(x[[1]]),
        PrDens = as.numeric(x[[2]])
      )
    }

    ggplot(x, aes(x = calBP, y = PrDens)) +
      geom_line(linewidth = 1) +
      scale_x_reverse() +
      labs(
        x = "Calendar years BP",
        y = "Summed probability",
        title = "Calibrated SPD"
      ) +
      theme_minimal(base_size = 16)
  })

  output$nimble_summary <- renderPrint({

    if (is.null(saved_results)) {
      cat(
        "No saved NIMBLE result file found.\n\n",
        "The app checked for:\n",
        paste(possible_results, collapse = "\n"),
        "\n\nNext step: copy one completed ",
        "AusData_decline_model_fits_*.rds file into data/."
      )
      return()
    }

    cat("Loaded:", result_path, "\n\n")

    # Give a safe compact view without assuming one historical object structure.
    if (is.list(saved_results)) {
      cat("Top-level objects:\n")
      print(names(saved_results))

      cat("\nObject structure:\n")
      str(saved_results, max.level = 2)
    } else {
      print(saved_results)
    }
  })
}

shinyApp(ui, server)

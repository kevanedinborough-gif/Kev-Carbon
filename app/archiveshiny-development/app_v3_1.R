library(shiny)
library(bslib)
library(ggplot2)
library(readr)
library(dplyr)

# RLifeHistory Remote Console v3.1
# Windows-safe background launcher.
# Runs the existing NIMBLE vignette in a separate R process.

possible_paths <- c("data/AusData.csv", "../data/AusData.csv", "AusData.csv",
                    "../AusData.csv", "AUS.csv", "../AUS.csv")
existing_paths <- possible_paths[file.exists(possible_paths)]
aus <- if (length(existing_paths)) read_csv(existing_paths[1], show_col_types = FALSE) else NULL

status_dir <- "run_status"
status_file <- file.path(status_dir, "latest.txt")
log_file <- file.path(status_dir, "latest.log")
report_file <- "AusData_simple_NIMBLE_slope_models.html"

read_status <- function() {
  if (!file.exists(status_file)) return(list(status = "READY"))
  z <- readLines(status_file, warn = FALSE)
  parts <- strsplit(z, "=", fixed = TRUE)
  keys <- vapply(parts, `[`, character(1), 1)
  vals <- vapply(parts, function(x) paste(x[-1], collapse = "="), character(1))
  as.list(setNames(vals, keys))
}

ui <- page_navbar(
  title = "RLifeHistory",

  nav_panel("Data",
    card(
      card_header("Australian radiocarbon data"),
      if (is.null(aus)) {
        p("AusData.csv not found here. The NIMBLE vignette itself can still use AusData.xlsx.")
      } else {
        tagList(
          p(paste(nrow(aus), "rows loaded")),
          plotOutput("data_plot", height = "430px")
        )
      },
      full_screen = TRUE
    )
  ),

  nav_panel("Run",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons(
          "run_mode",
          "Run length",
          choices = c("Test" = "test", "Medium" = "medium", "Long" = "long"),
          selected = "test"
        ),
        actionButton("run_job", "RUN NIMBLE ANALYSIS", class = "btn-primary"),
        hr(),
        p("Runs your existing vignette in a separate R process on the desktop.")
      ),

      card(
        card_header("Desktop job status"),
        uiOutput("job_status"),
        actionButton("refresh", "Refresh status")
      ),

      card(
        card_header("Latest log"),
        verbatimTextOutput("job_log")
      )
    )
  ),

  nav_panel("Results",
    card(
      card_header("Latest completed report"),
      uiOutput("report_status"),
      downloadButton("download_report", "Download HTML report")
    )
  )
)

server <- function(input, output, session) {

  output$data_plot <- renderPlot({
    req(!is.null(aus))
    dat <- aus
    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
    validate(need(length(numeric_cols) >= 1, "No numeric columns found."))
    likely_age <- grep("age|date|c14|bp", numeric_cols, ignore.case = TRUE, value = TRUE)
    age_col <- if (length(likely_age)) likely_age[1] else numeric_cols[1]

    plot_dat <- data.frame(
      Index = seq_len(nrow(dat)),
      Age = dat[[age_col]]
    )

    ggplot(plot_dat, aes(Index, Age)) +
      geom_point(size = 2, alpha = 0.7) +
      labs(
        title = paste("Using column:", age_col),
        x = "Record",
        y = age_col
      ) +
      theme_minimal(base_size = 16)
  })

  observeEvent(input$run_job, {
    st <- read_status()

    if (!is.null(st$status) && identical(st$status, "RUNNING")) {
      showNotification("A NIMBLE job is already marked as running.", type = "warning")
      return()
    }

    req(file.exists("AusData_simple_NIMBLE_slope_models.Rmd"))
    req(file.exists("run_shiny_job.R"))
    req(file.exists("AusData.xlsx"))

    dir.create(status_dir, showWarnings = FALSE, recursive = TRUE)

    if (file.exists(log_file)) unlink(log_file)

    rscript <- file.path(R.home("bin"), "Rscript.exe")
    if (!file.exists(rscript)) {
      rscript <- file.path(R.home("bin"), "Rscript")
    }

    rscript <- normalizePath(rscript, winslash = "\\", mustWork = TRUE)
    worker <- normalizePath("run_shiny_job.R", winslash = "\\", mustWork = TRUE)
    logfile_abs <- normalizePath(status_dir, winslash = "\\", mustWork = TRUE)
    logfile_abs <- file.path(logfile_abs, "latest.log")

    # Windows-safe launch:
    # use system2() so executable + args are passed separately and paths with spaces work.
    if (.Platform$OS.type == "windows") {
      system2(
        command = rscript,
        args = c(
          shQuote(worker),
          input$run_mode
        ),
        stdout = logfile_abs,
        stderr = logfile_abs,
        wait = FALSE,
        invisible = TRUE
      )
    } else {
      system2(
        command = rscript,
        args = c(worker, input$run_mode),
        stdout = logfile_abs,
        stderr = logfile_abs,
        wait = FALSE,
        invisible = TRUE
      )
    }

    showNotification(
      paste("Started", input$run_mode, "analysis on desktop."),
      type = "message"
    )
  })

  output$job_status <- renderUI({
    input$refresh
    invalidateLater(3000, session)

    st <- read_status()
    status <- if (is.null(st$status)) "UNKNOWN" else st$status

    cls <- switch(
      status,
      RUNNING = "alert alert-warning",
      FINISHED = "alert alert-success",
      ERROR = "alert alert-danger",
      "alert alert-secondary"
    )

    tags$div(
      class = cls,
      tags$h4(status),
      if (!is.null(st$mode)) tags$p(paste("Mode:", st$mode)),
      if (!is.null(st$started)) tags$p(paste("Started:", st$started)),
      if (!is.null(st$finished)) tags$p(paste("Finished:", st$finished)),
      if (!is.null(st$error)) tags$p(paste("Error:", st$error))
    )
  })

  output$job_log <- renderText({
    input$refresh
    invalidateLater(5000, session)

    if (!file.exists(log_file)) {
      return("No job log yet.")
    }

    x <- readLines(log_file, warn = FALSE)
    paste(tail(x, 40), collapse = "\n")
  })

  output$report_status <- renderUI({
    input$refresh
    invalidateLater(5000, session)

    if (file.exists(report_file)) {
      tags$div(
        class = "alert alert-success",
        tags$strong("HTML report is available."),
        tags$br(),
        paste("Last modified:", format(file.info(report_file)$mtime))
      )
    } else {
      tags$div(
        class = "alert alert-secondary",
        "No completed HTML report found yet."
      )
    }
  })

  output$download_report <- downloadHandler(
    filename = function() {
      paste0("RLifeHistory_", Sys.Date(), ".html")
    },
    content = function(file) {
      req(file.exists(report_file))
      file.copy(report_file, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)

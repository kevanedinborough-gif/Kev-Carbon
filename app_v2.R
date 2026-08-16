library(shiny)
library(bslib)
library(ggplot2)
library(readr)
library(dplyr)

# ============================================================
# Kev Carbon / RLifeHistory Remote Console v2
# ------------------------------------------------------------
# Keeps the original working real-data explorer intact and adds
# phone-friendly navigation plus model/run/results scaffolding.
#
# IMPORTANT:
# - No NIMBLE model is run in this version yet.
# - The Run tab records the requested configuration only.
# - The next step is to connect your existing working NIMBLE code.
# ============================================================

# ---- Locate real data robustly ----

possible_paths <- c(
  "data/AusData.csv",
  "../data/AusData.csv",
  "AusData.csv",
  "../AusData.csv",
  "AUS.csv",
  "../AUS.csv"
)

existing_paths <- possible_paths[file.exists(possible_paths)]

if (length(existing_paths) == 0) {
  stop(
    paste0(
      "No AusData file could be located.\n",
      "Working directory: ", getwd(), "\n",
      "Checked:\n",
      paste(possible_paths, collapse = "\n")
    )
  )
}

data_path <- existing_paths[1]

cat("Using data file:", normalizePath(data_path), "\n")

aus <- read_csv(
  data_path,
  show_col_types = FALSE
)

# ---- Helpers ----

run_settings <- list(
  Test = list(iter = 2000, burnin = 500, thin = 1),
  Medium = list(iter = 20000, burnin = 5000, thin = 5),
  Long = list(iter = 100000, burnin = 20000, thin = 10)
)

# ---- App UI ----

ui <- page_navbar(
  title = "Kev Carbon",

  nav_panel(
    "Data",

    layout_sidebar(
      sidebar = sidebar(
        h4("Australian radiocarbon explorer"),

        sliderInput(
          "rows",
          "Dates to display",
          min = 10,
          max = max(10, nrow(aus)),
          value = min(100, nrow(aus)),
          step = 10
        ),

        checkboxInput(
          "show_table",
          "Show data table",
          value = FALSE
        ),

        hr(),

        downloadButton(
          "download_data",
          "Download displayed data",
          class = "btn-primary"
        )
      ),

      layout_column_wrap(
        width = 1/2,

        value_box(
          title = "Radiocarbon dates",
          value = nrow(aus)
        ),

        value_box(
          title = "Variables",
          value = ncol(aus)
        )
      ),

      card(
        card_header("Dataset structure"),
        verbatimTextOutput("columns")
      ),

      card(
        card_header("Radiocarbon data"),
        plotOutput(
          "data_plot",
          height = "420px"
        ),
        full_screen = TRUE
      ),

      conditionalPanel(
        condition = "input.show_table == true",

        card(
          card_header("Data preview"),
          tableOutput("data_table")
        )
      )
    )
  ),

  nav_panel(
    "Models",

    layout_sidebar(
      sidebar = sidebar(
        h4("Model selection"),

        selectInput(
          "model",
          "Model",
          choices = c(
            "Single-slope exponential" = "single",
            "Two-slope / change-point" = "change"
          ),
          selected = "single"
        ),

        radioButtons(
          "run_mode",
          "Run length",
          choices = c("Test", "Medium", "Long"),
          selected = "Test"
        ),

        hr(),

        p(
          "This version prepares the model configuration. ",
          "Your working NIMBLE model code will be connected next."
        )
      ),

      card(
        card_header("Selected model"),
        uiOutput("model_summary")
      ),

      card(
        card_header("MCMC settings"),
        tableOutput("mcmc_settings")
      )
    )
  ),

  nav_panel(
    "Run",

    layout_column_wrap(
      width = 1,

      card(
        card_header("Remote run control"),

        p(
          "Choose the model and run length in the Models tab, ",
          "then prepare the run here."
        ),

        actionButton(
          "prepare_run",
          "Prepare run",
          class = "btn-primary"
        ),

        br(),
        br(),

        uiOutput("run_status")
      ),

      card(
        card_header("What happens in v2"),
        p(
          "The app checks and records your selected model and MCMC settings. ",
          "It does not yet start NIMBLE, so there is no risk of accidentally ",
          "launching a long computation from your phone."
        )
      )
    )
  ),

  nav_panel(
    "Results",

    layout_column_wrap(
      width = 1/2,

      value_box(
        title = "Model engine",
        value = "Not connected yet"
      ),

      value_box(
        title = "Saved results",
        value = "0"
      )
    ),

    card(
      card_header("Bayesian results"),
      uiOutput("results_message")
    ),

    card(
      card_header("Next connection"),
      p(
        "This panel is ready for posterior summaries, trace plots, ",
        "WAIC/model comparison and downloadable run outputs once ",
        "the working NIMBLE model code is connected."
      )
    )
  ),

  footer = tags$div(
    style = "padding: 12px; text-align: center; opacity: 0.7;",
    "RLifeHistory remote console • computation runs on the desktop PC"
  )
)

# ---- Server ----

server <- function(input, output, session) {

  output$columns <- renderPrint({
    names(aus)
  })

  display_data <- reactive({
    n <- min(
      input$rows,
      nrow(aus)
    )

    aus[seq_len(n), , drop = FALSE]
  })

  output$data_table <- renderTable({
    head(
      display_data(),
      30
    )
  })

  output$data_plot <- renderPlot({

    dat <- display_data()

    numeric_cols <- names(dat)[
      vapply(dat, is.numeric, logical(1))
    ]

    validate(
      need(
        length(numeric_cols) >= 1,
        "No numeric columns found."
      )
    )

    likely_age <- grep(
      "age|date|c14|bp",
      numeric_cols,
      ignore.case = TRUE,
      value = TRUE
    )

    if (length(likely_age) > 0) {
      age_col <- likely_age[1]
    } else {
      age_col <- numeric_cols[1]
    }

    plot_dat <- data.frame(
      Index = seq_len(nrow(dat)),
      Age = dat[[age_col]]
    )

    ggplot(
      plot_dat,
      aes(
        x = Index,
        y = Age
      )
    ) +
      geom_point(
        size = 2,
        alpha = 0.7
      ) +
      labs(
        title = paste(
          "Using column:",
          age_col
        ),
        x = "Record",
        y = age_col
      ) +
      theme_minimal(
        base_size = 16
      )
  })

  output$download_data <- downloadHandler(
    filename = function() {
      paste0(
        "AusData_displayed_",
        Sys.Date(),
        ".csv"
      )
    },
    content = function(file) {
      write_csv(
        display_data(),
        file
      )
    }
  )

  selected_settings <- reactive({
    req(input$run_mode)
    run_settings[[input$run_mode]]
  })

  output$model_summary <- renderUI({
    req(input$model, input$run_mode)

    model_name <- switch(
      input$model,
      single = "Single-slope exponential",
      change = "Two-slope / change-point"
    )

    tags$div(
      tags$h4(model_name),
      tags$p(
        paste("Run mode:", input$run_mode)
      )
    )
  })

  output$mcmc_settings <- renderTable({
    s <- selected_settings()

    data.frame(
      Setting = c(
        "Iterations",
        "Burn-in",
        "Thin"
      ),
      Value = c(
        s$iter,
        s$burnin,
        s$thin
      )
    )
  })

  prepared_run <- eventReactive(
    input$prepare_run,
    {
      req(input$model, input$run_mode)

      s <- selected_settings()

      list(
        time = Sys.time(),
        model = switch(
          input$model,
          single = "Single-slope exponential",
          change = "Two-slope / change-point"
        ),
        mode = input$run_mode,
        iter = s$iter,
        burnin = s$burnin,
        thin = s$thin
      )
    },
    ignoreInit = TRUE
  )

  output$run_status <- renderUI({
    x <- prepared_run()

    if (is.null(x)) {
      return(
        tags$div(
          class = "alert alert-secondary",
          "No run has been prepared yet."
        )
      )
    }

    tags$div(
      class = "alert alert-success",
      tags$strong("Configuration prepared."),
      tags$br(),
      paste("Model:", x$model),
      tags$br(),
      paste("Mode:", x$mode),
      tags$br(),
      paste("Iterations:", format(x$iter, big.mark = ",")),
      tags$br(),
      paste("Burn-in:", format(x$burnin, big.mark = ",")),
      tags$br(),
      paste("Thin:", x$thin),
      tags$br(),
      paste("Prepared:", format(x$time))
    )
  })

  output$results_message <- renderUI({
    tags$div(
      class = "alert alert-info",
      tags$strong("No NIMBLE run has been started."),
      tags$br(),
      "This is intentional in v2. The next version will connect ",
      "the existing modelling code and populate this page with real results."
    )
  })
}

shinyApp(ui, server)

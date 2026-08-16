
# KevCarbon_graph_first_app_FIXED.R
library(shiny)
library(bslib)
library(ggplot2)
library(readr)
library(dplyr)

first_existing <- function(paths){
  z <- paths[file.exists(paths)]
  if(length(z)) z[1] else NA_character_
}

results_file <- first_existing(c(
"C:/Users/Kevan/Documents/Kev Carbon/data/AusData_NIMBLE_results.rds",
"data/AusData_NIMBLE_results.rds","../data/AusData_NIMBLE_results.rds"))

data_file <- first_existing(c(
"C:/Users/Kevan/Documents/Kev Carbon/data/AusData.csv",
"data/AusData.csv","../data/AusData.csv"))

res <- readRDS(results_file)
aus <- suppressWarnings(read_csv(data_file, show_col_types = FALSE))

is_gg <- function(x) inherits(x,"ggplot")

ui <- page_navbar(
  title="Kev Carbon",
  theme=bs_theme(version=5, primary="#176B87"),
  nav_panel("Home",
    h2("Australian radiocarbon decline"),
    verbatimTextOutput("statement"),
    card(card_header("Model support"),
         plotOutput("weights",height="350px")),
    card(card_header("Steepest decline"),
         plotOutput("winner",height="450px"))
  ),
  nav_panel("Bayes",
    card(card_header("Fitted trajectory"),
         plotOutput("curve",height="450px")),
    card(card_header("Slope through time"),
         plotOutput("slope",height="450px"))
  ),
  nav_panel("Data",
    plotOutput("hist",height="350px"))
)

server <- function(input,output,session){

 output$statement <- renderText({
   if(!is.null(res$decline_statement)) res$decline_statement else "No statement available."
 })

 output$weights <- renderPlot({
   if(is_gg(res$p_weights)){
     print(res$p_weights)
   } else {
     d <- res$weight_plot_data
     ggplot(d,aes(x=WAIC_weight,y=model,fill=selected))+
       geom_col()+theme_minimal()
   }
 },res=120)

 output$winner <- renderPlot({
   if(is_gg(res$p_winner_slope)){
     print(res$p_winner_slope)
   } else {
     d <- res$winner_slope
     ggplot(d,aes(calendar_display_value,slope_toward_present))+
       geom_line()+theme_minimal()
   }
 },res=120)

 output$curve <- renderPlot({
   if(is_gg(res$p_curve)) print(res$p_curve)
   else if(is_gg(res$p_winner)) print(res$p_winner)
   else {
      d <- res$curve_data
      n <- names(d)[sapply(d,is.numeric)]
      ggplot(d,aes(.data[[n[1]]],.data[[n[2]]]))+geom_line()+theme_minimal()
   }
 },res=120)

 output$slope <- renderPlot({
   if(is_gg(res$p_slope)) print(res$p_slope)
   else {
      d <- res$winner_slope
      ggplot(d,aes(calendar_display_value,slope_toward_present))+
        geom_line()+theme_minimal()
   }
 },res=120)

 output$hist <- renderPlot({
   nc <- names(aus)[sapply(aus,is.numeric)][1]
   ggplot(aus,aes(.data[[nc]]))+geom_histogram(bins=40)+theme_minimal()
 })
}

shinyApp(ui,server)

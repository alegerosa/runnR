library(shiny)
library(runnR)

ui <- fluidPage(
  fluidRow(
    column(
      width = 6,
      align = "center",
      h1("Pace Converter"),
      h4("Input the minutes and seconds of your pace in minutes per kilometer"),
      fluidRow(
        column(width = 6,
               selectInput(inputId = "pace_km_min", label = "Minutes", choices = as.list(1:25), selected = 7, width = "50%")
               ),
        column(width = 6,
               selectInput(inputId = "pace_km_sec", label = "Seconds", choices = as.list(0:60), selected = 10, width = "50%")
               )
        ),
      actionButton(inputId = "pace_km_submit", label = "Convert to min/mile"),
      h3(textOutput(outputId = "pace_mi_output")),
      hr(),
      h4("Input the minutes and seconds of your pace in minutes per mile"),
      fluidRow(
        column(width = 6,
               selectInput(inputId = "pace_mi_min", label = "Minutes", choices = as.list(1:25), selected = 11, width = "50%")
        ),
        column(width = 6,
               selectInput(inputId = "pace_mi_sec", label = "Seconds", choices = as.list(0:60), selected = 32, width = "50%")
        )
      ),
      actionButton(inputId = "pace_mi_submit", label = "Convert to min/km"),
      h3(textOutput(outputId = "pace_km_output")),
      hr())
      ))



server <- function(input, output) {
  pace_km_min <- eventReactive(input$pace_km_submit, {
    as.numeric(input$pace_km_min)
  })
  pace_km_sec <- eventReactive(input$pace_km_submit, {
    as.numeric(input$pace_km_sec)
  })
  output$pace_mi_output <- renderText({
    paste(
      "Your pace is ",
      pace_km_to_mi_num_input(pace_km_min(), pace_km_sec()),
      " minutes per mile")
  })

  pace_mi_min <- eventReactive(input$pace_mi_submit, {
    as.numeric(input$pace_mi_min)
  })
  pace_mi_sec <- eventReactive(input$pace_mi_submit, {
    as.numeric(input$pace_mi_sec)
  })
  output$pace_km_output <- renderText({
    paste(
      "Your pace is ",
      pace_mi_to_km_num_input(pace_mi_min(), pace_mi_sec()),
      " minutes per kilometer")

  })
}


shinyApp(ui = ui, server = server)

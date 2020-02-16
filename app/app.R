library(shiny)
library(runnR)

ui <- fluidPage(
  h1("Pace converter"),
  fluidRow(
    column(
      width = 6,
      align = "center",
      h4("Input the minutes and seconds of your pace in minutes per kilometer", align = "left"),
      fluidRow(
        column(width = 6,
               align = "right",
               numericInput(inputId = "pace_km_min", label = "Minutes", value = 7, min = 0, max = 60, step = 1, width = "50%")
               ),
        column(width = 6,
               align = "left",
               numericInput(inputId = "pace_km_sec", label = "Seconds", value = 10, min = 0, max = 60, step = 1, width = "50%")
               )
        ),
      actionButton(inputId = "pace_km_submit", label = "Convert to min/mile"),
      h3(textOutput(outputId = "pace_mi_output")),
      hr()),
    column(
      width = 6,
      align = "center",
      h4("Input the minutes and seconds of your pace in minutes per mile", align = "left"),
      fluidRow(
        column(width = 6,
               align = "right",
               numericInput(inputId = "pace_mi_min", label = "Minutes", value = 7, min = 0, max = 60, step = 1, width = "50%")
        ),
        column(width = 6,
               align = "left",
               numericInput(inputId = "pace_mi_sec", label = "Seconds", value = 10, min = 0, max = 60, step = 1, width = "50%")
        )
      ),
      actionButton(inputId = "pace_mi_submit", label = "Convert to min/km"),
      h3(textOutput(outputId = "pace_km_output")),
      hr())
      )
)


server <- function(input, output) {
  pace_km_min <- eventReactive(input$pace_km_submit, {
    input$pace_km_min
  })
  pace_km_sec <- eventReactive(input$pace_km_submit, {
    input$pace_km_sec
  })
  output$pace_mi_output <- renderText({
    paste(
      "Your pace is ",
      pace_km_to_mi_num_input(pace_km_min(), pace_km_sec()),
      " minutes per mile")
  })

  pace_mi_min <- eventReactive(input$pace_mi_submit, {
    input$pace_mi_min
  })
  pace_mi_sec <- eventReactive(input$pace_mi_submit, {
    input$pace_mi_sec
  })
  output$pace_km_output <- renderText({
    paste(
      "Your pace is ",
      pace_mi_to_km_num_input(pace_mi_min(), pace_mi_sec()),
      " minutes per kilometer")

  })
}


shinyApp(ui = ui, server = server)

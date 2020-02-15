library(shiny)
library(runnR)

ui <- fluidPage(
  tags$h1("Pace converter"),
  textInput(inputId = "pace_km", "Your pace in minutes per kilometer"),
  actionButton(inputId = "pace_km_submit", label = "Convert to min/mile"),
  textOutput(outputId = "pace_mi_output"),
  tags$br(),
  textInput(inputId = "pace_mi", "Your pace in minutes per mile"),
  actionButton(inputId = "pace_mi_submit", label = "Convert to min/km"),
  textOutput(outputId = "pace_km_output")
)




server <- function(input, output) {
  pace_km <- eventReactive(input$pace_km_submit, {
    input$pace_km
  })
  output$pace_mi_output <- renderText({
    paste(
      "Your pace is ",
      pace_km_to_pace_mi(pace_km()),
      " minutes per mile")

  })

  pace_mi <- eventReactive(input$pace_mi_submit, {
    input$pace_mi
  })
  output$pace_km_output <- renderText({
    paste(
      "Your pace is ",
      pace_mi_to_pace_km(pace_mi()),
      " minutes per kilometer")

  })
}

shinyApp(ui = ui, server = server)

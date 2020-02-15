library(shiny)
library(runnR)

ui <- fluidPage(
  "Convert min/km to min/mile",
  textInput(inputId = "pace_km", "Your pace in minutes per kilometer", value = "7:00"),
  textOutput(outputId = "pace_mi_output")
)




server <- function(input, output) {
  output$pace_mi_output <- renderText({
    paste(
      "Your pace is ",
      pace_km_to_pace_mi(input$pace_km),
      " minutes per mile")

  })
}

shinyApp(ui = ui, server = server)

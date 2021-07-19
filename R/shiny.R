#' Placeholder app for testing
#' @export
greeting_app <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::textInput("name", "What's your name"),
      shiny::textOutput("greeting"),
      shiny::actionButton("reset", "Reset")
    ),
    server = function(input, output, session) {
      output$greeting <- shiny::renderText({
        shiny::req(input$name)
        paste0("Hi ", input$name)
      })
      shiny::observeEvent(input$reset, shiny::updateTextInput(session, "name", value = ""))
    }
  )
}

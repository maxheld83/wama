#' Placeholder app for testing
#' @export
hello_world_app <- function() {
  shiny::shinyApp(
    # change string here to see updates live preview
    ui = shiny::fluidPage("Hello, world!"),
    # change string here to see updates shiny backend console
    server = function(input, output, session) print("Hello, server!")
  )
}

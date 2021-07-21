#' Refresh shiny app automatically
#' @inheritDotParams shiny::runApp
#' @export
run_app_auto <- function(...) {
  cli::cli_alert_info("Auto-updating enabled")
  sb <- cli::cli_status("{cli::symbol$arrow_right} Launching auto-updating")
  rp <- run_app_dev_bg(...)
  on.exit(rp$kill(), add = TRUE)
  testthat::watch(
    path = fs::dir_ls(recurse = TRUE),
    callback = function(added, deleted, modified) {
      cli::cli_status_update(
        id = sb,
        "{cli::symbol$arrow_right} Update triggered"
      )
      rp <<- run_app_dev_bg(rp = rp, ...)
      TRUE
    },
    hash = TRUE
  )
}

#' Run shiny app in dev context and background
#' @param rp
#' R background process, running the shiny session from a previous run.
#' Pass `NULL` (default) if there is no previous run.
#' @inheritDotParams shiny::runApp
#' @noRd
run_app_dev_bg <- function(rp = NULL,
                           ...) {
  sb <- cli::cli_status("{cli::symbol$arrow_right} Starting update")
  if (!is.null(rp)) {
    cli::cli_status_update(
      id = sb,
      "{cli::symbol$arrow_right} Shutting down prior background process"
    )
    rp$kill()
  }
  cli::cli_status_update(
    id = sb,
    "{cli::symbol$arrow_right} Starting new background process"
  )
  callr::r_bg(
    func = function(app, ...) {
      options(cli.message_class = "callr_message")
      app(...)
    },
    args = c(list(app = run_app_dev), list(...)),
    poll_connection = TRUE,
    supervise = TRUE,
    package = TRUE
  )
}

#' Run shiny app in dev context
#' @inheritDotParams shiny::runApp
#' @noRd
run_app_dev <- function(quiet = FALSE, port = 1613, ...) {
  sb <- cli::cli_status("{cli::symbol$arrow_right} Loading package")
  pkgload::load_all(quiet = TRUE)
  cli::cli_status_update(id = sb, "{cli::symbol$arrow_right} Launching app")
  shiny::runApp(
    appDir = getOption("wama.default.app", default = "."),
    quiet = quiet,
    port = port,
    ...
  )
}

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
      print("This is the shiny server talking.")
      output$greeting <- shiny::renderText({
        shiny::req(input$name)
        paste0("Hi ", input$name)
      })
      shiny::observeEvent(input$reset, shiny::updateTextInput(session, "name", value = ""))
    }
  )
}

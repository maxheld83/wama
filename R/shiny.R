#' Refresh shiny app automatically
#' @inheritParams shiny::runApp
#' @inheritDotParams shiny::runApp
#' @export
run_app_auto <- function(port = 1818, ...) {
  cli::cli_alert_info("Auto-updating enabled")
  sb <- cli::cli_status("{cli::symbol$arrow_right} Launching auto-updating")
  rp <- run_app_dev_bg(port = port, ...)
  # needs time to be ready
  # hack-fix until https://github.com/maxheld83/wama/issues/7
  Sys.sleep(2)
  # browse URL from within background processes does not work
  suppressMessages(browse_url2(url = paste0("http://localhost:", port)))
  on.exit(rp$kill(), add = TRUE)
  testthat::watch(
    path = fs::dir_ls(recurse = TRUE),
    callback = function(added, deleted, modified) {
      cli::cli_status_update(
        id = sb,
        "{cli::symbol$arrow_right} Update triggered"
      )
      rp <<- run_app_dev_bg(rp = rp, port = port, ...)
      Sys.sleep(2)
      suppressMessages(browse_url2(url = paste0("http://localhost:", port)))
      TRUE
    },
    hash = TRUE
  )
}

#' Helper to overwrite vscode browseURL behavior
#' @inheritDotParams base::browseURL
#' @noRd
browse_url2 <- function(...) {
  # need to overwrite vscode here until
  # https://github.com/maxheld83/wama/issues/6
  if (Sys.getenv("TERM_PROGRAM") == "vscode") {
    withr::local_options(list(vsc.browser = FALSE))
  }
  utils::browseURL(...)
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
run_app_dev <- function(...) {
  sb <- cli::cli_status("{cli::symbol$arrow_right} Loading package")
  pkgload::load_all(quiet = TRUE)
  cli::cli_status_update(id = sb, "{cli::symbol$arrow_right} Launching app")
  shiny::runApp(
    appDir = getOption("wama.default.app", default = "."),
    quiet = FALSE,
    ...
  )
}

#' Placeholder app for testing
#' @export
greeting_app <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::textInput("name", "What's your names"),
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

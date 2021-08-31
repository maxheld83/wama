#' Refresh shiny app automatically
#' @inheritParams shiny::runApp
#' @inheritDotParams shiny::runApp
#' @export
run_app_auto <- function(port = 8181,
                         host = getOption("shiny.host", "127.0.0.1"),
                         ...) {
  cli::cli_alert_info("Auto-updating enabled")
  rp <- run_app_dev_bg(port = port, host = host, ...)
  on.exit(rp$kill(), add = TRUE)
  # browse URL from within background processes does not work
  browse_when_ready(host = host, port = port)
  testthat::watch(
    path = fs::dir_ls(recurse = TRUE),
    callback = function(added, deleted, modified) {
      cli::cli_alert_info("Changes detected")
      rp <<- run_app_dev_bg(rp = rp, port = port, ...)
      # browse URL from within background processes does not work
      browse_when_ready(host = host, port = port)
      TRUE
    },
    hash = TRUE
  )
}

#' Find default shiny app
#'
#' Wama uses the first non-empty return of the below as your default shiny app:
#'
#' 1. The `Config/wama/defaultShinyApp` field of the `DESCRIPTION` in the working directory.
#'
#'    To set this for your package, add this to your `DESCRIPTION`:
#'
#'    ```
#'    Config/wama/defaultShinyApp: myApp()
#'    ```
#'
#' 1. The `wama.default.shiny.app` option.
#'
#'    This is best used interactively (`options(wama.default.app = myApp())`)
#'    or set in your `.Rprofile`.
#'
#' @export
find_default_app <- function() {
  app <- desc::desc_get_field("Config/wama/defaultShinyApp", default = NULL)
  if (!is.null(app)) {
    app <- eval(rlang::parse_expr(app))
  } else {
    app <- getOption("wama.default.shiny.app", default = NULL)
  }
  app
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
  utils::browseURL(
    ...,
    # do not take focus on opening
    browser = "/usr/bin/open -g"
  )
}

#' Wait for shiny server to be up, then browse.
#' @noRd
#' @inheritDotParams base::browseURL
browse_when_ready <- function(host, port) {
  cli::cli_process_start("Waiting for server to accept queries")
  sp <- cli::make_spinner()
  while (!pingr::is_up(destination = host, port = port)) {
    sp$spin()
  }
  sp$finish()
  cli::cli_process_done()
  cli::cli_process_start("Launching browser")
  suppressMessages(browse_url2(url = paste0("http://", host, ":", port)))
}

#' Run shiny app in dev context and background
#' @param rp
#' R background process, running the shiny session from a previous run.
#' Pass `NULL` (default) if there is no previous run.
#' @inheritDotParams shiny::runApp
#' @noRd
run_app_dev_bg <- function(rp = NULL,
                           ...) {
  if (!is.null(rp)) {
    cli::cli_process_start("Shutting down prior background process")
    rp$kill()
    cli::cli_process_done()
  }
  cli::cli_process_start("Starting new background process")
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
  cli::cli_process_start(msg = "Loading package")
  pkgload::load_all(quiet = TRUE)
  cli::cli_process_done()
  cli::cli_process_start("Launching app")
  shiny::runApp(
    appDir = find_default_app(),
    quiet = FALSE,
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

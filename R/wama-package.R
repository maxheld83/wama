#' Automating loading workflows during interactive development
#'
#' @section Package options:
#'
#' Wama uses the following following [options()] to configure behaviour:
#'
#' - `wama.default.app`:
#'     The default shiny application to run in the development context
#'     of some package.
#'
#'     If your package includes only *one* shiny application,
#'     you can conveniently set declare this to be used in your `zzz.R`
#'     (the filename is a convention):
#'
#'     ```r
#'     .onLoad <- function(libname, pkgname) options(wama.default.app = myapp())
#'     ```
#'     When your package is loaded *last* (as is typically the case in development),
#'     all wama functions will default to this default app of your package.
#'
#'     If you have more than one app, you can always change the default app
#'     interactively using `options(wama.default.app = some_app())`.
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

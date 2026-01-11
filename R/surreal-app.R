#' Launch the Surreal Shiny App
#'
#' Opens an interactive Shiny application for exploring the surreal algorithm.
#' The app allows you to generate datasets with hidden images in residual plots
#' using demo data, custom text, or uploaded images.
#'
#' @param launch.browser Logical. If `TRUE` (default), opens the app in the
#'   default web browser. If `FALSE`, returns the app URL for manual opening.
#' @param port Integer. The port to run the app on. If `NULL` (default),
#'   Shiny will choose an available port.
#' @param host Character. The host address. Default is `"127.0.0.1"` (localhost).
#'
#' @return This function is called for its side effect of launching the Shiny
#'   app. It does not return a value.
#'
#' @details
#' The app provides:
#' \itemize{
#'   \item Demo datasets (Jack-o-Lantern, R Logo)
#'   \item Custom text input to embed messages in residual plots
#'   \item Image upload support (PNG, JPEG, BMP, TIFF, SVG)
#'   \item Interactive controls for R², predictors, and image processing settings
#'   \item Dark/light mode toggle
#'   \item Data export to CSV
#' }
#'
#' @section Requirements:
#' The app requires the \pkg{shiny} and \pkg{bslib} packages to be installed.
#' For image uploads, additional packages may be needed depending on the format:
#' \itemize{
#'   \item JPEG: \pkg{jpeg}
#'   \item BMP: \pkg{bmp}
#'   \item TIFF: \pkg{tiff}
#'   \item SVG: \pkg{rsvg}
#' }
#'
#' @examples
#' \dontrun{
#' # Launch the app in the default browser
#' surreal_app()
#'
#' # Launch on a specific port
#' surreal_app(port = 3838)
#'
#' # Get the app without launching browser
#' surreal_app(launch.browser = FALSE)
#' }
#'
#' @seealso
#' [surreal()] for the core algorithm.
#' [surreal_text()] for embedding text programmatically.
#' [surreal_image()] for processing images programmatically.
#'
#' @export
surreal_app <- function(launch.browser = TRUE, port = NULL, host = "127.0.0.1") {
  require_packages(c("shiny", "bslib"))

  app_dir <- system.file("surreal-app", package = "surreal")

  if (app_dir == "") {
    cli::cli_abort(c(
      "Could not find the Surreal app directory.",
      "i" = "Try reinstalling the {.pkg surreal} package."
    ))
  }

  shiny::runApp(
    appDir = app_dir,
    launch.browser = launch.browser,
    port = port,
    host = host
  )
}

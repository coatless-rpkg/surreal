#' Verify suggested packages are available
#'
#' Checks if the required packages are available. If not, an error message is
#' thrown.
#'
#' @param packages Character vector of package names
#'
#' @return
#' Stops with an error message if any of the required packages are missing.
#' Otherwise, returns `TRUE` invisibly.
#'
#' @keywords internal
require_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Required package{?s} not installed: {.pkg {missing}}",
      "i" = "Install with: {.code install.packages(c({paste0('\"', missing, '\"', collapse = ', ')}))}"
    ))
  }

  invisible(TRUE)
}

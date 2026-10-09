#' Record the Search Behind a Surreal Dataset
#'
#' This function runs the same search as [`surreal()`] and keeps what it had at
#' every iteration, so the search can be plotted or played back. The data it
#' ends on is the data [`surreal()`] returns for the same seed.
#'
#' @inheritParams surreal
#' @param step Numeric. The fraction of its proposed update that each
#'   iteration takes, from above 0 to 1. The default of 1 is the step
#'   [`surreal()`] takes. A smaller step slows the search down and gives more
#'   iterations to watch.
#'
#' @return
#' An object of class `surreal_trace`, a list with:
#' \describe{
#'   \item{data}{The data frame the search ended on.}
#'   \item{iterations}{A data frame with a row for each iteration: its number,
#'     the `change` it proposed (what `tolerance` is compared with) and the
#'     `distance` of the fitted values from their targets.}
#'   \item{fitted}{A matrix with a column of fitted values for each iteration.}
#'   \item{residuals}{The residuals, which are the same at every iteration.}
#'   \item{target}{The fitted values the search is aiming for.}
#'   \item{step}{The step that was used.}
#'   \item{converged}{Whether the search stopped because the change fell below
#'     `tolerance`, rather than running out of iterations.}
#' }
#'
#' @details
#' The search starts from random predictors. The picture's vertical positions,
#' the residuals, are exact from the first iteration: the predictors are built
#' to be unrelated to them. The search moves the fitted values, the picture's
#' horizontal positions. Each iteration rebuilds one predictor so
#' that the fitted values land on their targets, which shifts the model
#' slightly, so the next iteration corrects again. With the default step this
#' settles in a handful of iterations.
#'
#' Plotting the fitted values of an iteration against the residuals shows the
#' picture as it stood then, which [`plot()`][plot.surreal_trace] does.
#'
#' @examples
#' set.seed(114)
#' trace <- surreal_trace(r_logo_image_data)
#' trace
#'
#' # The picture at the first iteration, and where the search ended
#' oldpar <- par(mfrow = c(1, 2))
#' plot(trace, iteration = 1)
#' plot(trace)
#' par(oldpar)
#'
#' # The distance of the fitted values from their targets, by iteration
#' plot(trace, type = "trace")
#'
#' # A smaller step gives a longer search to watch
#' set.seed(114)
#' slow <- surreal_trace(r_logo_image_data, step = 0.25)
#' nrow(slow$iterations)
#'
#' @seealso
#' [`surreal()`] for the method itself.
#'
#' @export
surreal_trace <- function(
    data,
    y_hat = data[, 1],
    R_0 = data[, 2],
    R_squared = 0.3, p = 5,
    n_add_points = 40,
    max_iter = 100, tolerance = 0.01,
    step = 1) {

  # Input validation
  check_settings(R_squared, p, n_add_points, max_iter, tolerance)
  if (!is.numeric(step) || length(step) != 1 || step <= 0 || step > 1) {
    cli::cli_abort(
      "{.arg step} must be a number above 0 and no more than 1 (got {.val {step}})."
    )
  }

  # Check if data is provided and extract y_hat and R_0
  if (!missing(data) && (is.data.frame(data) | is.matrix(data)) && ncol(data) == 2) {
    y_hat <- as.vector(data[, 1])
    R_0 <- as.vector(data[, 2])
  }

  check_lengths(y_hat, R_0)

  # Frame the picture and center its residuals
  picture <- frame_picture(y_hat, R_0, n_add_points)

  # Run the core algorithm, keeping each iteration
  found <- find_X_y_core(
    picture$y_hat, picture$R_0, R_squared = R_squared, p = p,
    max_iter = max_iter, tolerance = tolerance,
    step = step, record = TRUE)

  change <- found$trace$change

  structure(
    list(
      data = data.frame(y = found$y, X = found$X),
      iterations = data.frame(
        iteration = seq_along(change),
        change = change,
        distance = found$trace$distance
      ),
      fitted = found$trace$fitted,
      residuals = picture$R_0,
      target = found$trace$target,
      step = step,
      converged = change[length(change)] < tolerance
    ),
    class = "surreal_trace"
  )
}

#' @export
print.surreal_trace <- function(x, ...) {
  iterations <- nrow(x$iterations)
  distance <- signif(x$iterations$distance[c(1, iterations)], 3)

  cat("<surreal_trace>\n")
  cat(
    iterations, if (iterations == 1) "iteration" else "iterations",
    "with a step of", paste0(x$step, ","),
    if (x$converged) "converged" else "stopped at the limit", "\n"
  )
  cat(
    "Fitted values began", distance[1], "from their targets and ended",
    distance[2], "from them\n"
  )

  invisible(x)
}

#' Plot a Recorded Search
#'
#' Draws the picture as it stood at one iteration of a search recorded by
#' [`surreal_trace()`], or the path the search took.
#'
#' @param x         A `surreal_trace` object.
#' @param iteration Integer. The iteration to draw, or to mark on the path.
#'   Default is the last one.
#' @param type      Character. `"picture"` (default) plots the fitted values of
#'   the iteration against the residuals. `"trace"` plots the distance of the
#'   fitted values from their targets at every iteration, on a log scale.
#' @param ...       Further arguments passed to [`plot()`][graphics::plot].
#'
#' @return
#' `x`, invisibly. Called for the plot it draws.
#'
#' @examples
#' set.seed(114)
#' trace <- surreal_trace(r_logo_image_data)
#'
#' plot(trace, iteration = 2)
#' plot(trace, type = "trace")
#'
#' @export
plot.surreal_trace <- function(x, iteration = nrow(x$iterations), type = c("picture", "trace"), ...) {
  type <- match.arg(type)
  iterations <- nrow(x$iterations)

  if (!is.numeric(iteration) || length(iteration) != 1 || !iteration %in% seq_len(iterations)) {
    cli::cli_abort(
      "{.arg iteration} must be a whole number from 1 to {iterations} (got {.val {iteration}})."
    )
  }

  defaults <- if (type == "picture") {
    list(
      x = x$fitted[, iteration], y = x$residuals, pch = 16,
      xlab = "Fitted", ylab = "Residuals",
      main = paste("Iteration", iteration)
    )
  } else {
    list(
      x = x$iterations$iteration, y = x$iterations$distance,
      type = "b", pch = 16, log = "y",
      xlab = "Iteration", ylab = "Distance from the targets",
      main = "Path of the search"
    )
  }
  do.call(plot, utils::modifyList(defaults, list(...)))

  if (type == "trace") {
    graphics::abline(v = iteration, lty = 2)
  }

  invisible(x)
}

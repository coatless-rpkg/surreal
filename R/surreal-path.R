#' Select Predictors One Step at a Time
#'
#' This function runs forward selection on a dataset made by [`surreal()`] and
#' keeps the model that stood at every step, so the selection can be plotted
#' or played back. It starts with no predictors and adds, at each step, the
#' one that explains most of what is left.
#'
#' @param data      A data frame from [`surreal()`], [`surreal_text()`],
#'   [`surreal_image()`] or [`surreal_decoys()`], with a response named `y`.
#' @param criterion Character. The score that picks the best step, `"BIC"`
#'   (default) or `"AIC"`. Lower is better.
#'
#' @return
#' An object of class `surreal_path`, a list with:
#' \describe{
#'   \item{steps}{A data frame with a row for each step, from 0 (no
#'     predictors) to the number of predictors: the predictor that `entered`,
#'     the `criterion` of the model and its `r_squared`.}
#'   \item{coefficients}{A matrix with a row of coefficients for each step.
#'     A predictor that has not entered yet has a coefficient of 0.}
#'   \item{fitted, residuals}{Matrices with a column for each step.}
#'   \item{criterion}{The criterion that was used.}
#'   \item{best}{The step with the lowest criterion.}
#'   \item{decoys}{The names of the decoy predictors, when `data` came from
#'     [`surreal_decoys()`].}
#' }
#' The rows of `coefficients` and the columns of `fitted` and `residuals` are
#' named by step, so `path$residuals[, "5"]` holds the residuals at step 5.
#'
#' @details
#' On data straight from [`surreal()`] every predictor is real, so the
#' criterion falls at each step and the hidden image appears at the last one.
#' With decoys from [`surreal_decoys()`] the criterion is lowest at the model
#' of the real predictors, where the image is clear, and rises as decoys
#' enter and blur it.
#'
#' The criterion is computed as [`BIC()`][stats::BIC] and
#' [`AIC()`][stats::AIC] compute it for a linear model.
#'
#' @examples
#' set.seed(114)
#' hidden <- surreal(r_logo_image_data)
#' decoyed <- surreal_decoys(hidden, n = 20)
#'
#' path <- surreal_path(decoyed)
#' path
#'
#' # The coefficient paths, the criterion and the residuals at the best step
#' plot(path)
#'
#' # One step too early
#' plot(path, step = path$best - 1)
#'
#' @seealso
#' [`surreal_decoys()`] to add the decoys that make this a real search.
#'
#' @export
surreal_path <- function(data, criterion = c("BIC", "AIC")) {
  check_hidden_data(data)
  criterion <- match.arg(criterion)

  y <- data$y
  X <- as.matrix(data[setdiff(names(data), "y")])
  n <- nrow(X)
  p <- ncol(X)
  penalty <- if (criterion == "BIC") log(n) else 2
  total <- sum((y - mean(y))^2)

  coefficients <- matrix(
    0, p + 1, p + 1,
    dimnames = list(0:p, c("(Intercept)", colnames(X)))
  )
  fitted <- residuals <- matrix(NA_real_, n, p + 1, dimnames = list(NULL, 0:p))
  score <- r_squared <- numeric(p + 1)
  entered <- integer()

  for (k in 0:p) {
    # The model with the predictors that have entered so far
    design <- cbind(1, X[, entered, drop = FALSE])
    fit <- stats::lm.fit(design, y)
    rss <- sum(fit$residuals^2)

    coefficients[k + 1, c(1, entered + 1)] <- fit$coefficients
    residuals[, k + 1] <- fit$residuals
    fitted[, k + 1] <- y - fit$residuals
    r_squared[k + 1] <- 1 - rss / total
    # -2 log-likelihood plus the penalty on k slopes, an intercept and sigma
    score[k + 1] <- n * (log(2 * pi) + 1 + log(rss / n)) + penalty * (k + 2)

    if (k == p) break

    # The predictor that explains most of what is left, once those already
    # in the model are accounted for
    waiting <- setdiff(seq_len(p), entered)
    rest <- stats::lm.fit(design, X[, waiting, drop = FALSE])$residuals
    rest <- matrix(rest, nrow = n)
    gain <- drop(crossprod(rest, fit$residuals))^2 / colSums(rest^2)
    gain[!is.finite(gain)] <- -Inf
    entered <- c(entered, waiting[which.max(gain)])
  }

  structure(
    list(
      steps = data.frame(
        step = 0:p,
        entered = c(NA_character_, colnames(X)[entered]),
        criterion = score,
        r_squared = r_squared
      ),
      coefficients = coefficients,
      fitted = fitted,
      residuals = residuals,
      criterion = criterion,
      best = which.min(score) - 1,
      decoys = attr(data, "decoys")
    ),
    class = "surreal_path"
  )
}

#' Say what part each predictor plays at a step of a selection
#'
#' @param x    A `surreal_path` object.
#' @param step Integer. The step to look at.
#'
#' @return A character vector named by predictor: `"real"` for a predictor in
#'   the model at the step, `"decoy"` for one in the model that is known to be
#'   a decoy, and `"out"` for one that has not entered yet.
#'
#' @noRd
path_states <- function(x, step) {
  predictors <- colnames(x$coefficients)[-1]
  entered <- x$steps$entered[seq_len(step) + 1]

  states <- ifelse(predictors %in% x$decoys, "decoy", "real")
  states[!predictors %in% entered] <- "out"

  stats::setNames(states, predictors)
}

#' Work out what each step of a selection explained
#'
#' @param x A `surreal_path` object.
#'
#' @return A list with `gain`, the share of the variation left before each
#'   step that the step's predictor explained, and `charge`, the least a step
#'   has to explain for the path's criterion to fall.
#'
#' @noRd
path_gains <- function(x) {
  n <- nrow(x$residuals)
  rss <- colSums(x$residuals^2)
  penalty <- if (x$criterion == "BIC") log(n) else 2

  list(
    gain = unname(1 - rss[-1] / rss[-length(rss)]),
    charge = 1 - exp(-penalty / n)
  )
}

#' @export
print.surreal_path <- function(x, ...) {
  predictors <- nrow(x$steps) - 1
  chosen <- x$steps$entered[seq_len(x$best) + 1]

  cat("<surreal_path>\n")
  cat(
    "Forward selection over", predictors,
    if (predictors == 1) "predictor" else "predictors", "by", x$criterion, "\n"
  )
  cat(x$criterion, "is lowest at step", x$best, "\n")
  if (length(chosen) > 0) {
    cat("In the model at that step:", paste(chosen, collapse = ", "), "\n")
  }

  invisible(x)
}

#' Plot a Recorded Selection
#'
#' Draws one step of a selection recorded by [`surreal_path()`], as a row of
#' panels: the share each step explained, the criterion along the path, and
#' the residual plot of the model at the step.
#'
#' @param x      A `surreal_path` object.
#' @param step   Integer. The step to draw, from 0 to the number of predictors.
#'   Default is the best step.
#' @param panels Character. The panels to draw, in order, from `"explained"`,
#'   `"coefficients"`, `"criterion"` and `"residuals"`. Default is all but
#'   `"coefficients"`. Four panels are drawn two to a row.
#' @param ...    Further arguments passed to [`plot()`][graphics::plot] for the
#'   residual plot.
#'
#' @return
#' `x`, invisibly. Called for the plot it draws.
#'
#' @details
#' The panels are:
#'
#' - `"explained"`: a bar for each step, on a log scale, for the share of the
#'   variation left before the step that its predictor explained. A dotted
#'   line marks the criterion's charge for a predictor: the least a step has
#'   to explain for the criterion to fall. The bars above it are the steps
#'   that improved the model.
#' - `"coefficients"`: the coefficient of every predictor along the path.
#' - `"criterion"`: the criterion along the path.
#' - `"residuals"`: the residual plot of the model at the step.
#'
#' A solid violet line marks the step that is drawn, and a dashed green line
#' the best step. A predictor is blue once it is in the model at the step, and
#' gray until then. A decoy in the model is orange, when the path knows its
#' decoys.
#'
#' @examples
#' set.seed(114)
#' hidden <- surreal(r_logo_image_data)
#' path <- surreal_path(surreal_decoys(hidden, n = 20))
#'
#' plot(path)
#' plot(path, step = 25)
#'
#' # The coefficient paths, with the criterion beside them
#' plot(path, panels = c("coefficients", "criterion"))
#'
#' @export
plot.surreal_path <- function(
    x,
    step = x$best,
    panels = c("explained", "criterion", "residuals"),
    ...) {
  panels <- match.arg(
    panels,
    c("explained", "coefficients", "criterion", "residuals"),
    several.ok = TRUE
  )
  last <- nrow(x$steps) - 1

  if (!is.numeric(step) || length(step) != 1 || !step %in% 0:last) {
    cli::cli_abort(
      "{.arg step} must be a whole number from 0 to {last} (got {.val {step}})."
    )
  }

  rows <- if (length(panels) > 3) 2 else 1
  oldpar <- graphics::par(mfrow = c(rows, ceiling(length(panels) / rows)))
  on.exit(graphics::par(oldpar))

  colors <- c(real = "#2a78d6", decoy = "#eb6834", out = "gray70")
  step_color <- "#4a3aa7"
  best_color <- "#1baf7a"
  parts <- if (is.null(x$decoys)) {
    c(real = "in the model", out = "not in the model")
  } else {
    c(real = "real, in the model", decoy = "decoy, in the model", out = "not in the model")
  }
  states <- path_states(x, step)

  # A key sits a little inside its corner, on the plot's own background, so
  # it covers the lines under it without touching the frame
  background <- graphics::par("bg")
  if (background == "transparent") background <- "white"
  key <- function(corner, ...) {
    graphics::legend(
      corner, ..., cex = 0.8, inset = c(0.03, 0.04),
      box.col = NA, bg = background
    )
  }

  # The step that is drawn in violet, then the best step over it in green
  # dashes, so that both show when they are the same step
  mark <- function(shift = 0) {
    graphics::abline(v = step + shift, lwd = 2, col = step_color)
    graphics::abline(v = x$best + shift, lty = 2, lwd = 2, col = best_color)
  }

  draw_explained <- function() {
    found <- path_gains(x)
    gain <- pmax(found$gain, .Machine$double.eps)
    # Each bar takes the color of the predictor that entered at its step
    entered <- states[x$steps$entered[-1]]
    # A log scale from under the smallest bar to room above the tallest for
    # the key
    low <- min(gain, found$charge) / 2
    high <- max(gain, found$charge)
    ylim <- c(low, high * (high / low)^0.45)

    # Percentages read across, so the axis title stands further out
    mar <- graphics::par(mar = graphics::par("mar") + c(0, 1.6, 0, 0))
    on.exit(graphics::par(mar))
    plot(
      NA, xlim = c(0.4, last + 0.6), ylim = ylim, log = "y", yaxt = "n",
      xlab = "Step", ylab = "", main = "Explained at each step"
    )
    # No share is above 100%, so the room for the key carries no labels
    ticks <- 10^seq(ceiling(log10(low)), 0)
    labels <- paste0(format(100 * ticks, scientific = FALSE, drop0trailing = TRUE, trim = TRUE), "%")
    graphics::axis(2, at = ticks, labels = labels, las = 1)
    graphics::title(ylab = "Share of the remaining variation", line = 4.4)
    graphics::rect(
      seq_len(last) - 0.35, low / 10, seq_len(last) + 0.35, gain,
      col = colors[entered], border = NA
    )
    graphics::abline(h = found$charge, lty = 3, lwd = 2)
    # The lines fall between the bars of the steps they divide
    mark(shift = 0.5)
    graphics::box()
    key(
      "topright",
      legend = c(parts, paste0(x$criterion, "'s charge for a predictor")),
      col = c(colors[names(parts)], graphics::par("fg")),
      pch = c(rep(15, length(parts)), NA), pt.cex = 1.5,
      lty = c(rep(NA, length(parts)), 3), lwd = c(rep(NA, length(parts)), 2)
    )
  }

  draw_coefficients <- function() {
    slopes <- x$coefficients[, -1, drop = FALSE]
    # Room above the paths for the key
    ylim <- range(slopes, na.rm = TRUE)
    ylim[2] <- ylim[2] + 0.35 * diff(ylim)
    graphics::matplot(
      0:last, slopes,
      type = "s", lty = 1, ylim = ylim, col = colors[states],
      xlab = "Step", ylab = "Coefficient", main = "Coefficient paths"
    )
    mark()
    key("topleft", legend = parts, col = colors[names(parts)], lty = 1, lwd = 2)
  }

  draw_criterion <- function() {
    plot(
      x$steps$step, x$steps$criterion,
      type = "l",
      xlab = "Step", ylab = x$criterion, main = x$criterion
    )
    mark()
    key(
      "topright", legend = c("this step", paste("lowest", x$criterion)),
      col = c(step_color, best_color), lty = c(1, 2), lwd = 2
    )
  }

  # With no predictors in the model every fitted value is the same, give or
  # take rounding, so the axis gets a unit of room on each side
  draw_residuals <- function(...) {
    fitted <- x$fitted[, step + 1]
    xlim <- range(fitted)
    if (diff(xlim) < 1e-8 * max(1, abs(xlim))) {
      xlim <- mean(xlim) + c(-1, 1)
    }
    defaults <- list(
      x = fitted, y = x$residuals[, step + 1], pch = 16, xlim = xlim,
      xlab = "Fitted", ylab = "Residuals", main = paste("Step", step)
    )
    do.call(plot, utils::modifyList(defaults, list(...)))
  }

  for (panel in panels) {
    switch(
      panel,
      explained = draw_explained(),
      coefficients = draw_coefficients(),
      criterion = draw_criterion(),
      residuals = draw_residuals(...)
    )
  }

  invisible(x)
}

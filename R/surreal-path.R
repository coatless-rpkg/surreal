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
#' Draws three panels for one step of a selection recorded by
#' [`surreal_path()`]: the coefficient of every predictor along the path, the
#' criterion along the path, and the residual plot of the model at the step.
#'
#' @param x    A `surreal_path` object.
#' @param step Integer. The step to draw, from 0 to the number of predictors.
#'   Default is the best step.
#' @param ...  Further arguments passed to [`plot()`][graphics::plot] for the
#'   residual plot.
#'
#' @return
#' `x`, invisibly. Called for the plot it draws.
#'
#' @details
#' A solid violet line marks the step that is drawn, and a dashed green line
#' the best step.
#' A path is blue once its predictor is in the model at the step, and gray
#' until then. A decoy in the model is orange, when the path knows its decoys.
#'
#' @examples
#' set.seed(114)
#' hidden <- surreal(r_logo_image_data)
#' path <- surreal_path(surreal_decoys(hidden, n = 20))
#'
#' plot(path)
#' plot(path, step = 25)
#'
#' @export
plot.surreal_path <- function(x, step = x$best, ...) {
  last <- nrow(x$steps) - 1

  if (!is.numeric(step) || length(step) != 1 || !step %in% 0:last) {
    cli::cli_abort(
      "{.arg step} must be a whole number from 0 to {last} (got {.val {step}})."
    )
  }

  oldpar <- graphics::par(mfrow = c(1, 3))
  on.exit(graphics::par(oldpar))

  # The step that is drawn in violet, then the best step over it in green
  # dashes, so that both show when they are the same step
  step_color <- "#4a3aa7"
  best_color <- "#1baf7a"
  background <- graphics::par("bg")
  if (background == "transparent") background <- "white"
  mark <- function() {
    graphics::abline(v = step, lwd = 2, col = step_color)
    graphics::abline(v = x$best, lty = 2, lwd = 2, col = best_color)
  }

  # Coefficient of every predictor along the path, colored by the part the
  # predictor plays at the step
  colors <- c(real = "#2a78d6", decoy = "#eb6834", out = "gray70")
  key <- if (is.null(x$decoys)) {
    c(real = "in the model", out = "not in the model")
  } else {
    c(real = "real, in the model", decoy = "decoy, in the model", out = "not in the model")
  }
  slopes <- x$coefficients[, -1, drop = FALSE]
  # Room above the paths for the key
  ylim <- range(slopes, na.rm = TRUE)
  ylim[2] <- ylim[2] + 0.3 * diff(ylim)
  graphics::matplot(
    0:last, slopes,
    type = "s", lty = 1, ylim = ylim,
    col = colors[path_states(x, step)],
    xlab = "Step", ylab = "Coefficient", main = "Coefficient paths"
  )
  mark()
  # The key is drawn on the plot's own background, over the lines that mark
  # the steps
  graphics::legend(
    "topleft", legend = key, col = colors[names(key)],
    lty = 1, lwd = 2, cex = 0.8, box.col = NA, bg = background
  )

  # The criterion along the path
  plot(
    x$steps$step, x$steps$criterion,
    type = "l",
    xlab = "Step", ylab = x$criterion, main = x$criterion
  )
  mark()
  graphics::legend(
    "topright", legend = c("this step", paste("lowest", x$criterion)),
    col = c(step_color, best_color),
    lty = c(1, 2), lwd = 2, cex = 0.8, box.col = NA, bg = background
  )

  # The residual plot at the step. With no predictors in the model every
  # fitted value is the same, give or take rounding, so the axis gets a unit
  # of room on each side.
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

  invisible(x)
}

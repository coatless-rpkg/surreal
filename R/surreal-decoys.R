#' Check a dataset made by the surreal method
#'
#' The functions that work on a finished dataset need a data frame with a
#' numeric response named `y` and at least one numeric predictor. An error is
#' reported as coming from the function the user called, which is `call`.
#'
#' @param data The dataset to check.
#' @param call The environment of the function to name in an error.
#'
#' @return `NULL`, invisibly. Called for its errors.
#'
#' @noRd
check_hidden_data <- function(data, call = parent.frame()) {
  if (!is.data.frame(data)) {
    cli::cli_abort(
      c(
        "{.arg data} must be a data frame.",
        "i" = "Use the data that {.fn surreal}, {.fn surreal_text} or {.fn surreal_image} returns."
      ),
      call = call
    )
  }

  numeric <- vapply(data, is.numeric, logical(1))
  if (!"y" %in% names(data) || ncol(data) < 2 || !all(numeric)) {
    cli::cli_abort(
      c(
        "{.arg data} must have a response named {.field y} and at least one predictor, all numeric.",
        "i" = "Use the data that {.fn surreal}, {.fn surreal_text} or {.fn surreal_image} returns."
      ),
      call = call
    )
  }

  invisible()
}

#' Add Decoy Predictors to a Surreal Dataset
#'
#' This function adds predictors of pure noise to a dataset made by
#' [`surreal()`]. The hidden image then shows only in the residuals of the
#' model with the real predictors: leave some out and it is not there yet,
#' and let decoys in and it blurs. Finding it becomes a variable selection
#' problem, which [`surreal_path()`] works through step by step.
#'
#' @param data    A data frame from [`surreal()`], [`surreal_text()`] or
#'   [`surreal_image()`], with a response named `y`.
#' @param n       Integer. Number of decoy predictors to add. Default is 30.
#' @param sd      Numeric or `NULL`. Standard deviation of the decoys. If `NULL`
#'   (default), the average standard deviation of the predictors in `data`.
#' @param shuffle Logical. If `TRUE` (default), the decoys are mixed in among
#'   the real predictors and every predictor is renamed `X.1`, `X.2`, ..., so
#'   that neither position nor name gives a decoy away. If `FALSE`, the decoys
#'   are added after the real predictors as `D.1`, `D.2`, ....
#'
#' @return
#' The data frame with `n` more columns. The names of the decoys are kept in
#' its `"decoys"` attribute, which is the answer key. The attribute is not
#' written out by [`write.csv()`].
#'
#' @examples
#' set.seed(114)
#' hidden <- surreal(r_logo_image_data)
#'
#' # Mix in 30 decoys
#' decoyed <- surreal_decoys(hidden)
#' names(decoyed)
#'
#' # The answer key
#' attr(decoyed, "decoys")
#'
#' # The model with every predictor no longer shows a clean image
#' model <- lm(y ~ ., data = decoyed)
#' plot(model$fitted, model$resid, pch = 16)
#'
#' @seealso
#' [`surreal_path()`] to select the predictors one step at a time.
#'
#' @export
surreal_decoys <- function(data, n = 30, sd = NULL, shuffle = TRUE) {
  check_hidden_data(data)

  if (!is.numeric(n) || length(n) != 1 || n < 1 || n != round(n)) {
    cli::cli_abort(
      "{.arg n} must be a whole number of at least 1 (got {.val {n}})."
    )
  }

  predictors <- setdiff(names(data), "y")

  if (is.null(sd)) {
    sd <- mean(vapply(data[predictors], stats::sd, numeric(1)))
  } else if (!is.numeric(sd) || length(sd) != 1 || sd <= 0) {
    cli::cli_abort(
      "{.arg sd} must be a positive number, or NULL to match the predictors (got {.val {sd}})."
    )
  }

  # Draw the decoys and put them after the real predictors
  decoys <- paste0("D.", seq_len(n))
  noise <- matrix(
    stats::rnorm(nrow(data) * n, sd = sd),
    ncol = n, dimnames = list(NULL, decoys)
  )
  result <- cbind(data, noise)

  # Mix them in and rename every predictor, so nothing tells them apart
  if (shuffle) {
    mixed <- sample(c(predictors, decoys))
    result <- result[c("y", mixed)]

    renamed <- paste0("X.", seq_along(mixed))
    names(result) <- c("y", renamed)
    decoys <- renamed[mixed %in% decoys]
  }

  attr(result, "decoys") <- decoys

  result
}

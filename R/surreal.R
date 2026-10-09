#' Transform Data by Adding a Border
#'
#' This function transforms the input data by adding points around the original data
#' to create a frame. It uses an optimization process to find the best alpha parameter
#' for point distribution, which helps in making the fitted values and residuals orthogonal.
#'
#' @param x            Numeric vector of x coordinates.
#' @param y            Numeric vector of y coordinates.
#' @param n_add_points Integer. Number of points to add on each side of the frame. Default is `40`.
#' @param verbose      Logical. If `TRUE`, prints optimization progress. Default is `FALSE`.
#'
#' @return
#' A matrix with two columns representing the transformed `x` and `y` coordinates.
#'
#' @export
#' @examples
#' # Simulate data
#' x <- rnorm(100)
#' y <- rnorm(100)
#'
#' # Append border to data
#' transformed_data <- border_augmentation(x, y)
#'
#' # Modify par settings for plotting side-by-side
#' oldpar <- par(mfrow = c(1, 2))
#'
#' # Graph original and transformed data
#' plot(x, y, pch = 16, main = "Original data")
#' plot(
#'   transformed_data[, 1], transformed_data[, 2], pch = 16,
#'   main = "Transformed data", xlab = 'x', ylab = 'y'
#' )
#'
#' # Restore original par settings
#' par(oldpar)
#' @importFrom stats optimize lm coef
border_augmentation <- function(x, y, n_add_points = 40, verbose = FALSE) {
  # Define constants for frame size and shift
  FRAME <- 0.05
  SHIFT <- 1 + 2 * FRAME

  # Helper function to calculate range and delta for x and y
  calculate_range <- function(values) {
    range <- range(values)
    delta <- diff(range)
    list(
      range = c(range[1] - FRAME * delta, range[2] + FRAME * delta),
      delta = delta
    )
  }

  # Calculate ranges and deltas for x and y
  x_data <- calculate_range(x)
  y_data <- calculate_range(y)

  # Helper function to generate points based on alpha
  generate_points <- function(alpha, range, delta) {
    # Create sequence of points based on alpha value
    pkt <- if (alpha <= 1) seq(0, 1, length.out = n_add_points)^alpha
    else 1 - seq(0, 1, length.out = n_add_points)^(2 - alpha)
    list(
      lower = range[1] + SHIFT * delta * pkt,
      upper = range[2] - SHIFT * delta * pkt
    )
  }

  # Helper function to combine original and generated points
  combine_points <- function(alpha) {
    x_points <- generate_points(alpha, x_data$range, x_data$delta)
    y_points <- generate_points(alpha, y_data$range, y_data$delta)

    xx <- c(x, x_points$lower, rep(x_data$range[2], n_add_points),
            x_points$upper, rep(x_data$range[1], n_add_points))
    yy <- c(y, rep(y_data$range[1], n_add_points), y_points$upper,
            rep(y_data$range[2], n_add_points), y_points$lower)

    list(xx = xx, yy = yy)
  }

  # Optimization function to find best alpha
  optimize_alpha <- function(alpha) {
    points <- combine_points(alpha)
    abs(stats::coef(stats::lm(points$yy ~ points$xx))[2])
  }

  # Find optimal alpha using optimization
  optimal_alpha <- stats::optimize(optimize_alpha, lower = 0, upper = 2)$minimum
  if (verbose) cat("Optimal alpha:", optimal_alpha, "\n")

  # Generate final points using optimal alpha
  final_points <- combine_points(optimal_alpha)
  cbind(final_points$xx, final_points$yy)
}


#' Core Algorithm for Finding X and Y
#'
#' This function implements the core algorithm for finding X and y in the
#' Residual (Sur)Realism method. It's called by [`surreal()`] and
#' [`surreal_trace()`] after performing the border transformation.
#'
#' @inheritParams surreal
#' @param step   Numeric. The fraction of its proposed update that each
#'   iteration takes, from above 0 to 1. The default of 1 takes it whole.
#' @param record Logical. If TRUE, the state at each iteration is kept.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{X}{The generated X matrix}
#'   \item{y}{The generated y vector}
#' }
#' When `record` is TRUE, a third element `trace` holds the target for the
#' fitted values and, for each iteration, the fitted values, their distance
#' from the target and the size of the proposed update.
#'
#' @importFrom stats rnorm sd lm
#' @noRd
find_X_y_core <- function(y_hat, R_0, R_squared = 0.3, p = 5, max_iter = 100, tolerance = 0.01, verbose = FALSE,
                          step = 1, record = FALSE) {
  n <- length(R_0)

  # Scale y_hat to achieve desired R-squared
  y_hat <- sd(R_0) / sd(y_hat) * sqrt(R_squared / (1 - R_squared)) * y_hat

  # Initialize parameters
  beta <- c(1, seq_len(p))  # beta_0 and beta_{1:p} combined
  j_star <- p + 1  # Adjusting for 1-based indexing in R

  # Generate random noise
  Z <- rnorm(n, sd = sd(R_0))
  M <- matrix(rnorm(n * p, sd = sd(y_hat)), n, p)

  # The projection onto R_0 is applied to vectors as they come, so the
  # n-by-n matrix behind it is never built
  R_0_ss <- sum(R_0^2)
  remove_R_0 <- function(V) V - R_0 %*% (crossprod(R_0, V) / R_0_ss)

  fitted <- change <- distance <- NULL

  # Iterative optimization
  for (i in seq_len(max_iter)) {
    X <- remove_R_0(M)
    W <- cbind(1, X)
    A_M_Z <- W %*% solve(crossprod(W), crossprod(W, Z))

    if (record) {
      # The fitted values of the full model on the data as it stands
      fitted_i <- beta[1] + X %*% beta[-1] + A_M_Z
      fitted <- cbind(fitted, fitted_i)
      distance <- c(distance, sqrt(mean((fitted_i - y_hat)^2)))
    }

    SUM_beta_M_all <- M %*% beta[-1]  # Exclude beta_0
    P_R_0_M_beta <- R_0 * (sum(R_0 * SUM_beta_M_all) / R_0_ss)
    FIRST <- y_hat - beta[1] - A_M_Z + P_R_0_M_beta - SUM_beta_M_all

    M_new <- M
    M_new[, j_star - 1] <- (FIRST + beta[j_star] * M[, j_star - 1]) / beta[j_star]

    delta <- sum((M_new - M)^2)

    if (verbose) {
      cat("Iteration", i, "- Delta:", delta, "\n")
    }

    if (record) change <- c(change, delta)

    if (delta < tolerance) break

    M <- if (step == 1) M_new else M + step * (M_new - M)
  }

  # Calculate final X and Y
  eps <- R_0 + A_M_Z
  X <- remove_R_0(M)
  Y <- beta[1] + X %*% beta[-1] + eps

  result <- list(y = Y, X = X)
  if (record) {
    result$trace <- list(
      target = y_hat, fitted = unname(fitted),
      change = change, distance = distance
    )
  }

  result
}

#' Check the settings of the surreal method
#'
#' Stops with the message [`surreal()`] has always given, reported as coming
#' from the function that was called.
#'
#' @inheritParams surreal
#'
#' @return `NULL`, invisibly. Called for its errors.
#'
#' @noRd
check_settings <- function(R_squared, p, n_add_points, max_iter, tolerance) {
  call <- sys.call(-1)
  fail <- function(...) stop(simpleError(paste0(...), call = call))

  if (R_squared <= 0 || R_squared >= 1) {
    fail("`R_squared` must be between 0 and 1 (supplied: ", R_squared ,")")
  }
  if (p < 1) {
    fail("`p` must be at least 1 (supplied: ", p ,")")
  }
  if (n_add_points < 0) {
    fail("`n_add_points` must be a non-negative integer (supplied: ", n_add_points ,")")
  }
  if (max_iter < 1) {
    fail("`max_iter` must be at least 1 (supplied: ", max_iter ,")")
  }
  if (tolerance <= 0) {
    fail("`tolerance` must be a positive number (supplied: ", tolerance ,")")
  }

  invisible()
}

#' Check that the two halves of a picture match in length
#'
#' @inheritParams surreal
#'
#' @return `NULL`, invisibly. Called for its error.
#'
#' @noRd
check_lengths <- function(y_hat, R_0) {
  if (length(y_hat) != length(R_0)) {
    message <- paste0(
      "`y_hat` and `R_0` must have the same length. (",
      length(y_hat), "!= ", length(R_0), ")"
    )
    stop(simpleError(message, call = sys.call(-1)))
  }

  invisible()
}

#' Frame a picture and center its residuals
#'
#' @inheritParams surreal
#'
#' @return A list with the `y_hat` and `R_0` the core algorithm works from.
#'
#' @noRd
frame_picture <- function(y_hat, R_0, n_add_points, verbose = FALSE) {
  # Apply bordering to data if n_add_points > 0
  if (n_add_points > 0) {
    xy <- border_augmentation(y_hat, R_0, n_add_points = n_add_points, verbose = verbose)
  } else {
    xy <- cbind(y_hat, R_0)
  }

  list(y_hat = xy[, 1], R_0 = xy[, 2] - mean(xy[, 2]))
}

#' Find X Matrix and Y Vector for Residual Surrealism
#'
#' This function implements the Residual (Sur)Realism algorithm as described by
#' Leonard A. Stefanski (2007). It finds a matrix X and vector y such that the
#' fitted values and residuals of lm(y ~ X) are similar to the inputs y_hat and R_0.
#'
#' @param data         A data frame or matrix with two columns representing the `y_hat` and `R_0` values.
#' @param y_hat        Numeric vector of desired fitted values (only used if `data` is not provided).
#' @param R_0          Numeric vector of desired residuals (only used if `data` is not provided).
#' @param R_squared    Numeric. Desired R-squared value. Default is 0.3.
#' @param p            Integer. Desired number of columns for matrix X. Default is 5.
#' @param n_add_points Integer. Number of points to add in border transformation. Default is 40.
#' @param max_iter     Integer. Maximum number of iterations for convergence. Default is 100.
#' @param tolerance    Numeric. Criteria for detecting convergence and stopping optimization early. Default is 0.01.
#' @param verbose      Logical. If TRUE, prints progress information. Default is FALSE.
#'
#' @return
#' A data frame containing the generated X matrix and y vector.
#'
#' @details
#' To disable the border augmentation, set `n_add_points = 0`.
#'
#' @importFrom stats rnorm sd
#' @importFrom graphics pairs
#' @export
#'
#' @examples
#' # Generate a 2D data set
#' data <- cbind(y_hat = rnorm(100), R_0 = rnorm(100))
#'
#' # Display original data
#' plot(data, pch = 16, main = "Original data")
#'
#' # Apply the surreal method
#' result <- surreal(data)
#'
#' # View the expanded data after transformation
#' pairs(y ~ ., data = result, main = "Data after transformation")
#'
#' # Fit a linear model to the transformed data
#' model <- lm(y ~ ., data = result)
#'
#' # Plot the residuals
#' plot(model$fitted, model$resid, type = "n", main = "Residual plot from transformed data")
#' points(model$fitted, model$resid, pch = 16)
#'
#' @references
#' Stefanski, L. A. (2007). Residual (Sur)Realism. The American Statistician, 61(2), 163-177.
surreal <- function(
    data,
    y_hat = data[, 1],
    R_0 = data[, 2],
    R_squared = 0.3, p = 5,
    n_add_points = 40,
    max_iter = 100, tolerance = 0.01, verbose = FALSE) {

  # Input validation
  check_settings(R_squared, p, n_add_points, max_iter, tolerance)

  # Check if data is provided and extract y_hat and R_0
  if (!missing(data) && (is.data.frame(data) | is.matrix(data)) && ncol(data) == 2) {
    y_hat <- as.vector(data[, 1])
    R_0 <- as.vector(data[, 2])
  }

  check_lengths(y_hat, R_0)

  # Plot original data if verbose
  if (verbose) {
    plot(y_hat, R_0, main = "Original data", xlab = '', ylab = '')
  }

  # Frame the picture and center its residuals
  picture <- frame_picture(y_hat, R_0, n_add_points, verbose = verbose)

  # Find X and y using core algorithm
  data <- find_X_y_core(
    picture$y_hat, picture$R_0, R_squared = R_squared, p = p,
    max_iter = max_iter, tolerance = tolerance, verbose = verbose)

  # Create result data frame
  result <- data.frame(y = data$y, X = data$X)

  # Plot transformed data and residuals if verbose
  if (verbose) {
    pairs(~ ., data = result, main = "Data after transformation")
    res <- lm(y ~ ., data = result)
    plot(res$fitted, res$residuals, main = "Reconstruction of data", xlab = '', ylab = '')
  }

  result
}

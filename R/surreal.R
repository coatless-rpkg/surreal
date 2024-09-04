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
#' x <- rnorm(100)
#' y <- rnorm(100)
#' transformed_data <- border_augmentation(x, y)
#'
#' par(mfrow = c(1, 2))
#' plot(x, y, pch = 16, main = "Original data")
#' plot(transformed_data[, 1], transformed_data[, 2], pch = 16, main = "Transformed data")
#'
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
#' Residual (Sur)Realism method. It's called by [`surreal()`] after
#' performing the border transformation.
#'
#' @inheritParams surreal
#'
#' @return A list with two elements:
#' \describe{
#'   \item{X}{The generated X matrix}
#'   \item{y}{The generated y vector}
#' }
#'
#' @importFrom stats rnorm sd lm
#' @noRd
find_X_y_core <- function(y_hat, R_0, R_squared = 0.3, p = 5, max_iter = 100, tolerance = 0.01, verbose = FALSE) {
  n <- length(R_0)

  # Scale y_hat to achieve desired R-squared
  y_hat <- sd(R_0) / sd(y_hat) * sqrt(R_squared / (1 - R_squared)) * y_hat

  # Initialize parameters
  beta <- c(1, seq_len(p))  # beta_0 and beta_{1:p} combined
  j_star <- p + 1  # Adjusting for 1-based indexing in R

  # Generate random noise
  Z <- rnorm(n, sd = sd(R_0))
  M <- matrix(rnorm(n * p, sd = sd(y_hat)), n, p)

  # Calculate projection matrix
  P_R_0 <- tcrossprod(R_0) / sum(R_0^2)

  # Iterative optimization
  for (i in seq_len(max_iter)) {
    W <- cbind(1, (diag(n) - P_R_0) %*% M)
    A_M <- W %*% solve(crossprod(W), t(W))

    SUM_beta_M_all <- M %*% beta[-1]  # Exclude beta_0
    FIRST <- y_hat - beta[1] - A_M %*% Z + P_R_0 %*% M %*% beta[-1] - SUM_beta_M_all

    M_new <- M
    M_new[, j_star - 1] <- (FIRST + beta[j_star] * M[, j_star - 1]) / beta[j_star]

    delta <- sum((M_new - M)^2)

    if (verbose) {
      cat("Iteration", i, "- Delta:", delta, "\n")
    }

    if (delta < tolerance) break

    M <- M_new
  }

  # Calculate final X and Y
  eps <- R_0 + A_M %*% Z
  X <- (diag(n) - P_R_0) %*% M
  Y <- beta[1] + X %*% beta[-1] + eps

  list(y = Y, X = X)
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
#' @param R_squared    Desired R-squared value. Default is 0.3.
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
  if (R_squared <= 0 || R_squared >= 1) {
    stop("`R_squared` must be between 0 and 1 (supplied: ", R_squared ,")")
  }
  if (p < 1) {
    stop("`p` must be at least 1 (supplied: ", p ,")")
  }
  if (n_add_points < 0) {
    stop("`n_add_points` must be a non-negative integer (supplied: ", n_add_points ,")")
  }
  if (max_iter < 1) {
    stop("`max_iter` must be at least 1 (supplied: ", max_iter ,")")
  }
  if (tolerance <= 0) {
    stop("`tolerance` must be a positive number (supplied: ", tolerance ,")")
  }

  # Check if data is provided and extract y_hat and R_0
  if (!missing(data) && (is.data.frame(data) | is.matrix(data)) && ncol(data) == 2) {
    y_hat <- as.vector(data[, 1])
    R_0 <- as.vector(data[, 2])
  }

  if (length(y_hat) != length(R_0)) {
    stop("`y_hat` and `R_0` must have the same length. (", length(y_hat) ,"!= ", length(R_0) ,")")
  }

  # Plot original data if verbose
  if (verbose) {
    plot(y_hat, R_0, main = "Original data", xlab = '', ylab = '')
  }

  # Apply bordering to data if n_add_points > 0
  if (n_add_points > 0) {
    xy <- border_augmentation(y_hat, R_0, n_add_points = n_add_points, verbose = verbose)
  } else {
    xy <- cbind(y_hat, R_0)
  }

  # Extract transformed y_hat and R_0
  y_hat <- xy[, 1]
  R_0 <- xy[, 2] - mean(xy[, 2])

  # Find X and y using core algorithm
  data <- find_X_y_core(
    y_hat, R_0, R_squared = R_squared, p = p,
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

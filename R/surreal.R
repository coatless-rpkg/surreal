#' Transform Data for X and Y Finding Algorithm by Adding a Border
#'
#' This function transforms the input data by adding points around the original data
#' to create a frame. It uses an optimization process to find the best alpha parameter
#' for point distribution.
#'
#' @param x            Numeric vector of x coordinates
#' @param y            Numeric vector of y coordinates
#' @param n_add_points Integer. Number of points to add on each side of the frame. Default is 40.
#' @param verbose      Logical. If TRUE, prints optimization progress. Default is FALSE.
#'
#' @return
#' A matrix with two columns representing the transformed x and y coordinates.
#'
#' @noRd
#' @examples
#' x <- rnorm(100)
#' y <- rnorm(100)
#' transformed_data <- border_augmentation(x, y)
#'
#' @importFrom stats optimize lm coef
border_augmentation <- function(x, y, n_add_points = 40, verbose = FALSE) {
  frame <- 0.05
  shift <- 1 + 2 * frame

  x_range <- range(x)
  y_range <- range(y)
  x_delta <- diff(x_range)
  y_delta <- diff(y_range)

  x_range <- c(x_range[1] - frame * x_delta, x_range[2] + frame * x_delta)
  y_range <- c(y_range[1] - frame * y_delta, y_range[2] + frame * y_delta)

  optimize_alpha <- function(alpha) {
    pkt <- if (alpha <= 1) seq(0, 1, length.out = n_add_points)^alpha
    else 1 - seq(0, 1, length.out = n_add_points)^(2 - alpha)

    xu_pkt <- x_range[1] + shift * x_delta * pkt
    yu_pkt <- y_range[1] + shift * y_delta * pkt
    xo_pkt <- x_range[2] - shift * x_delta * pkt
    yo_pkt <- y_range[2] - shift * y_delta * pkt

    xx <- c(x, xu_pkt, rep(x_range[2], n_add_points), xo_pkt, rep(x_range[1], n_add_points))
    yy <- c(y, rep(y_range[1], n_add_points), yo_pkt, rep(y_range[2], n_add_points), yu_pkt)

    abs(coef(lm(yy ~ xx))[2])
  }

  optimal_alpha <- optimize(optimize_alpha, lower = 0, upper = 2)$minimum

  if (verbose) cat("Optimal alpha:", optimal_alpha, "\n")

  pkt <- if (optimal_alpha <= 1) seq(0, 1, length.out = n_add_points)^optimal_alpha
  else 1 - seq(0, 1, length.out = n_add_points)^(2 - optimal_alpha)

  xu_pkt <- x_range[1] + shift * x_delta * pkt
  yu_pkt <- y_range[1] + shift * y_delta * pkt
  xo_pkt <- x_range[2] - shift * x_delta * pkt
  yo_pkt <- y_range[2] - shift * y_delta * pkt

  xx <- c(x, xu_pkt, rep(x_range[2], n_add_points), xo_pkt, rep(x_range[1], n_add_points))
  yy <- c(y, rep(y_range[1], n_add_points), yo_pkt, rep(y_range[2], n_add_points), yu_pkt)

  cbind(xx, yy)
}

#' Find X Matrix and Y Vector for Residual Surrealism
#'
#' This function implements the Residual (Sur)Realism algorithm as described by
#' Leonard A. Stefanski (2007). It finds a matrix X and vector y such that the
#' fitted values and residuals of lm(y ~ X) are similar to the inputs y_hat and R_0.
#'
#' @param data         A data frame or matrix with two columns representing the `y_hat` and `R_0` values.
#' @param y_hat        Numeric vector of desired fitted values, or a matrix/data.frame with two columns
#' @param R_0          Numeric vector of desired residuals (only used if y_hat is a vector)
#' @param R_squared    Desired R-squared value. Default is 0.3.
#' @param p            Integer. Desired number of columns for matrix X. Default is 5.
#' @param n_add_points Integer. Number of points to add in data transformation. Default is 40.
#' @param max_iter     Integer. Maximum number of iterations for convergence. Default is 20.
#' @param verbose      Logical. If TRUE, prints progress information. Default is FALSE.
#'
#' @return
#' A data frame with containing the generated X matrix and y vector.
#'
#' @importFrom stats rnorm sd
#' @importFrom graphics pairs
#' @export
#' @examples
#' # Generate a 2D data set
#' data <- cbind(y = rnorm(100), x = rnorm(100))
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
    R_0 = data[, 2],
    y_hat = data[, 1],
    R_squared = 0.3, p = 5,
    n_add_points = 40, max_iter = 20, verbose = FALSE) {

  if ((is.data.frame(data) | is.matrix(data)) && ncol(data) == 2) {
    R_0 <- as.vector(data[, 2])
    y_hat <- as.vector(data[, 1])
  }

  # Apply bordering to data
  xy <- border_augmentation(y_hat, R_0, n_add_points = n_add_points, verbose = verbose)

  y_hat <- xy[, 1]
  R_0 <- xy[, 2] - mean(xy[, 2])

  # Find X and y
  data <- find_X_y_core(
    y_hat, R_0, R_squared = R_squared, p = p,
    max_iter = max_iter, verbose = verbose)

  # Re-do the data frame
  data.frame(y = data$y, X = data$X)
}

#' Core Algorithm for Finding X and Y
#'
#' This function implements the core algorithm for finding X and y in the
#' Residual (Sur)Realism method. It's called by find_X_y after data transformation.
#'
#' @inheritParams surreal
#'
#' @return
#' A list with two elements:
#'
#' \describe{
#'   \item{X}{The generated X matrix}
#'   \item{y}{The generated y vector}
#' }
#'
#' @importFrom stats rnorm sd lm
#' @noRd
find_X_y_core <- function(y_hat, R_0, R_squared = 0.3, p = 5, max_iter = 20, verbose = FALSE) {
  n <- length(R_0)
  y_hat <- sd(R_0) / sd(y_hat) * sqrt(R_squared / (1 - R_squared)) * y_hat

  beta_0 <- 0
  beta_p <- 1:p
  j_star <- p

  Z <- rnorm(n, sd = sd(R_0))
  M <- matrix(rnorm(n * p, sd = sd(y_hat)), n, p)

  P_R_0 <- R_0 %*% t(R_0) / (R_0 %*% R_0)[1]
  M_jstar_old <- M[, j_star]

  for (i in 1:max_iter) {
    W <- cbind(1, (diag(n) - P_R_0) %*% M)
    A_M <- W %*% solve(t(W) %*% W) %*% t(W)
    SUM_beta_M_all <- M %*% beta_p
    FIRST <- y_hat - beta_0 - A_M %*% Z + P_R_0 %*% M %*% beta_p - SUM_beta_M_all
    M[, j_star] <- 1 / beta_p[j_star] * (FIRST + beta_p[j_star] * M[, j_star])

    h <- M[, j_star]
    h_delta_sum <- sum((h - M_jstar_old)^2)

    if (verbose) print(h_delta_sum)
    if (h_delta_sum < 0.01) break

    M_jstar_old <- h
  }

  eps <- R_0 + A_M %*% Z
  X <- (diag(n) - P_R_0) %*% M
  Y <- beta_0 + X %*% beta_p + eps

  if (verbose) {
    pairs(X)
    res <- lm(Y ~ X)
    plot(res$fitted, res$residuals, type = "p", main = "Reconstruction of data")
  }

  list(y = Y, X = X)
}

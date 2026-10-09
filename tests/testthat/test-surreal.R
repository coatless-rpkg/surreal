test_that("border_augmentation() keeps the points and frames them on every side", {
  withr::local_seed(42)
  x <- runif(60, 0, 10)
  y <- rnorm(60)

  framed <- border_augmentation(x, y, n_add_points = 10)

  expect_equal(dim(framed), c(60 + 4 * 10, 2))
  expect_equal(framed[1:60, ], cbind(x, y), ignore_attr = TRUE)
  expect_equal(range(framed[, 1]), range(x) + c(-0.05, 0.05) * diff(range(x)))
  expect_equal(range(framed[, 2]), range(y) + c(-0.05, 0.05) * diff(range(y)))
})

test_that("border_augmentation() leaves no linear trend in the framed points", {
  withr::local_seed(42)
  x <- runif(60, 0, 10)
  y <- rnorm(60)

  framed <- border_augmentation(x, y, n_add_points = 10)
  slope <- coef(lm(framed[, 2] ~ framed[, 1]))[[2]]

  expect_lt(abs(slope), 1e-3)
})

test_that("border_augmentation() reports the alpha it settled on when verbose", {
  expect_output(
    border_augmentation(1:20, rev(1:20), n_add_points = 5, verbose = TRUE),
    "Optimal alpha"
  )
})

test_that("surreal() returns the response and p predictors for each point and its frame", {
  withr::local_seed(1)

  hidden <- surreal(
    y_hat = runif(60),
    R_0 = rnorm(60),
    p = 3,
    n_add_points = 10
  )

  expect_s3_class(hidden, "data.frame")
  expect_named(hidden, c("y", "X.1", "X.2", "X.3"))
  expect_equal(nrow(hidden), 60 + 4 * 10)
})

test_that("surreal() leaves R_0 as the residuals of the full model", {
  withr::local_seed(1)
  y_hat <- runif(200, 0, 10)
  R_0 <- rnorm(200)

  hidden <- surreal(y_hat = y_hat, R_0 = R_0, n_add_points = 0)
  fit <- lm(y ~ ., data = hidden)

  expect_equal(unname(residuals(fit)), R_0 - mean(R_0))
})

test_that("surreal() leaves the framed R_0 as the residuals when a frame is added", {
  withr::local_seed(1)
  y_hat <- runif(200, 0, 10)
  R_0 <- rnorm(200)
  framed <- border_augmentation(y_hat, R_0, n_add_points = 10)

  hidden <- surreal(y_hat = y_hat, R_0 = R_0, n_add_points = 10)
  fit <- lm(y ~ ., data = hidden)

  expect_equal(unname(residuals(fit)), framed[, 2] - mean(framed[, 2]))
})

test_that("surreal() puts y_hat in the fitted values of the full model", {
  withr::local_seed(1)
  y_hat <- runif(200, 0, 10)
  R_0 <- rnorm(200)

  hidden <- surreal(y_hat = y_hat, R_0 = R_0, n_add_points = 0)
  fit <- lm(y ~ ., data = hidden)

  expect_gt(cor(fitted(fit), y_hat), 0.99)
})

test_that("surreal() fits with close to the R-squared that was asked for", {
  withr::local_seed(1)
  y_hat <- runif(200, 0, 10)
  R_0 <- rnorm(200)

  hidden <- surreal(y_hat = y_hat, R_0 = R_0, R_squared = 0.6)
  fit <- lm(y ~ ., data = hidden)

  expect_equal(summary(fit)$r.squared, 0.6, tolerance = 0.05)
})

test_that("surreal() names a single predictor X", {
  withr::local_seed(1)

  hidden <- surreal(y_hat = runif(30), R_0 = rnorm(30), p = 1)

  expect_named(hidden, c("y", "X"))
})

test_that("surreal() gives the same data for the same seed", {
  picture <- cbind(seq(0, 10, length.out = 50), sin(1:50))

  first <- withr::with_seed(7, surreal(picture))
  second <- withr::with_seed(7, surreal(picture))

  expect_equal(first, second)
})

test_that("surreal() reads a matrix, a data frame and two vectors alike", {
  picture <- cbind(seq(0, 10, length.out = 50), sin(1:50))

  from_matrix <- withr::with_seed(7, surreal(picture))
  from_frame <- withr::with_seed(7, surreal(as.data.frame(picture)))
  from_vectors <- withr::with_seed(
    7,
    surreal(y_hat = picture[, 1], R_0 = picture[, 2])
  )

  expect_equal(from_frame, from_matrix)
  expect_equal(from_vectors, from_matrix)
})

test_that("surreal() reproduces its recorded data for a fixed seed", {
  y_hat <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  R_0 <- c(0.5, -1.2, 0.3, 2.1, -0.7, 0.9, -1.5, 0.2, 1.1, -0.4, -0.9, 0.6)

  hidden <- withr::with_seed(
    2007,
    surreal(y_hat = y_hat, R_0 = R_0, p = 2, n_add_points = 0)
  )

  expect_snapshot_value(round(hidden, 6), style = "json2", tolerance = 1e-6)
})

test_that("surreal() rejects settings outside their ranges", {
  picture <- cbind(1:10, 10:1)

  expect_snapshot(error = TRUE, {
    surreal(picture, R_squared = 1)
    surreal(picture, p = 0)
    surreal(picture, n_add_points = -1)
    surreal(picture, max_iter = 0)
    surreal(picture, tolerance = 0)
    surreal(y_hat = 1:3, R_0 = 1:4)
  })
})

test_that("surreal() reports each iteration when verbose", {
  withr::local_pdf(NULL)
  withr::local_seed(1)

  expect_output(
    surreal(y_hat = runif(30), R_0 = rnorm(30), verbose = TRUE),
    "Iteration 1 - Delta"
  )
})

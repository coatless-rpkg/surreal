test_that("surreal_trace() ends on the data surreal() returns for the same seed", {
  direct <- withr::with_seed(114, surreal(r_logo_image_data))
  trace <- withr::with_seed(114, surreal_trace(r_logo_image_data))

  expect_s3_class(trace, "surreal_trace")
  expect_equal(trace$data, direct)
})

test_that("surreal_trace() records each iteration of the search", {
  trace <- withr::with_seed(114, surreal_trace(r_logo_image_data))
  iterations <- nrow(trace$iterations)

  expect_named(trace$iterations, c("iteration", "change", "distance"))
  expect_equal(trace$iterations$iteration, seq_len(iterations))
  expect_equal(dim(trace$fitted), c(nrow(trace$data), iterations))
  expect_length(trace$residuals, nrow(trace$data))
  expect_length(trace$target, nrow(trace$data))
})

test_that("surreal_trace() has the picture in the residuals from the first iteration", {
  framed <- border_augmentation(r_logo_image_data$x, r_logo_image_data$y)

  trace <- withr::with_seed(114, surreal_trace(r_logo_image_data))

  expect_equal(trace$residuals, framed[, 2] - mean(framed[, 2]))
})

test_that("surreal_trace() moves the fitted values onto their targets", {
  trace <- withr::with_seed(114, surreal_trace(r_logo_image_data))
  distance <- trace$iterations$distance
  last <- length(distance)

  expect_lt(distance[last], distance[1] / 1000)
  expect_equal(trace$fitted[, last], trace$target, tolerance = 1e-3)
  expect_identical(trace$converged, TRUE)
})

test_that("surreal_trace() ends on the fitted values of the full model", {
  trace <- withr::with_seed(114, surreal_trace(r_logo_image_data))
  fit <- lm(y ~ ., data = trace$data)

  expect_equal(trace$fitted[, ncol(trace$fitted)], unname(fitted(fit)))
})

test_that("surreal_trace() takes more iterations with a smaller step", {
  whole <- withr::with_seed(114, surreal_trace(r_logo_image_data))
  slow <- withr::with_seed(114, surreal_trace(r_logo_image_data, step = 0.25))

  expect_gt(nrow(slow$iterations), 3 * nrow(whole$iterations))
  expect_equal(slow$fitted[, 1], whole$fitted[, 1])
  expect_identical(slow$converged, TRUE)
})

test_that("surreal_trace() says when the search ran out of iterations", {
  trace <- withr::with_seed(
    114,
    surreal_trace(r_logo_image_data, step = 0.25, max_iter = 5)
  )

  expect_equal(nrow(trace$iterations), 5)
  expect_identical(trace$converged, FALSE)
})

test_that("surreal_trace() rejects settings outside their ranges", {
  expect_snapshot(error = TRUE, {
    surreal_trace(r_logo_image_data, step = 0)
    surreal_trace(r_logo_image_data, step = 1.5)
    surreal_trace(r_logo_image_data, R_squared = 1)
    surreal_trace(y_hat = 1:3, R_0 = 1:4)
  })
})

test_that("print() says how the search went", {
  trace <- withr::with_seed(114, surreal_trace(r_logo_image_data))

  expect_output(print(trace), "4 iterations")
  expect_output(print(trace), "converged")
  expect_invisible(print(trace))
})

test_that("plot() draws the picture at an iteration and the path of the search", {
  withr::local_pdf(NULL)
  trace <- withr::with_seed(114, surreal_trace(r_logo_image_data))

  expect_no_error(plot(trace))
  expect_no_error(plot(trace, iteration = 1))
  expect_no_error(plot(trace, type = "trace"))
})

test_that("plot() rejects an iteration that was not run", {
  withr::local_pdf(NULL)
  trace <- withr::with_seed(114, surreal_trace(r_logo_image_data))

  expect_snapshot(error = TRUE, plot(trace, iteration = 99))
})

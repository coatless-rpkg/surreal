test_that("surreal_path() records a step for the empty model and one for each predictor", {
  decoyed <- decoyed_logo()

  path <- surreal_path(decoyed)

  expect_s3_class(path, "surreal_path")
  expect_named(path$steps, c("step", "entered", "criterion", "r_squared"))
  expect_equal(path$steps$step, 0:25)
  expect_identical(path$steps$entered[1], NA_character_)
  expect_setequal(path$steps$entered[-1], names(decoyed)[-1])
})

test_that("surreal_path() scores the first and last steps as R scores those models", {
  decoyed <- decoyed_logo()

  by_bic <- surreal_path(decoyed)
  by_aic <- surreal_path(decoyed, criterion = "AIC")

  expect_equal(by_bic$steps$criterion[1], BIC(lm(y ~ 1, data = decoyed)))
  expect_equal(by_bic$steps$criterion[26], BIC(lm(y ~ ., data = decoyed)))
  expect_equal(by_aic$steps$criterion[26], AIC(lm(y ~ ., data = decoyed)))
  expect_equal(by_aic$criterion, "AIC")
})

test_that("surreal_path() holds the fit of the model that stood at each step", {
  decoyed <- decoyed_logo()

  path <- surreal_path(decoyed)
  entered <- path$steps$entered[2:4]
  fit <- lm(reformulate(entered, "y"), data = decoyed)
  waiting <- setdiff(names(decoyed)[-1], entered)

  expect_equal(path$coefficients["3", c("(Intercept)", entered)], coef(fit))
  expect_equal(path$coefficients["3", waiting], rep(0, 22), ignore_attr = TRUE)
  expect_equal(path$residuals[, "3"], unname(residuals(fit)))
  expect_equal(path$fitted[, "3"], unname(fitted(fit)))
  expect_equal(path$steps$r_squared[4], summary(fit)$r.squared)
})

test_that("surreal_path() takes the predictor that helps most at each step", {
  decoyed <- decoyed_logo()

  path <- surreal_path(decoyed)
  first <- vapply(
    names(decoyed)[-1],
    function(name) {
      summary(lm(reformulate(name, "y"), data = decoyed))$r.squared
    },
    numeric(1)
  )

  expect_equal(path$steps$entered[2], names(which.max(first)))
  expect_gte(min(diff(path$steps$r_squared)), 0)
})

test_that("surreal_path() scores the model of the real predictors best", {
  hidden <- hidden_logo()
  decoyed <- decoyed_logo()

  path <- surreal_path(decoyed)

  expect_equal(path$best, 5)
  expect_setequal(path$steps$entered[2:6], names(hidden)[-1])
  expect_equal(
    path$residuals[, "5"],
    unname(residuals(lm(y ~ ., data = hidden)))
  )
})

test_that("surreal_path() blurs the picture once decoys are in the model", {
  decoyed <- decoyed_logo()

  path <- surreal_path(decoyed)
  blur <- sqrt(mean((path$residuals[, "25"] - path$residuals[, "5"])^2))

  expect_gt(blur, 0.05 * sd(path$residuals[, "5"]))
})

test_that("surreal_path() scores the full model best when there are no decoys", {
  path <- surreal_path(hidden_logo())

  expect_equal(path$best, 5)
  expect_null(path$decoys)
})

test_that("surreal_path() remembers which predictors are decoys", {
  path <- surreal_path(decoyed_logo())

  expect_equal(path$decoys, paste0("D.", 1:20))
})

test_that("surreal_path() rejects data it cannot use", {
  hidden <- hidden_logo()

  expect_snapshot(error = TRUE, {
    surreal_path(as.matrix(hidden))
    surreal_path(hidden["y"])
  })
})

test_that("surreal_path() takes only the criteria it knows", {
  expect_error(
    surreal_path(hidden_logo(), criterion = "Cp"),
    "should be one of"
  )
})

test_that("print() names the best step and what is in its model", {
  path <- surreal_path(decoyed_logo())

  expect_snapshot(print(path))
  expect_invisible(print(path))
})

test_that("plot() draws the paths, the criterion and the residuals at a step", {
  withr::local_pdf(NULL)
  path <- surreal_path(decoyed_logo())

  expect_no_error(plot(path))
  expect_no_error(plot(path, step = 0))
  expect_no_error(plot(path, step = 25))
})

test_that("plot() rejects a step that is not on the path", {
  withr::local_pdf(NULL)
  path <- surreal_path(decoyed_logo())

  expect_snapshot(error = TRUE, plot(path, step = 40))
})

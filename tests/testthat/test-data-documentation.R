test_that("r_logo_image_data holds the logo as x and y coordinates", {
  expect_s3_class(r_logo_image_data, "data.frame")
  expect_named(r_logo_image_data, c("x", "y"))
  expect_equal(nrow(r_logo_image_data), 2000)
})

test_that("jackolantern_surreal_data holds a response and six predictors", {
  expect_s3_class(jackolantern_surreal_data, "data.frame")
  expect_named(jackolantern_surreal_data, c("y", paste0("x", 1:6)))
  expect_equal(nrow(jackolantern_surreal_data), 5395)
})

test_that("jackolantern_surreal_data gives every predictor a coefficient", {
  fit <- lm(y ~ ., data = jackolantern_surreal_data)

  expect_length(coef(fit), 7)
  expect_equal(sum(is.na(coef(fit))), 0)
})

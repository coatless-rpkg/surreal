test_that("surreal_decoys() adds n predictors and leaves the response alone", {
  hidden <- hidden_logo()

  decoyed <- withr::with_seed(2, surreal_decoys(hidden, n = 10))

  expect_s3_class(decoyed, "data.frame")
  expect_equal(dim(decoyed), c(nrow(hidden), ncol(hidden) + 10))
  expect_equal(decoyed$y, hidden$y)
})

test_that("surreal_decoys() mixes the decoys in and gives every predictor a plain name", {
  hidden <- hidden_logo()

  decoyed <- withr::with_seed(2, surreal_decoys(hidden, n = 10))
  decoys <- attr(decoyed, "decoys")
  real <- decoyed[setdiff(names(decoyed), c("y", decoys))]

  expect_named(decoyed, c("y", paste0("X.", 1:15)))
  expect_length(decoys, 10)
  expect_in(decoys, names(decoyed))
  expect_equal(
    sort(colSums(real)),
    sort(colSums(hidden[-1])),
    ignore_attr = TRUE
  )
  expect_gt(sum(names(real) != names(hidden)[-1]), 0)
})

test_that("surreal_decoys() puts the decoys last, under their own names, without a shuffle", {
  hidden <- hidden_logo()

  decoyed <- withr::with_seed(
    2,
    surreal_decoys(hidden, n = 10, shuffle = FALSE)
  )

  expect_named(decoyed, c(names(hidden), paste0("D.", 1:10)))
  expect_equal(decoyed[names(hidden)], hidden, ignore_attr = TRUE)
  expect_equal(attr(decoyed, "decoys"), paste0("D.", 1:10))
})

test_that("surreal_decoys() gives the decoys the spread of the real predictors", {
  hidden <- hidden_logo()
  spread <- mean(vapply(hidden[-1], sd, numeric(1)))

  decoyed <- withr::with_seed(
    2,
    surreal_decoys(hidden, n = 50, shuffle = FALSE)
  )
  decoys <- unlist(decoyed[attr(decoyed, "decoys")])

  expect_equal(sd(decoys), spread, tolerance = 0.02)
  expect_equal(mean(decoys), 0, tolerance = 0.02 * spread)
})

test_that("surreal_decoys() gives the decoys the spread that is asked for", {
  decoyed <- withr::with_seed(
    2,
    surreal_decoys(hidden_logo(), n = 50, sd = 3, shuffle = FALSE)
  )
  decoys <- unlist(decoyed[attr(decoyed, "decoys")])

  expect_equal(sd(decoys), 3, tolerance = 0.02)
})

test_that("surreal_decoys() gives the same decoys for the same seed", {
  first <- withr::with_seed(2, surreal_decoys(hidden_logo(), n = 10))
  second <- withr::with_seed(2, surreal_decoys(hidden_logo(), n = 10))

  expect_identical(first, second)
})

test_that("surreal_decoys() rejects data and settings it cannot use", {
  hidden <- hidden_logo()

  expect_snapshot(error = TRUE, {
    surreal_decoys(as.matrix(hidden))
    surreal_decoys(hidden[-1])
    surreal_decoys(hidden, n = 0)
    surreal_decoys(hidden, n = 2.5)
    surreal_decoys(hidden, sd = -1)
  })
})

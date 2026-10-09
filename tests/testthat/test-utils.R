test_that("require_packages() is silent when every package is installed", {
  expect_invisible(require_packages(c("stats", "utils")))
  expect_identical(require_packages("stats"), TRUE)
})

test_that("require_packages() names the packages that are missing", {
  expect_snapshot(error = TRUE, {
    require_packages("notapackage1")
    require_packages(c("stats", "notapackage1", "notapackage2"))
  })
})

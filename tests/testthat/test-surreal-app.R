test_that("the Source pane is redrawn for each message that is generated", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  shiny::testServer(app_dir(), {
    session$setInputs(
      input_mode = "text",
      text = "A",
      r_squared = 0.3,
      p = 5,
      point_size = 0.6,
      max_points = 3000,
      image_mode = "auto",
      threshold = 0.5,
      dark_mode = "light"
    )
    session$setInputs(generate = 1)
    first <- output$compare_source$src

    session$setInputs(text = "B")
    session$setInputs(generate = 2)
    second <- output$compare_source$src

    expect_match(first, "^data:image/png")
    expect_length(unique(c(first, second)), 2)
  })
})

test_that("Undo brings back the message that went with the restored data", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  shiny::testServer(app_dir(), {
    session$setInputs(
      input_mode = "text",
      text = "A",
      r_squared = 0.3,
      p = 5,
      point_size = 0.6,
      max_points = 3000,
      image_mode = "auto",
      threshold = 0.5,
      dark_mode = "light"
    )
    session$setInputs(generate = 1)
    first <- output$compare_source$src

    session$setInputs(text = "B")
    session$setInputs(generate = 2)
    session$setInputs(undo = 1)

    expect_identical(output$compare_source$src, first)
  })
})

test_that("the Source download holds the message that was generated", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  shiny::testServer(app_dir(), {
    session$setInputs(
      input_mode = "text",
      text = "A",
      r_squared = 0.3,
      p = 5,
      point_size = 0.6,
      max_points = 3000,
      image_mode = "auto",
      threshold = 0.5,
      dark_mode = "light"
    )
    session$setInputs(generate = 1)
    generated <- readBin(output$download_source, "raw", 1e6)

    session$setInputs(text = "B")
    edited <- readBin(output$download_source, "raw", 1e6)

    expect_identical(edited, generated)
  })
})

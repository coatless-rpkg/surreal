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

test_that("every pane is drawn once data is generated", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  shiny::testServer(app_dir(), {
    do.call(session$setInputs, app_inputs(input_mode = "demo_rlogo"))
    session$setInputs(generate = 1)

    expect_match(output$compare_source$src, "^data:image/png")
    expect_match(output$compare_residual$src, "^data:image/png")
    expect_match(output$pairs_plot$src, "^data:image/png")
    expect_equal(output$obs_count, "2,160 points")
    expect_match(output$data_table, "X.5", fixed = TRUE)
    expect_match(output$coef_table, "(Intercept)", fixed = TRUE)
  })
})

test_that("dark mode redraws the plots in other colors", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  shiny::testServer(app_dir(), {
    do.call(session$setInputs, app_inputs(input_mode = "demo_rlogo"))
    session$setInputs(generate = 1)
    light <- c(output$compare_source$src, output$compare_residual$src)

    session$setInputs(dark_mode = "dark")
    dark <- c(output$compare_source$src, output$compare_residual$src)

    expect_length(unique(c(light, dark)), 4)
  })
})

test_that("the downloads hold the plots and the data that are on screen", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  shiny::testServer(app_dir(), {
    do.call(session$setInputs, app_inputs(input_mode = "demo_rlogo"))
    session$setInputs(generate = 1)

    residual <- readBin(output$download_residual, "raw", 8)
    saved <- read.csv(output$download)

    expect_identical(residual, png_signature())
    expect_equal(saved, rv$data, ignore_attr = TRUE)
  })
})

test_that("the Code dialog shows the call for the source that is chosen", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  shiny::testServer(app_dir(), {
    do.call(session$setInputs, app_inputs(input_mode = "text", text = "A"))
    expect_match(
      generate_code(),
      'surreal_text("A", R_squared = 0.30, p = 5)',
      fixed = TRUE
    )

    session$setInputs(input_mode = "demo_rlogo", p = 3)
    expect_match(
      generate_code(),
      "surreal(r_logo_image_data, R_squared = 0.30, p = 3)",
      fixed = TRUE
    )

    session$setInputs(input_mode = "demo_jack")
    expect_match(generate_code(), "jackolantern_surreal_data", fixed = TRUE)
  })
})

test_that("the Code dialog leaves the threshold to the package in auto mode", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  shiny::testServer(app_dir(), {
    do.call(session$setInputs, app_inputs(input_mode = "image"))
    expect_match(generate_code(), "threshold = NULL", fixed = TRUE)

    session$setInputs(image_mode = "dark", threshold = 0.4)
    expect_match(generate_code(), 'mode = "dark"', fixed = TRUE)
    expect_match(generate_code(), "threshold = 0.40", fixed = TRUE)
  })
})

test_that("the code shown for a message runs to that message", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- load_app()
  message <- 'Say "hi"\nnow'

  code <- app$example_code("text", app_inputs(text = message))
  made <- parse(text = code)[[2]]

  expect_equal(made[[3]][[2]], message)
})

test_that("the code shown for an image has no blank line inside the call", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- load_app()

  code <- app$example_code("image", app_inputs(input_mode = "image"))

  expect_no_match(code, "\n\n", fixed = TRUE)
})

test_that("with_seed() runs from a seed and leaves the random numbers as they were", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- load_app()
  withr::local_seed(1)
  next_number <- withr::with_seed(1, runif(1))

  seeded <- app$with_seed(99, runif(2))

  expect_equal(seeded, withr::with_seed(99, runif(2)))
  expect_equal(runif(1), next_number)
})

test_that("the Search and Selection tabs are offered when the package can fill them", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- load_app()

  page <- as.character(app$ui)

  expect_identical(app$steps_available(), TRUE)
  expect_match(page, 'data-value="Search"', fixed = TRUE)
  expect_match(page, 'data-value="Selection"', fixed = TRUE)
})

test_that("each preset sets every slider it shares with the others, the decoys among them", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- load_app()
  settings <- c("r_squared", "p", "point_size", "max_points", "decoys")

  expect_named(app$presets, c("fast", "balanced", "detail"))
  expect_named(app$presets$fast, settings)
  expect_named(app$presets$balanced, settings)
  expect_named(app$presets$detail, settings)
  expect_lt(app$presets$fast$decoys, app$presets$detail$decoys)
})

test_that("the number of decoys is set with the other inputs, not in the Selection tab", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- load_app()

  page <- as.character(app$ui)
  tab <- as.character(app$selection_tab())

  expect_length(gregexpr('id="decoys"', page, fixed = TRUE)[[1]], 1)
  expect_match(page, 'id="decoys"', fixed = TRUE)
  expect_no_match(tab, 'id="decoys"', fixed = TRUE)
})

test_that("axis_labels() writes numbers in full, with commas in the thousands", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- load_app()

  expect_equal(app$axis_labels(c(100, 1, 0.01)), c("100", "1", "0.01"))
  expect_equal(app$axis_labels(c(47000, 48500)), c("47,000", "48,500"))
  expect_equal(app$axis_labels(c(-2, 0, 2.5)), c("-2", "0", "2.5"))
})

test_that("a frame whose fitted values are all but equal is drawn without complaint", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  withr::local_pdf(NULL)
  app <- load_app()
  # A spread this small is what the first step of a selection can have, and
  # it is in the narrow band that makes R's axis warn
  flat <- 27.00666 + c(0, 1, 1 / 2, 1 / 3, 0, 1) * 1.14e-13

  expect_no_warning(
    app$draw_frame(flat, c(-2, -1, 0, 1, 2, 3), app$plot_palette(FALSE), 0.6)
  )
})

test_that("the Search and Selection tabs describe themselves without question words", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- load_app()
  question_words <- "\\b(how|what|why|where|when|which|who)\\b"

  words <- gsub("<[^>]+>", " ", paste(app$search_tab(), app$selection_tab()))

  expect_no_match(words, question_words, ignore.case = TRUE)
})

test_that("the search starts at a step below 1, with the reason behind an information icon", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- load_app()

  tab <- as.character(app$search_tab())

  expect_match(tab, 'id="search_step"[^>]*data-from="0.25"')
  expect_match(tab, "<bslib-tooltip", fixed = TRUE)
  expect_match(tab, "circle-info", fixed = TRUE)
  expect_match(tab, "takes a step of 1", fixed = TRUE)
})

test_that("the search that is shown ends on the data that was generated", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  shiny::testServer(app_dir(), {
    do.call(session$setInputs, app_inputs(input_mode = "text", text = "Hi"))
    session$setInputs(generate = 1)

    expect_equal(trace()$data, rv$data)
    expect_match(output$search_picture$src, "^data:image/png")
    expect_match(output$search_path$src, "^data:image/png")
  })
})

test_that("a smaller step gives the search more iterations to show", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  shiny::testServer(app_dir(), {
    do.call(session$setInputs, app_inputs(input_mode = "demo_rlogo"))
    session$setInputs(generate = 1)
    whole <- nrow(trace()$iterations)

    session$setInputs(search_step = 0.25)

    expect_gt(nrow(trace()$iterations), 3 * whole)
  })
})

test_that("Undo brings back the search that went with the restored data", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  shiny::testServer(app_dir(), {
    do.call(session$setInputs, app_inputs(input_mode = "text", text = "Hi"))
    session$setInputs(generate = 1)
    first <- rv$data
    session$setInputs(text = "Ho", p = 3)
    session$setInputs(generate = 2)

    session$setInputs(undo = 1)

    expect_equal(rv$data, first)
    expect_equal(trace()$data, first)
  })
})

test_that("there is no search to show for the ready-made data", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  shiny::testServer(app_dir(), {
    do.call(session$setInputs, app_inputs(input_mode = "demo_jack"))
    session$setInputs(generate = 1)

    expect_error(trace(), "ready-made")
  })
})

test_that("the selection scores the model of the real predictors best among decoys", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  # The app draws a new seed for each dataset. With this one, no decoy
  # happens to improve the criterion, which about one draw in ten does.
  withr::local_seed(2007)

  shiny::testServer(app_dir(), {
    do.call(session$setInputs, app_inputs(input_mode = "demo_rlogo"))
    session$setInputs(generate = 1)

    expect_equal(nrow(path()$steps), 5 + 20 + 1)
    expect_equal(path()$best, 5)
    expect_match(output$selection_residual$src, "^data:image/png")
    expect_match(output$selection_paths$src, "^data:image/png")
    expect_match(output$selection_criterion$src, "^data:image/png")
  })
})

test_that("the selection runs over the real predictors alone when no decoys are asked for", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  shiny::testServer(app_dir(), {
    do.call(session$setInputs, app_inputs(input_mode = "demo_jack", decoys = 0))
    session$setInputs(generate = 1)

    expect_equal(path()$steps$step, 0:6)
    expect_null(path()$decoys)
  })
})

test_that("download buttons drop the download attribute when R runs in the browser", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  app <- load_app()
  in_browser <- app$download_button("save", "Save", in_browser = TRUE)
  installed <- app$download_button("save", "Save", in_browser = FALSE)

  expect_null(in_browser$attribs[["download"]])
  expect_identical(installed$attribs[["download"]], NA)
})

test_that("surreal_app() runs the app that comes with the package", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  local_mocked_bindings(runApp = function(...) list(...), .package = "shiny")

  started <- surreal_app(launch.browser = FALSE, port = 4321)

  expect_equal(started$appDir, app_dir())
  expect_identical(started$launch.browser, FALSE)
  expect_equal(started$port, 4321)
  expect_equal(started$host, "127.0.0.1")
})

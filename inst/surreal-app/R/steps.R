# The two tabs that show the method one step at a time. "Search" is how the
# method finds the data. "Selection" is how an analyst finds the image in it.

# These tabs call functions that were added after surreal 0.0.2, so they are
# offered only when the installed package has them
steps_available <- function() {
  needed <- c(
    "surreal_text_points",
    "surreal_image_points",
    "surreal_trace",
    "surreal_decoys",
    "surreal_path"
  )
  all(needed %in% getNamespaceExports("surreal"))
}

# Run code from a seed, then put the random number stream back as it was
with_seed <- function(seed, code) {
  has_stream <- exists(".Random.seed", globalenv(), inherits = FALSE)
  stream <- if (has_stream) get(".Random.seed", globalenv())
  on.exit(
    if (has_stream) {
      assign(".Random.seed", stream, globalenv())
    } else {
      rm(".Random.seed", envir = globalenv())
    }
  )

  set.seed(seed)
  code
}

# The line of explanation at the top of a tab
tab_note <- function(...) {
  div(
    class = "mb-2 ps-2 py-1 small text-body-secondary border-start border-primary border-3 bg-body-secondary rounded-end",
    ...
  )
}

# A plot in a card, with what it shows above it and its controls below
plot_card <- function(title, plot_id, height, ..., full_screen = FALSE) {
  controls <- list(...)
  card(
    full_screen = full_screen,
    card_header(class = "py-1 small", title),
    card_body(class = "p-1", plotOutput(plot_id, height = height)),
    if (length(controls) > 0) card_footer(class = "py-2 small", controls)
  )
}

search_tab <- function() {
  nav_panel(
    title = "Search",
    card_body(
      class = "p-2",
      tab_note(
        "How the method finds the data. Each iteration moves the fitted values toward their targets. The residuals are right from the first one."
      ),
      layout_columns(
        col_widths = c(5, 7),
        plot_card(
          "Distance of the fitted values from their targets",
          "search_path",
          "300px",
          sliderInput(
            "search_step",
            "Step size",
            min = 0.05,
            max = 1,
            value = 1,
            step = 0.05,
            width = "100%"
          ),
          div(
            class = "text-body-secondary",
            "A step of 1 is what surreal() takes. A smaller one slows the search down to watch it."
          )
        ),
        plot_card(
          "Fitted vs residuals at an iteration",
          "search_picture",
          "340px",
          sliderInput(
            "search_iteration",
            "Iteration",
            min = 1,
            max = 4,
            value = 1,
            step = 1,
            width = "100%",
            ticks = FALSE,
            animate = animationOptions(interval = 500)
          ),
          full_screen = TRUE
        )
      )
    )
  )
}

selection_tab <- function() {
  nav_panel(
    title = "Selection",
    card_body(
      class = "p-2",
      tab_note(
        "How an analyst finds the image. Forward selection adds one predictor at each step. Among decoys, set under Plot Settings, the criterion is lowest where the image is clear."
      ),
      layout_columns(
        col_widths = c(5, 7),
        div(
          plot_card(
            "Coefficients along the path",
            "selection_paths",
            "170px"
          ),
          plot_card(
            "Criterion along the path",
            "selection_criterion",
            "170px",
            selectInput("criterion", "Criterion", c("BIC", "AIC"))
          )
        ),
        plot_card(
          "Fitted vs residuals at a step",
          "selection_residual",
          "340px",
          sliderInput(
            "selection_step",
            "Step",
            min = 0,
            max = 25,
            value = 5,
            step = 1,
            width = "100%",
            ticks = FALSE,
            animate = animationOptions(interval = 500)
          ),
          full_screen = TRUE
        )
      )
    )
  )
}

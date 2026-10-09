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

# A slider over whole steps, with its play button beside its label. Shiny
# puts the button at the lower right of a slider, and there it sits under the
# button that expands the card. Shiny plays any link of this class through
# the slider named in `data-target-id`.
step_slider <- function(id, label, min, max, value) {
  play <- tags$a(
    href = "#",
    class = "slider-animate-button ms-2",
    role = "button",
    title = "Play",
    `data-target-id` = id,
    `data-interval` = 500,
    `data-loop` = "FALSE",
    span(class = "play", icon("play")),
    span(class = "pause", icon("pause"))
  )

  sliderInput(
    id,
    span(label, play),
    min = min,
    max = max,
    value = value,
    step = 1,
    width = "100%",
    ticks = FALSE
  )
}

search_tab <- function() {
  nav_panel(
    title = "Search",
    card_body(
      class = "p-2",
      tab_note(
        "The search that makes the data, one iteration at a time. The residuals are in place from the first iteration, and each one moves the fitted values toward their targets."
      ),
      layout_columns(
        col_widths = c(5, 7),
        plot_card(
          "Distance of the fitted values from their targets",
          "search_path",
          "300px",
          # The slider starts below 1, the step surreal() takes, so that there
          # are enough iterations to watch
          sliderInput(
            "search_step",
            span(
              "Step size",
              tooltip(
                icon("circle-info", class = "ms-1 text-body-secondary"),
                "surreal() takes a step of 1 and finishes in a few iterations. A smaller step slows the search, so the image can be watched as it forms."
              )
            ),
            min = 0.05,
            max = 1,
            value = 0.25,
            step = 0.05,
            width = "100%"
          )
        ),
        plot_card(
          "Fitted vs residuals at an iteration",
          "search_picture",
          "340px",
          step_slider(
            "search_iteration",
            "Iteration",
            min = 1,
            max = 4,
            value = 1
          ),
          full_screen = TRUE
        )
      )
    )
  )
}

# The key to the colors of the coefficient paths, for a card's header
path_key <- function() {
  entry <- function(part, label) {
    span(
      class = "ms-2 text-nowrap",
      span(class = paste0("path-key path-key-", part)),
      label
    )
  }
  span(
    class = "text-body-secondary",
    entry("real", "real"),
    entry("decoy", "decoy"),
    entry("out", "not in the model")
  )
}

# The key to the two lines that mark steps on the plots along the path
step_key <- function() {
  entry <- function(part, label) {
    span(
      class = "ms-2 text-nowrap",
      span(class = paste0("path-key path-key-", part)),
      label
    )
  }
  span(
    class = "text-body-secondary",
    entry("step", "this step"),
    entry("best", "lowest")
  )
}

selection_tab <- function() {
  nav_panel(
    title = "Selection",
    card_body(
      class = "p-2",
      tab_note(
        "Forward selection over the data, one predictor at each step. With decoy predictors, set under Plot Settings, the criterion is lowest at the step with the clearest image. Step past it to see the image blur."
      ),
      layout_columns(
        col_widths = c(5, 7),
        div(
          plot_card(
            div(
              class = "d-flex flex-wrap justify-content-between",
              span("Coefficients along the path"),
              path_key()
            ),
            "selection_paths",
            "170px"
          ),
          plot_card(
            div(
              class = "d-flex flex-wrap justify-content-between",
              span("Criterion along the path"),
              step_key()
            ),
            "selection_criterion",
            "170px",
            selectInput("criterion", "Criterion", c("BIC", "AIC"))
          )
        ),
        plot_card(
          "Fitted vs residuals at a step",
          "selection_residual",
          "340px",
          step_slider("selection_step", "Step", min = 0, max = 25, value = 5),
          full_screen = TRUE
        )
      )
    )
  )
}

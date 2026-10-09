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

# One entry of a key: a sample of the line or color, then its name
key_entry <- function(part, label) {
  span(
    class = "me-2 text-nowrap",
    span(class = paste0("path-key path-key-", part)),
    label
  )
}

# The key to the colors of the predictors, for a card's header. The entry
# for the criterion's charge shows only with the plot that draws it.
path_key <- function() {
  div(
    class = "d-flex flex-wrap text-body-secondary",
    key_entry("real", "real"),
    key_entry("decoy", "decoy"),
    key_entry("out", "not in the model"),
    conditionalPanel(
      condition = "input.path_view != 'coefficients'",
      key_entry("charge", "the criterion's charge")
    )
  )
}

# The key to the two lines that mark steps on the plots along the path
step_key <- function() {
  div(
    class = "d-flex flex-wrap text-body-secondary",
    key_entry("step", "this step"),
    key_entry("best", "lowest")
  )
}

# A two-way switch for an input, in place of a menu that would hide one of
# the choices. `choices` is named by label, and the first one starts chosen.
# Shiny reads any group of radio buttons under this class.
two_way_switch <- function(id, choices, label) {
  choice <- function(i) {
    button <- paste0(id, "_", tolower(choices[[i]]))
    tagList(
      tags$input(
        type = "radio",
        class = "btn-check",
        name = id,
        id = button,
        value = choices[[i]],
        autocomplete = "off",
        checked = if (i == 1) NA
      ),
      tags$label(
        class = "btn btn-outline-secondary btn-sm py-0 px-2",
        `for` = button,
        names(choices)[[i]]
      )
    )
  }
  div(
    id = id,
    class = "shiny-input-radiogroup btn-group",
    role = "group",
    `aria-label` = label,
    lapply(seq_along(choices), choice)
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
              class = "d-flex flex-wrap justify-content-between align-items-center gap-1",
              span("Each step along the path"),
              two_way_switch(
                "path_view",
                c(Explained = "explained", Coefficients = "coefficients"),
                "View of each step"
              ),
              path_key()
            ),
            "selection_paths",
            "190px"
          ),
          plot_card(
            div(
              class = "d-flex flex-wrap justify-content-between align-items-center gap-1",
              span("Criterion along the path"),
              two_way_switch(
                "criterion",
                c(BIC = "BIC", AIC = "AIC"),
                "Criterion"
              ),
              step_key()
            ),
            "selection_criterion",
            "190px"
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

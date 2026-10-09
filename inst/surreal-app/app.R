# Surreal Shiny App
# Hidden Images in Residual Plots

library(shiny)
library(bslib)
library(surreal)

# Helper: Check image package availability
check_image_package <- function(ext) {
  pkg <- switch(
    tolower(ext),
    "jpg" = ,
    "jpeg" = "jpeg",
    "bmp" = "bmp",
    "tif" = ,
    "tiff" = "tiff",
    "svg" = "rsvg",
    NULL
  )

  if (is.null(pkg)) {
    return(list(available = TRUE, package = NULL))
  }
  list(available = requireNamespace(pkg, quietly = TRUE), package = pkg)
}

# Shinylive serves a download through a service worker, and Chromium skips the
# service worker for a link that carries the `download` attribute. When R is
# running in the browser the buttons go without it, so the file arrives.
download_button <- function(
  ...,
  in_browser = identical(R.version$os, "emscripten")
) {
  button <- downloadButton(...)
  if (in_browser) {
    button$attribs$download <- NULL
  }
  button
}

# The settings each preset button applies
presets <- list(
  fast = list(
    r_squared = 0.3,
    p = 3,
    point_size = 0.4,
    max_points = 1500,
    decoys = 10
  ),
  balanced = list(
    r_squared = 0.3,
    p = 5,
    point_size = 0.6,
    max_points = 3000,
    decoys = 20
  ),
  detail = list(
    r_squared = 0.2,
    p = 8,
    point_size = 0.8,
    max_points = 6000,
    decoys = 40
  )
)

# Move sliders to the values given for them by name
set_sliders <- function(session, values) {
  for (id in names(values)) {
    updateSliderInput(session, id, value = values[[id]])
  }
}

# UI
ui <- page_navbar(
  title = span("Surreal", class = "fw-bold"),
  window_title = "Surreal: Hidden Images in Residuals",
  fillable = TRUE,

  # Enable busy indicators during computation
  header = tagList(
    useBusyIndicators(),
    tags$style(HTML(
      "
      /* The key to the colors of the coefficient paths */
      .path-key {
        display: inline-block;
        width: 14px;
        height: 3px;
        margin-right: 4px;
        vertical-align: middle;
        border-radius: 2px;
      }
      .path-key-real { background: #2a78d6; }
      .path-key-decoy { background: #eb6834; }
      .path-key-out { background: #a0aec0; }
      [data-bs-theme='dark'] .path-key-real { background: #3987e5; }
      [data-bs-theme='dark'] .path-key-decoy { background: #d95926; }
      [data-bs-theme='dark'] .path-key-out { background: #718096; }
      .path-key-step { background: #4a3aa7; }
      [data-bs-theme='dark'] .path-key-step { background: #d6bcfa; }
      .path-key-best {
        height: 0;
        border-top: 3px dashed #1baf7a;
        border-radius: 0;
      }
      [data-bs-theme='dark'] .path-key-best { border-top-color: #199e70; }

      /* Dark mode outline button fix */
      [data-bs-theme='dark'] .btn-outline-secondary {
        --bs-btn-color: #adb5bd;
        --bs-btn-border-color: #adb5bd;
        --bs-btn-hover-color: #fff;
        --bs-btn-hover-bg: #6c757d;
        --bs-btn-hover-border-color: #6c757d;
      }
    "
    )),
    tags$script(HTML(
      "
      // Toggle Image Settings visibility based on input mode
      $(document).on('shiny:inputchanged', function(e) {
        if (e.name === 'input_mode') {
          $('#image_settings_accordion').toggle(e.value === 'image');
        }
      });
      // Initialize on load
      $(document).on('shiny:connected', function() {
        var mode = $('#input_mode').val();
        $('#image_settings_accordion').toggle(mode === 'image');
      });
    "
    ))
  ),

  theme = bs_theme(
    preset = "bootstrap",
    primary = "#1a365d",
    secondary = "#2c5282",
    success = "#38a169",
    info = "#3182ce",
    warning = "#ed8936",
    danger = "#e53e3e",
    light = "#f7fafc",
    dark = "#1a202c",
    "body-color" = "#2d3748",
    "link-color" = "#1a365d",
    "link-hover-color" = "#2c5282",
    "card-border-radius" = "0.5rem"
  ),

  # Links and dark mode toggle in navbar (top-right)
  nav_spacer(),
  nav_item(
    tags$a(
      href = "https://r-pkg.thecoatlessprofessor.com/surreal/",
      target = "_blank",
      class = "nav-link",
      icon("book"),
      " Docs"
    )
  ),
  nav_item(
    tags$a(
      href = "https://github.com/coatless-rpkg/surreal",
      target = "_blank",
      class = "nav-link",
      icon("github")
    )
  ),
  nav_item(input_dark_mode(id = "dark_mode", mode = "light")),

  # Single main panel with sidebar layout
  nav_panel(
    title = NULL,
    value = "main",

    layout_sidebar(
      fillable = TRUE,

      sidebar = sidebar(
        width = 280,
        open = TRUE,

        # Source selection
        selectInput(
          "input_mode",
          "Source",
          choices = c(
            "Demo: Jack-o-Lantern" = "demo_jack",
            "Demo: R Logo" = "demo_rlogo",
            "Custom Text" = "text",
            "Upload Image" = "image"
          ),
          selected = "image"
        ),

        # Conditional inputs for text
        conditionalPanel(
          condition = "input.input_mode == 'text'",
          textAreaInput(
            "text",
            NULL,
            value = "Hello\nR World!",
            placeholder = "Enter text...",
            rows = 2
          )
        ),

        # Conditional inputs for image
        conditionalPanel(
          condition = "input.input_mode == 'image'",
          fileInput(
            "image",
            NULL,
            accept = c(".png", ".jpg", ".jpeg", ".bmp", ".tif", ".tiff", ".svg")
          ),
          uiOutput("image_warning")
        ),

        # Plot Settings
        accordion(
          id = "plot_settings_accordion",
          open = FALSE,
          accordion_panel(
            "Plot Settings",
            icon = icon("chart-line"),
            sliderInput(
              "r_squared",
              HTML("R<sup>2</sup>"),
              0.1,
              0.9,
              0.3,
              0.05
            ),
            sliderInput("p", "Predictors", 2, 10, 5, 1),
            # Noise predictors for the Selection tab to tell from the real ones
            if (steps_available()) {
              sliderInput("decoys", "Decoy predictors", 0, 60, 20, 5)
            },
            sliderInput("point_size", "Point Size", 0.2, 2, 0.6, 0.1)
          )
        ),

        # Image Settings (toggled via JavaScript)
        accordion(
          id = "image_settings_accordion",
          open = FALSE,
          accordion_panel(
            "Image Settings",
            icon = icon("image"),
            selectInput(
              "image_mode",
              "Mode",
              choices = c("Auto" = "auto", "Dark" = "dark", "Light" = "light")
            ),
            sliderInput("threshold", "Threshold", 0.1, 0.9, 0.5, 0.05),
            sliderInput("max_points", "Max Points", 1000, 8000, 3000, 500)
          )
        ),

        # Preset buttons
        div(
          class = "mb-2",
          span(class = "small text-body-secondary", "Presets:"),
          div(
            class = "btn-group w-100 mt-1",
            role = "group",
            actionButton(
              "preset_fast",
              "Fast",
              class = "btn-outline-secondary btn-sm"
            ),
            actionButton(
              "preset_balanced",
              "Balanced",
              class = "btn-outline-secondary btn-sm"
            ),
            actionButton(
              "preset_detail",
              "Detail",
              class = "btn-outline-secondary btn-sm"
            )
          )
        ),

        # Generate button at bottom of sidebar
        actionButton(
          "generate",
          "Generate",
          class = "btn-primary w-100",
          icon = icon("wand-magic-sparkles")
        ),

        # Copy R code and Undo buttons (compact row)
        div(
          class = "btn-group w-100 mt-2",
          role = "group",
          actionButton(
            "show_code",
            "Code",
            class = "btn-secondary btn-sm",
            icon = icon("code")
          ),
          actionButton(
            "undo",
            "Undo",
            class = "btn-secondary btn-sm",
            icon = icon("rotate-left")
          )
        )
      ),

      # Main content with tabs
      navset_card_tab(
        id = "main_tabs",

        nav_panel(
          title = "Compare",
          card_body(
            class = "p-2",
            layout_columns(
              col_widths = c(6, 6),
              card(
                class = "source-card",
                full_screen = TRUE,
                card_header(
                  class = "py-1 small d-flex justify-content-between align-items-center",
                  span("Source"),
                  download_button(
                    "download_source",
                    "PNG",
                    class = "btn-sm btn-outline-secondary py-0 px-2"
                  )
                ),
                card_body(
                  class = "p-1",
                  plotOutput("compare_source", height = "400px")
                ),
                card_footer(
                  class = "py-1 small text-body-secondary",
                  "Original input coordinates or image"
                )
              ),
              card(
                full_screen = TRUE,
                card_header(
                  class = "py-1 small d-flex justify-content-between align-items-center",
                  span("Residuals"),
                  download_button(
                    "download_residual",
                    "PNG",
                    class = "btn-sm btn-outline-secondary py-0 px-2"
                  )
                ),
                card_body(
                  class = "p-1",
                  plotOutput("compare_residual", height = "400px")
                ),
                card_footer(
                  class = "py-1 small text-body-secondary d-flex justify-content-between",
                  span("Fitted vs residuals - hidden image revealed"),
                  span(textOutput("obs_count", inline = TRUE))
                )
              )
            )
          )
        ),

        nav_panel(
          title = "Pairs Plot",
          card_body(
            class = "p-2",
            div(
              class = "mb-2 ps-2 py-1 small text-body-secondary border-start border-primary border-3 bg-body-secondary rounded-end",
              "Scatterplot matrix showing relationships between all variables."
            ),
            plotOutput("pairs_plot", height = "500px")
          )
        ),

        nav_panel(
          title = "Statistics",
          card_body(
            class = "p-2",
            div(
              class = "mb-2 ps-2 py-1 small text-body-secondary border-start border-primary border-3 bg-body-secondary rounded-end",
              "Model fit statistics from lm(y ~ .)."
            ),
            layout_columns(
              col_widths = c(4, 8),
              card(
                card_header(class = "py-2", "Model Summary"),
                card_body(
                  uiOutput("model_stats")
                )
              ),
              card(
                card_header(class = "py-2", "Coefficients"),
                card_body(
                  tableOutput("coef_table")
                )
              )
            )
          )
        ),

        nav_panel(
          title = "Data",
          card_body(
            div(
              class = "d-flex justify-content-between align-items-center mb-2 ps-2 py-1 small text-body-secondary border-start border-primary border-3 bg-body-secondary rounded-end",
              span("First 20 rows of the generated dataset."),
              download_button(
                "download",
                "Download CSV",
                class = "btn-sm btn-outline-primary"
              )
            ),
            tableOutput("data_table")
          )
        ),

        # The method one step at a time, when the package has what it takes
        if (steps_available()) search_tab(),
        if (steps_available()) selection_tab()
      )
    )
  ),

  # Footer
  footer = tags$footer(
    class = "border-top py-3 mt-auto text-center small bg-body-tertiary text-body-secondary",
    div(HTML("&copy; 2026 surreal authors")),
    div(
      "Based on ",
      tags$a(
        href = "https://doi.org/10.1198/000313007X190079",
        target = "_blank",
        "Stefanski, L. A. (2007). \"Residual (Sur)realism\". ",
        tags$em("The American Statistician"),
        ", 61(2), 163-177."
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive data storage with history
  rv <- reactiveValues(
    data = NULL,
    source_coords = NULL,
    source_type = NULL,
    source_text = NULL,
    seed = NULL,
    used = NULL,
    history = list()
  )

  # Clear state when switching source modes
  observeEvent(
    input$input_mode,
    {
      rv$data <- NULL
      rv$source_coords <- NULL
      rv$source_type <- NULL
      rv$source_text <- NULL
      rv$seed <- NULL
      rv$used <- NULL
      rv$history <- list()
    },
    ignoreInit = TRUE
  )

  # Preset handlers
  lapply(names(presets), function(name) {
    observeEvent(input[[paste0("preset_", name)]], {
      set_sliders(session, presets[[name]])
    })
  })

  # Update undo button state
  observe({
    hist_len <- length(rv$history)
    if (hist_len > 0) {
      updateActionButton(
        session,
        "undo",
        label = paste("Undo (", hist_len, ")"),
        disabled = FALSE
      )
    } else {
      updateActionButton(session, "undo", label = "Undo", disabled = TRUE)
    }
  })

  # Undo handler
  observeEvent(input$undo, {
    req(length(rv$history) > 0)
    last_state <- rv$history[[length(rv$history)]]
    # Remove from history first to avoid re-triggering
    rv$history <- rv$history[-length(rv$history)]

    # Restore data
    rv$data <- last_state$data
    rv$source_coords <- last_state$source_coords
    rv$source_type <- last_state$source_type
    rv$source_text <- last_state$source_text
    rv$seed <- last_state$seed
    rv$used <- last_state$used

    # Restore settings if available
    if (!is.null(last_state$settings)) {
      sliders <- c(
        "r_squared",
        "p",
        "point_size",
        "max_points",
        "threshold",
        "decoys"
      )
      set_sliders(
        session,
        Filter(Negate(is.null), last_state$settings[sliders])
      )
      updateSelectInput(
        session,
        "image_mode",
        selected = last_state$settings$image_mode
      )
    }

    showNotification("Restored previous state", type = "message", duration = 2)
  })

  # Image package warning
  output$image_warning <- renderUI({
    req(input$image)
    ext <- tools::file_ext(input$image$name)
    check <- check_image_package(ext)
    if (!check$available) {
      div(
        class = "alert alert-warning py-2 small mb-0",
        icon("triangle-exclamation"),
        " Package ",
        tags$b(check$package),
        " required.",
        br(),
        code(paste0('install.packages("', check$package, '")'))
      )
    }
  })

  # Generate data
  observeEvent(input$generate, {
    # Save current state to history (max 5)
    if (!is.null(rv$data)) {
      rv$history <- c(
        rv$history,
        list(list(
          data = rv$data,
          source_coords = rv$source_coords,
          source_type = rv$source_type,
          source_text = rv$source_text,
          seed = rv$seed,
          used = rv$used,
          settings = list(
            r_squared = input$r_squared,
            p = input$p,
            point_size = input$point_size,
            max_points = input$max_points,
            image_mode = input$image_mode,
            threshold = input$threshold,
            decoys = input$decoys
          )
        ))
      )
      if (length(rv$history) > 5) {
        rv$history <- rv$history[-1]
      }
    }

    # Early validation with user-friendly messages
    if (input$input_mode == "image" && is.null(input$image)) {
      showNotification("Please upload an image first.", type = "warning")
      return()
    }
    if (input$input_mode == "text" && nchar(trimws(input$text)) == 0) {
      showNotification("Please enter some text first.", type = "warning")
      return()
    }

    # The data is generated from a seed that is kept, so the Search tab can
    # run the same search again and show its iterations
    seed <- sample.int(1e9, 1)

    tryCatch(
      {
        result <- with_seed(
          seed,
          switch(
            input$input_mode,
            "demo_jack" = {
              rv$source_coords <- data.frame(
                x = jackolantern_surreal_data[[2]],
                y = jackolantern_surreal_data[[1]]
              )
              rv$source_type <- "demo"
              showNotification(
                "Jack-o-Lantern uses pre-built data. R^2 and Predictor sliders don't apply.",
                type = "warning",
                duration = 4
              )
              jackolantern_surreal_data
            },
            "demo_rlogo" = {
              rv$source_coords <- r_logo_image_data
              rv$source_type <- "demo"
              surreal(
                r_logo_image_data,
                R_squared = input$r_squared,
                p = input$p
              )
            },
            "text" = {
              req(nchar(trimws(input$text)) > 0)
              rv$source_type <- "text"
              rv$source_coords <- NULL
              rv$source_text <- input$text
              surreal_text(input$text, R_squared = input$r_squared, p = input$p)
            },
            "image" = {
              req(input$image)
              ext <- tools::file_ext(input$image$name)
              check <- check_image_package(ext)
              validate(need(
                check$available,
                paste("Install", check$package, "package")
              ))
              rv$source_type <- "image"
              rv$source_coords <- input$image$datapath
              surreal_image(
                input$image$datapath,
                mode = input$image_mode,
                threshold = if (input$image_mode == "auto") {
                  NULL
                } else {
                  input$threshold
                },
                max_points = input$max_points,
                R_squared = input$r_squared,
                p = input$p
              )
            }
          )
        )
        rv$data <- result
        rv$seed <- seed
        rv$used <- list(
          mode = input$input_mode,
          r_squared = input$r_squared,
          p = input$p,
          image_mode = input$image_mode,
          threshold = input$threshold,
          max_points = input$max_points
        )
        showNotification(
          paste("Generated", format(nrow(result), big.mark = ","), "points"),
          type = "message",
          duration = 3
        )
      },
      error = function(e) {
        showNotification(e$message, type = "error")
      }
    )
  })

  # Observation count
  output$obs_count <- renderText({
    req(rv$data)
    paste(format(nrow(rv$data), big.mark = ","), "points")
  })

  # Model
  model <- reactive({
    req(rv$data)
    lm(y ~ ., data = rv$data)
  })

  # What the plots are drawn with and from
  colors <- reactive(plot_palette(isTRUE(input$dark_mode == "dark")))
  current_source <- reactive(list(
    type = rv$source_type,
    coords = rv$source_coords,
    text = rv$source_text
  ))

  # Write a plot to a PNG file at download size
  save_png <- function(file, draw) {
    png(file, width = 1200, height = 800, res = 150, bg = colors()$bg)
    on.exit(dev.off())
    draw()
  }

  # Download residual plot as PNG
  output$download_residual <- downloadHandler(
    filename = function() paste0("surreal_residual_", Sys.Date(), ".png"),
    content = function(file) {
      req(model())
      save_png(file, function() {
        draw_residuals(
          model(),
          colors(),
          isolate(input$point_size),
          full = TRUE
        )
      })
    }
  )

  # Download source plot as PNG
  output$download_source <- downloadHandler(
    filename = function() paste0("surreal_source_", Sys.Date(), ".png"),
    content = function(file) {
      save_png(file, function() {
        draw_source(
          current_source(),
          colors(),
          isolate(input$point_size),
          full = TRUE
        )
      })
    }
  )

  # Pairs plot
  output$pairs_plot <- renderPlot(
    {
      req(rv$data)
      draw_pairs(rv$data, colors())
    },
    bg = "transparent",
    res = 96
  )

  # Compare view - Source plot
  output$compare_source <- renderPlot(
    draw_source(current_source(), colors(), isolate(input$point_size)),
    bg = "transparent",
    res = 96
  )

  # Compare view - Residual plot
  output$compare_residual <- renderPlot(
    {
      req(model())
      draw_residuals(model(), colors(), isolate(input$point_size))
    },
    bg = "transparent",
    res = 96
  )

  # The search: the same run that made the data, with its iterations kept
  trace <- reactive({
    req(steps_available(), rv$data, rv$used, input$search_step)
    used <- rv$used
    validate(need(
      used$mode != "demo_jack",
      "The jack-o'-lantern data comes ready-made, so there is no search to show. Pick another source."
    ))

    with_seed(rv$seed, {
      points <- switch(
        used$mode,
        "demo_rlogo" = r_logo_image_data,
        "text" = surreal_text_points(rv$source_text),
        "image" = surreal_image_points(
          rv$source_coords,
          mode = used$image_mode,
          threshold = if (used$image_mode == "auto") NULL else used$threshold,
          max_points = used$max_points
        )
      )
      surreal_trace(
        points,
        R_squared = used$r_squared,
        p = used$p,
        step = input$search_step
      )
    })
  })

  # The selection: forward selection over the data, with decoys added to it
  path <- reactive({
    req(steps_available(), rv$data, input$criterion)
    decoys <- input$decoys
    data <- rv$data
    if (isTRUE(decoys >= 1)) {
      data <- with_seed(
        rv$seed + 1,
        surreal_decoys(data, n = min(round(decoys), 60), shuffle = FALSE)
      )
    }
    surreal_path(data, criterion = input$criterion)
  })

  # Each slider covers the steps there are, and starts where the story does.
  # A search or selection that cannot be shown leaves its slider alone.
  quietly <- function(code) tryCatch(code, error = function(e) NULL)

  observeEvent(quietly(trace()), {
    updateSliderInput(
      session,
      "search_iteration",
      max = nrow(trace()$iterations),
      value = 1
    )
  })
  observeEvent(quietly(path()), {
    updateSliderInput(
      session,
      "selection_step",
      max = nrow(path()$steps) - 1,
      value = path()$best
    )
  })

  output$search_path <- renderPlot(
    {
      iteration <- min(input$search_iteration, nrow(trace()$iterations))
      draw_search_path(trace(), iteration, colors())
    },
    bg = "transparent",
    res = 96
  )

  output$search_picture <- renderPlot(
    {
      iteration <- min(input$search_iteration, nrow(trace()$iterations))
      draw_frame(
        trace()$fitted[, iteration],
        trace()$residuals,
        colors(),
        isolate(input$point_size)
      )
    },
    bg = "transparent",
    res = 96
  )

  output$selection_paths <- renderPlot(
    {
      step <- min(input$selection_step, nrow(path()$steps) - 1)
      draw_coefficient_paths(path(), step, colors())
    },
    bg = "transparent",
    res = 96
  )

  output$selection_criterion <- renderPlot(
    {
      step <- min(input$selection_step, nrow(path()$steps) - 1)
      draw_criterion(path(), step, colors())
    },
    bg = "transparent",
    res = 96
  )

  output$selection_residual <- renderPlot(
    {
      step <- min(input$selection_step, nrow(path()$steps) - 1)
      draw_frame(
        path()$fitted[, step + 1],
        path()$residuals[, step + 1],
        colors(),
        isolate(input$point_size)
      )
    },
    bg = "transparent",
    res = 96
  )

  # Model statistics
  output$model_stats <- renderUI({
    req(model())
    s <- summary(model())

    div(
      class = "small",
      tags$dl(
        class = "row mb-0",
        tags$dt(class = "col-6", HTML("R<sup>2</sup>")),
        tags$dd(class = "col-6 text-end", round(s$r.squared, 4)),
        tags$dt(class = "col-6", HTML("Adj. R<sup>2</sup>")),
        tags$dd(class = "col-6 text-end", round(s$adj.r.squared, 4)),
        tags$dt(class = "col-6", "F-statistic"),
        tags$dd(class = "col-6 text-end", round(s$fstatistic[1], 2)),
        tags$dt(class = "col-6", "DF"),
        tags$dd(
          class = "col-6 text-end",
          paste(s$fstatistic[2], "/", s$fstatistic[3])
        ),
        tags$dt(class = "col-6", "Residual SE"),
        tags$dd(class = "col-6 text-end", round(s$sigma, 4)),
        tags$dt(class = "col-6", "Observations"),
        tags$dd(class = "col-6 text-end", format(nrow(rv$data), big.mark = ","))
      )
    )
  })

  # Coefficients table
  output$coef_table <- renderTable(
    {
      req(model())
      s <- summary(model())
      coefs <- as.data.frame(s$coefficients)
      coefs <- cbind(Term = rownames(coefs), coefs)
      rownames(coefs) <- NULL
      names(coefs) <- c("Term", "Estimate", "Std. Error", "t value", "Pr(>|t|)")
      coefs
    },
    digits = 4,
    striped = TRUE,
    hover = TRUE,
    spacing = "s"
  )

  # Data table
  output$data_table <- renderTable(
    {
      req(rv$data)
      head(rv$data, 20)
    },
    digits = 4
  )

  # Download
  output$download <- downloadHandler(
    filename = function() paste0("surreal_", Sys.Date(), ".csv"),
    content = function(file) write.csv(rv$data, file, row.names = FALSE)
  )

  # Generate R code
  generate_code <- reactive({
    example_code(input$input_mode, reactiveValuesToList(input))
  })

  # Show code modal
  observeEvent(input$show_code, {
    showModal(modalDialog(
      title = "R Code",
      tags$pre(
        id = "code-block",
        class = "bg-body-secondary p-3 rounded",
        style = "white-space: pre-wrap; font-family: monospace;",
        generate_code()
      ),
      footer = tagList(
        tags$button(
          type = "button",
          class = "btn btn-primary",
          onclick = "navigator.clipboard.writeText(document.getElementById('code-block').innerText).then(() => { this.innerText = 'Copied!'; setTimeout(() => { this.innerText = 'Copy to Clipboard'; }, 2000); });",
          icon("copy"),
          " Copy to Clipboard"
        ),
        modalButton("Close")
      ),
      easyClose = TRUE
    ))
  })
}

shinyApp(ui, server)

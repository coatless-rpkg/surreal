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
                  downloadButton(
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
                  downloadButton(
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
              downloadButton(
                "download",
                "Download CSV",
                class = "btn-sm btn-outline-primary"
              )
            ),
            tableOutput("data_table")
          )
        )
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
    history = list()
  )

  # Clear state when switching source modes
  observeEvent(
    input$input_mode,
    {
      rv$data <- NULL
      rv$source_coords <- NULL
      rv$source_type <- NULL
      rv$history <- list()
    },
    ignoreInit = TRUE
  )

  # Preset handlers
  observeEvent(input$preset_fast, {
    updateSliderInput(session, "r_squared", value = 0.3)
    updateSliderInput(session, "p", value = 3)
    updateSliderInput(session, "point_size", value = 0.4)
    updateSliderInput(session, "max_points", value = 1500)
  })

  observeEvent(input$preset_balanced, {
    updateSliderInput(session, "r_squared", value = 0.3)
    updateSliderInput(session, "p", value = 5)
    updateSliderInput(session, "point_size", value = 0.6)
    updateSliderInput(session, "max_points", value = 3000)
  })

  observeEvent(input$preset_detail, {
    updateSliderInput(session, "r_squared", value = 0.2)
    updateSliderInput(session, "p", value = 8)
    updateSliderInput(session, "point_size", value = 0.8)
    updateSliderInput(session, "max_points", value = 6000)
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

    # Restore settings if available
    if (!is.null(last_state$settings)) {
      updateSliderInput(
        session,
        "r_squared",
        value = last_state$settings$r_squared
      )
      updateSliderInput(session, "p", value = last_state$settings$p)
      updateSliderInput(
        session,
        "point_size",
        value = last_state$settings$point_size
      )
      updateSliderInput(
        session,
        "max_points",
        value = last_state$settings$max_points
      )
      updateSelectInput(
        session,
        "image_mode",
        selected = last_state$settings$image_mode
      )
      updateSliderInput(
        session,
        "threshold",
        value = last_state$settings$threshold
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
          settings = list(
            r_squared = input$r_squared,
            p = input$p,
            point_size = input$point_size,
            max_points = input$max_points,
            image_mode = input$image_mode,
            threshold = input$threshold
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

    tryCatch(
      {
        result <- switch(
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
            surreal(r_logo_image_data, R_squared = input$r_squared, p = input$p)
          },
          "text" = {
            req(nchar(trimws(input$text)) > 0)
            rv$source_type <- "text"
            rv$source_coords <- NULL
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
        rv$data <- result
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

  # Helper function for residual plot
  plot_residuals <- function(m, point_size, is_dark) {
    bg <- if (is_dark) "#1a202c" else "#ffffff"
    fg <- if (is_dark) "#f7fafc" else "#2d3748"
    pt <- if (is_dark) "#63b3ed" else "#1a365d"

    par(mar = c(4, 4, 1, 1), bg = bg, fg = fg, col.axis = fg, col.lab = fg)
    plot(
      m$fitted.values,
      m$residuals,
      pch = 20,
      cex = point_size,
      col = pt,
      xlab = "Fitted",
      ylab = "Residuals",
      axes = FALSE
    )
    axis(1)
    axis(2)
    box()
    abline(h = 0, lty = 2, col = if (is_dark) "#4a5568" else "#cbd5e0")
  }

  # Download residual plot as PNG
  output$download_residual <- downloadHandler(
    filename = function() paste0("surreal_residual_", Sys.Date(), ".png"),
    content = function(file) {
      req(model())
      is_dark <- isTRUE(input$dark_mode == "dark")
      bg <- if (is_dark) "#1a202c" else "#ffffff"

      png(file, width = 1200, height = 800, res = 150, bg = bg)
      plot_residuals(model(), isolate(input$point_size), is_dark)
      dev.off()
    }
  )

  # Download source plot as PNG
  output$download_source <- downloadHandler(
    filename = function() paste0("surreal_source_", Sys.Date(), ".png"),
    content = function(file) {
      is_dark <- isTRUE(input$dark_mode == "dark")
      bg <- if (is_dark) "#1a202c" else "#ffffff"
      fg <- if (is_dark) "#f7fafc" else "#2d3748"
      pt <- if (is_dark) "#63b3ed" else "#1a365d"
      point_size <- isolate(input$point_size)

      png(file, width = 1200, height = 800, res = 150, bg = bg)

      if (rv$source_type == "image" && !is.null(rv$source_coords)) {
        img <- surreal:::load_image_file(rv$source_coords)
        par(mar = c(0, 0, 0, 0), bg = bg)
        plot(0:1, 0:1, type = "n", axes = FALSE, xlab = "", ylab = "", asp = 1)
        graphics::rasterImage(img, 0, 0, 1, 1)
      } else if (rv$source_type == "text") {
        par(mar = c(0, 0, 0, 0), bg = bg, fg = fg)
        plot(0:1, 0:1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(0.5, 0.5, isolate(input$text), cex = 4, col = pt, font = 2)
      } else if (!is.null(rv$source_coords)) {
        par(mar = c(4, 4, 1, 1), bg = bg, fg = fg, col.axis = fg, col.lab = fg)
        plot(
          rv$source_coords$x,
          rv$source_coords$y,
          pch = 20,
          cex = point_size,
          col = pt,
          xlab = "X",
          ylab = "Y",
          axes = FALSE,
          asp = 1
        )
        axis(1)
        axis(2)
        box()
      }

      dev.off()
    }
  )

  # Pairs plot
  output$pairs_plot <- renderPlot(
    {
      req(rv$data)

      is_dark <- isTRUE(input$dark_mode == "dark")
      bg <- if (is_dark) "#1a202c" else "#ffffff"
      fg <- if (is_dark) "#f7fafc" else "#2d3748"
      pt <- if (is_dark) "#63b3ed60" else "#1a365d60"

      par(bg = bg, fg = fg, col.axis = fg, col.lab = fg)
      pairs(rv$data, pch = 20, cex = 0.3, col = pt)
    },
    bg = "transparent",
    res = 96
  )

  # Compare view - Source plot
  output$compare_source <- renderPlot(
    {
      is_dark <- isTRUE(input$dark_mode == "dark")
      bg <- if (is_dark) "#1a202c" else "#ffffff"
      fg <- if (is_dark) "#f7fafc" else "#2d3748"
      pt <- if (is_dark) "#63b3ed" else "#1a365d"
      point_size <- isolate(input$point_size)

      # Empty state - no data yet
      if (is.null(rv$source_type)) {
        par(mar = c(0, 0, 0, 0), bg = bg)
        plot.new()
      } else if (rv$source_type == "image") {
        img <- surreal:::load_image_file(rv$source_coords)
        par(mar = c(0, 0, 0, 0), bg = bg)
        plot(0:1, 0:1, type = "n", axes = FALSE, xlab = "", ylab = "", asp = 1)
        graphics::rasterImage(img, 0, 0, 1, 1)
      } else if (rv$source_type == "text") {
        par(mar = c(0, 0, 0, 0), bg = bg, fg = fg)
        plot(0:1, 0:1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(0.5, 0.5, isolate(input$text), cex = 3, col = pt, font = 2)
      } else {
        par(mar = c(3, 3, 1, 1), bg = bg, fg = fg, col.axis = fg, col.lab = fg)
        plot(
          rv$source_coords$x,
          rv$source_coords$y,
          pch = 20,
          cex = point_size * 0.8,
          col = pt,
          xlab = "",
          ylab = "",
          axes = FALSE,
          asp = 1
        )
        axis(1, cex.axis = 0.8)
        axis(2, cex.axis = 0.8)
        box()
      }
    },
    bg = "transparent",
    res = 96
  )

  # Compare view - Residual plot
  output$compare_residual <- renderPlot(
    {
      req(model())
      is_dark <- isTRUE(input$dark_mode == "dark")
      point_size <- isolate(input$point_size)
      bg <- if (is_dark) "#1a202c" else "#ffffff"
      fg <- if (is_dark) "#f7fafc" else "#2d3748"
      pt <- if (is_dark) "#63b3ed" else "#1a365d"
      m <- model()

      par(mar = c(3, 3, 1, 1), bg = bg, fg = fg, col.axis = fg, col.lab = fg)
      plot(
        m$fitted.values,
        m$residuals,
        pch = 20,
        cex = point_size * 0.8,
        col = pt,
        xlab = "",
        ylab = "",
        axes = FALSE
      )
      axis(1, cex.axis = 0.8)
      axis(2, cex.axis = 0.8)
      box()
      abline(h = 0, lty = 2, col = if (is_dark) "#4a5568" else "#cbd5e0")
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
    code <- switch(
      input$input_mode,
      "demo_jack" = 'library(surreal)
data("jackolantern_surreal_data")
result <- jackolantern_surreal_data
model <- lm(y ~ ., data = result)
plot(model$fitted.values, model$residuals, pch = 20)',

      "demo_rlogo" = sprintf(
        'library(surreal)
data("r_logo_image_data")
result <- surreal(r_logo_image_data, R_squared = %.2f, p = %d)
model <- lm(y ~ ., data = result)
plot(model$fitted.values, model$residuals, pch = 20)',
        input$r_squared,
        input$p
      ),

      "text" = sprintf(
        'library(surreal)
result <- surreal_text("%s", R_squared = %.2f, p = %d)
model <- lm(y ~ ., data = result)
plot(model$fitted.values, model$residuals, pch = 20)',
        input$text,
        input$r_squared,
        input$p
      ),

      "image" = sprintf(
        'library(surreal)
result <- surreal_image(
  "path/to/your/image.png",
  mode = "%s",
  threshold = %s,
  max_points = %d,

  R_squared = %.2f,
  p = %d
)
model <- lm(y ~ ., data = result)
plot(model$fitted.values, model$residuals, pch = 20)',
        input$image_mode,
        if (input$image_mode == "auto") {
          "NULL"
        } else {
          sprintf("%.2f", input$threshold)
        },
        input$max_points,
        input$r_squared,
        input$p
      )
    )
    code
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

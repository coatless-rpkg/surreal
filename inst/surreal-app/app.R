# Surreal Shiny App
# Hidden Images in Residual Plots

library(shiny)
library(bslib)
library(surreal)

# Helper: Check image package availability
check_image_package <- function(ext) {
  pkg <- switch(tolower(ext),
    "jpg" = , "jpeg" = "jpeg",
    "bmp" = "bmp",
    "tif" = , "tiff" = "tiff",
    "svg" = "rsvg",
    NULL
  )

  if (is.null(pkg)) return(list(available = TRUE, package = NULL))
  list(available = requireNamespace(pkg, quietly = TRUE), package = pkg)
}

# UI
ui <- page_navbar(
  title = span("Surreal", class = "fw-bold"),
  window_title = "Surreal: Hidden Images in Residuals",
  fillable = TRUE,

  # Enable busy indicators during computation
  header = useBusyIndicators(),

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
      icon("book"), " Docs"
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
          )
        ),

        # Conditional inputs for text
        conditionalPanel(
          condition = "input.input_mode == 'text'",
          textAreaInput(
            "text", NULL,
            value = "Hello R World!",
            placeholder = "Enter text...",
            rows = 2
          )
        ),

        # Conditional inputs for image
        conditionalPanel(
          condition = "input.input_mode == 'image'",
          fileInput(
            "image", NULL,
            accept = c(".png", ".jpg", ".jpeg", ".bmp", ".tif", ".tiff", ".svg")
          ),
          uiOutput("image_warning")
        ),

        hr(),

        # Plot Settings (always visible, expanded)
        accordion(
          id = "plot_settings_accordion",
          open = TRUE,
          accordion_panel(
            "Plot Settings",
            icon = icon("chart-line"),
            sliderInput("r_squared", "R\u00b2", 0.1, 0.9, 0.3, 0.05),
            sliderInput("p", "Predictors", 2, 10, 5, 1),
            sliderInput("point_size", "Point Size", 0.2, 2, 0.6, 0.1)
          )
        ),

        # Image Settings (only for image uploads, expanded)
        conditionalPanel(
          condition = "input.input_mode == 'image'",
          accordion(
            id = "image_settings_accordion",
            open = TRUE,
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
          )
        ),

        hr(),

        # Generate button at bottom of sidebar
        actionButton(
          "generate",
          "Generate",
          class = "btn-primary w-100",
          icon = icon("wand-magic-sparkles")
        ),

        # Copy R code button
        actionButton(
          "show_code",
          "Copy R Code",
          class = "btn-outline-secondary w-100 mt-2",
          icon = icon("code")
        )
      ),

      # Main content with tabs
      navset_card_tab(
        id = "main_tabs",
        full_screen = TRUE,

        nav_panel(
          title = "Residual Plot",
          card_body(
            class = "p-2",
            div(
              class = "d-flex justify-content-between align-items-center mb-2 ps-2 py-1 small text-body-secondary border-start border-primary border-3 bg-body-secondary rounded-end",
              span("Fitted values vs residuals from lm(y ~ .). The hidden image appears here."),
              span(textOutput("obs_count", inline = TRUE))
            ),
            plotOutput("residual_plot", height = "100%")
          )
        ),

        nav_panel(
          title = "Source",
          card_body(
            class = "p-2",
            div(
              class = "mb-2 ps-2 py-1 small text-body-secondary border-start border-primary border-3 bg-body-secondary rounded-end",
              "Original input coordinates or image before transformation."
            ),
            plotOutput("source_plot", height = "100%")
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
          title = "Data",
          card_body(
            div(
              class = "d-flex justify-content-between align-items-center mb-2 ps-2 py-1 small text-body-secondary border-start border-primary border-3 bg-body-secondary rounded-end",
              span("First 20 rows of the generated dataset."),
              downloadButton("download", "Download CSV", class = "btn-sm btn-outline-primary")
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
    div(
      "\u00a9 2026 surreal authors | ",
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

  # Reactive data storage
  rv <- reactiveValues(data = NULL, source_coords = NULL, source_type = "demo")

  # Load demo data on startup
  observe({
    if (is.null(rv$data)) {
      rv$data <- jackolantern_surreal_data
      rv$source_coords <- data.frame(
        x = jackolantern_surreal_data[[2]],
        y = jackolantern_surreal_data[[1]]
      )
      rv$source_type <- "demo"
    }
  })

  # Image package warning
  output$image_warning <- renderUI({
    req(input$image)
    ext <- tools::file_ext(input$image$name)
    check <- check_image_package(ext)
    if (!check$available) {
      div(
        class = "alert alert-warning py-2 small mb-0",
        icon("triangle-exclamation"), " Package ", tags$b(check$package), " required.",
        br(), code(paste0('install.packages("', check$package, '")'))
      )
    }
  })

  # Generate data
  observeEvent(input$generate, {
    tryCatch({
      result <- switch(input$input_mode,
        "demo_jack" = {
          rv$source_coords <- data.frame(
            x = jackolantern_surreal_data[[2]],
            y = jackolantern_surreal_data[[1]]
          )
          rv$source_type <- "demo"
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
          validate(need(check$available, paste("Install", check$package, "package")))
          rv$source_type <- "image"
          rv$source_coords <- input$image$datapath
          surreal_image(
            input$image$datapath,
            mode = input$image_mode,
            threshold = if (input$image_mode == "auto") NULL else input$threshold,
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
    }, error = function(e) {
      showNotification(e$message, type = "error")
    })
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

  # Residual plot
  output$residual_plot <- renderPlot({
    req(model())
    m <- model()

    is_dark <- isTRUE(input$dark_mode == "dark")
    bg <- if (is_dark) "#1a202c" else "#ffffff"
    fg <- if (is_dark) "#f7fafc" else "#2d3748"
    pt <- if (is_dark) "#63b3ed" else "#1a365d"

    par(mar = c(4, 4, 1, 1), bg = bg, fg = fg, col.axis = fg, col.lab = fg)
    plot(m$fitted.values, m$residuals, pch = 20, cex = input$point_size, col = pt,
         xlab = "Fitted", ylab = "Residuals", axes = FALSE)
    axis(1); axis(2); box()
    abline(h = 0, lty = 2, col = if (is_dark) "#4a5568" else "#cbd5e0")
  }, bg = "transparent", res = 96)

  # Source plot
  output$source_plot <- renderPlot({
    is_dark <- isTRUE(input$dark_mode == "dark")
    bg <- if (is_dark) "#1a202c" else "#ffffff"
    fg <- if (is_dark) "#f7fafc" else "#2d3748"
    pt <- if (is_dark) "#63b3ed" else "#1a365d"

    if (rv$source_type == "image" && !is.null(rv$source_coords)) {
      # Display uploaded image
      img <- png::readPNG(rv$source_coords)
      par(mar = c(0, 0, 0, 0), bg = bg)
      plot(0:1, 0:1, type = "n", axes = FALSE, xlab = "", ylab = "", asp = 1)
      graphics::rasterImage(img, 0, 0, 1, 1)
    } else if (rv$source_type == "text") {
      # Display text
      par(mar = c(0, 0, 0, 0), bg = bg, fg = fg)
      plot(0:1, 0:1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0.5, 0.5, input$text, cex = 4, col = pt, font = 2)
    } else if (!is.null(rv$source_coords)) {
      # Display point coordinates
      par(mar = c(4, 4, 1, 1), bg = bg, fg = fg, col.axis = fg, col.lab = fg)
      plot(rv$source_coords$x, rv$source_coords$y, pch = 20, cex = input$point_size, col = pt,
           xlab = "X", ylab = "Y", axes = FALSE, asp = 1)
      axis(1); axis(2); box()
    }
  }, bg = "transparent", res = 96)

  # Pairs plot
  output$pairs_plot <- renderPlot({
    req(rv$data)

    is_dark <- isTRUE(input$dark_mode == "dark")
    bg <- if (is_dark) "#1a202c" else "#ffffff"
    fg <- if (is_dark) "#f7fafc" else "#2d3748"
    pt <- if (is_dark) "#63b3ed60" else "#1a365d60"

    par(bg = bg, fg = fg, col.axis = fg, col.lab = fg)
    pairs(rv$data, pch = 20, cex = 0.3, col = pt)
  }, bg = "transparent", res = 96)

  # Data table
  output$data_table <- renderTable({
    req(rv$data)
    head(rv$data, 20)
  }, digits = 4)

  # Download
  output$download <- downloadHandler(
    filename = function() paste0("surreal_", Sys.Date(), ".csv"),
    content = function(file) write.csv(rv$data, file, row.names = FALSE)
  )

  # Generate R code
  generate_code <- reactive({
    code <- switch(input$input_mode,
      "demo_jack" = 'library(surreal)
data("jackolantern_surreal_data")
result <- jackolantern_surreal_data
model <- lm(y ~ ., data = result)
plot(model$fitted.values, model$residuals, pch = 20)',

      "demo_rlogo" = sprintf('library(surreal)
data("r_logo_image_data")
result <- surreal(r_logo_image_data, R_squared = %.2f, p = %d)
model <- lm(y ~ ., data = result)
plot(model$fitted.values, model$residuals, pch = 20)',
        input$r_squared, input$p),

      "text" = sprintf('library(surreal)
result <- surreal_text("%s", R_squared = %.2f, p = %d)
model <- lm(y ~ ., data = result)
plot(model$fitted.values, model$residuals, pch = 20)',
        input$text, input$r_squared, input$p),

      "image" = sprintf('library(surreal)
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
        if (input$image_mode == "auto") "NULL" else sprintf("%.2f", input$threshold),
        input$max_points, input$r_squared, input$p)
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
          icon("copy"), " Copy to Clipboard"
        ),
        modalButton("Close")
      ),
      easyClose = TRUE
    ))
  })
}

shinyApp(ui, server)

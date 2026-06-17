
#' Create a responsive Bootstrap grid layout
#'
#' Arranges content items into a uniform responsive grid. The number of columns
#' at each Bootstrap breakpoint is specified as a named integer vector, without
#' needing to know Bootstrap's column width classes. Column counts must be
#' divisors of 12 (i.e. 1, 2, 3, 4, 6, or 12).
#'
#' @param ... Content items to arrange in the grid. Each item is wrapped in a
#'   column div automatically.
#' @param cols A named integer vector specifying the number of columns at each
#'   Bootstrap breakpoint. Names must be one or more of `"xs"`, `"sm"`,
#'   `"md"`, `"lg"`, `"xl"`, or `"xxl"`. For example,
#'   `c(xs = 1, md = 2, lg = 3)` produces a single-column layout on mobile,
#'   two columns on tablet, and three on desktop. Unspecified breakpoints
#'   inherit from the next smaller breakpoint. Column counts must be divisors
#'   of 12: 1, 2, 3, 4, 6, or 12.
#' @param gap The gutter size between grid items, as a Bootstrap spacing unit
#'   from `0` (no gap) to `5`. Controls both horizontal and vertical gaps.
#' @param class Additional CSS classes to apply to the outer row element.
#'
#' @returns An HTML tag that can be added to a UI definition.
#'
#' @seealso [grid_demo()]
#'
#' @export
#'
#' @examples
#' # No run
bs_grid <- function(..., cols = c(md = 2), gap = 3, class = NULL) {

  valid_breakpoints <- c("xs", "sm", "md", "lg", "xl", "xxl")
  valid_cols        <- c(1L, 2L, 3L, 4L, 6L, 12L)

  unknown <- setdiff(names(cols), valid_breakpoints)
  if (length(unknown) > 0)
    rlang::abort(paste0(
      "Unknown breakpoint(s): ", paste(unknown, collapse = ", "),
      ". Must be one of: ", paste(valid_breakpoints, collapse = ", ")
    ))

  invalid_counts <- setdiff(as.integer(cols), valid_cols)
  if (length(invalid_counts) > 0)
    rlang::abort(paste0(
      "Invalid column count(s): ", paste(invalid_counts, collapse = ", "),
      ". Must be a divisor of 12: ", paste(valid_cols, collapse = ", ")
    ))

  items <- rlang::dots_list(...)

  # Build per-item column classes: col-{width} or col-{bp}-{width}
  col_classes <- vapply(names(cols), function(bp) {
    n     <- as.integer(cols[[bp]])
    width <- 12L %/% n
    if (bp == "xs") paste0("col-", width) else paste0("col-", bp, "-", width)
  }, character(1))

  col_class <- paste(col_classes, collapse = " ")

  row_class <- paste(
    trimws(paste("row", paste0("g-", gap), if (!is.null(class)) class))
  )

  wrapped_items <- lapply(items, function(item) {
    htmltools::tags$div(class = col_class, item)
  })

  htmltools::tags$div(class = row_class, wrapped_items)
}


#' Interactively test the bs_grid layout
#'
#' Launches a Shiny app for interactively exploring [bs_grid()] layouts.
#' Controls allow adjusting the number of columns at each Bootstrap breakpoint
#' and the gutter size, with a live preview of the resulting grid and the
#' exact Bootstrap classes being applied.
#'
#' @returns Launches a Shiny app; called for its side effect.
#'
#' @seealso [bs_grid()]
#'
#' @export
#'
#' @examples
#' # No run
grid_demo <- function() {

  col_choices <- c("1", "2", "3", "4", "6", "12")

  placeholder_card <- function(n) {
    htmltools::tags$div(
      class = "card h-100",
      htmltools::tags$div(
        class = "card-body d-flex align-items-center justify-content-center",
        htmltools::tags$span(
          class = "text-muted fs-4",
          paste("Item", n)
        )
      )
    )
  }

  ui <- bslib::page_fluid(

    theme = bslib::bs_theme(preset = "bootstrap"),

    htmltools::tags$div(
      class = "container-fluid py-4",

      htmltools::h4("Grid Layout Demo"),
      htmltools::tags$p(
        class = "text-muted",
        "Adjust the controls to change the column layout at each breakpoint. ",
        "Resize the browser window to see breakpoints take effect."
      ),
      htmltools::hr(),

      htmltools::tags$div(
        class = "row g-4",

        # Controls panel
        htmltools::tags$div(
          class = "col-12 col-md-3",
          htmltools::tags$div(
            class = "card",
            htmltools::tags$div(class = "card-header fw-semibold", "Grid Controls"),
            htmltools::tags$div(
              class = "card-body",
              shiny::numericInput("n_items", "Number of items",
                                  value = 6, min = 1, max = 12, step = 1),
              htmltools::tags$hr(),
              htmltools::tags$p(class = "fw-semibold mb-1", "Columns per breakpoint"),
              htmltools::tags$p(
                class = "text-muted small mb-3",
                "Set — to inherit from the next smaller breakpoint."
              ),
              shiny::selectInput("cols_xs",  "xs  (< 576px)",    choices = c("—" = "0", col_choices), selected = "1"),
              shiny::selectInput("cols_sm",  "sm  (≥ 576px)",  choices = c("—" = "0", col_choices), selected = "0"),
              shiny::selectInput("cols_md",  "md  (≥ 768px)",  choices = c("—" = "0", col_choices), selected = "2"),
              shiny::selectInput("cols_lg",  "lg  (≥ 992px)",  choices = c("—" = "0", col_choices), selected = "3"),
              shiny::selectInput("cols_xl",  "xl  (≥ 1200px)", choices = c("—" = "0", col_choices), selected = "0"),
              shiny::selectInput("cols_xxl", "xxl (≥ 1400px)", choices = c("—" = "0", col_choices), selected = "0"),
              htmltools::tags$hr(),
              shiny::sliderInput("gap", "Gap", min = 0, max = 5, value = 3, step = 1, ticks = FALSE)
            )
          )
        ),

        # Preview panel
        htmltools::tags$div(
          class = "col-12 col-md-9",
          htmltools::tags$p(class = "mb-1 text-muted small", "Applied column classes:"),
          shiny::verbatimTextOutput("active_classes", placeholder = TRUE),
          htmltools::tags$hr(),
          shiny::uiOutput("grid_preview")
        )
      )
    )
  )

  server <- function(input, output, session) {

    active_cols <- shiny::reactive({
      result <- integer(0)
      if (!is.null(input$cols_xs)  && input$cols_xs  != "0") result["xs"]  <- as.integer(input$cols_xs)
      if (!is.null(input$cols_sm)  && input$cols_sm  != "0") result["sm"]  <- as.integer(input$cols_sm)
      if (!is.null(input$cols_md)  && input$cols_md  != "0") result["md"]  <- as.integer(input$cols_md)
      if (!is.null(input$cols_lg)  && input$cols_lg  != "0") result["lg"]  <- as.integer(input$cols_lg)
      if (!is.null(input$cols_xl)  && input$cols_xl  != "0") result["xl"]  <- as.integer(input$cols_xl)
      if (!is.null(input$cols_xxl) && input$cols_xxl != "0") result["xxl"] <- as.integer(input$cols_xxl)
      result
    })

# Show the exact col classes being applied to each item
    output$active_classes <- shiny::renderPrint({
      cols <- active_cols()
      if (length(cols) == 0) {
        cat("No breakpoints set")
      } else {
        classes <- vapply(names(cols), function(bp) {
          width <- 12L %/% as.integer(cols[[bp]])
          if (bp == "xs") paste0("col-", width) else paste0("col-", bp, "-", width)
        }, character(1))
        cat(paste(classes, collapse = " "))
      }
    })

    output$grid_preview <- shiny::renderUI({
      cols <- active_cols()
      n    <- as.integer(input$n_items)

      if (length(cols) == 0) {
        htmltools::tags$p(
          class = "text-muted",
          "Set at least one breakpoint to preview the grid."
        )
      } else {
        items <- lapply(seq_len(n), placeholder_card)
        do.call(bs_grid, c(items, list(cols = cols, gap = input$gap)))
      }
    })

  }

  shiny::shinyApp(ui = ui, server = server)
}

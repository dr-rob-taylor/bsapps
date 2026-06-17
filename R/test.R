library(shiny)
library(bslib)
library(htmltools)

ui <- page_fluid(
  theme = bs_theme(preset = "bootstrap"),
  tags$div(
    class = "row g-3 mt-2",
    tags$div(class = "col-12 col-md-6 col-lg-4", tags$div(class = "card card-body", "Item 1")),
    tags$div(class = "col-12 col-md-6 col-lg-4", tags$div(class = "card card-body", "Item 2")),
    tags$div(class = "col-12 col-md-6 col-lg-4", tags$div(class = "card card-body", "Item 3")),
    tags$div(class = "col-12 col-md-6 col-lg-4", tags$div(class = "card card-body", "Item 4")),
    tags$div(class = "col-12 col-md-6 col-lg-4", tags$div(class = "card card-body", "Item 5")),
    tags$div(class = "col-12 col-md-6 col-lg-4", tags$div(class = "card card-body", "Item 6"))
  )
)

shinyApp(ui, function(input, output, session) {})

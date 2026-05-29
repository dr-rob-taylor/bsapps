
test_containers <- function(){

  shiny::shinyApp(
    ui <- bslib::page_fluid(
      htmltools::div(
        class = "container",
        htmltools::div(
          class = "row",
          htmltools::div(class = "col-md-4", ".col-md-4", style = "border: 1px solid black;"),
          htmltools::div(class = "col-md-4", ".col-md-4", style = "border: 1px solid black;"),
          htmltools::div(class = "col-md-4", ".col-md-4", style = "border: 1px solid black;")
        )
      )
    ),

    server <- function(input, output, session){}
  )
}

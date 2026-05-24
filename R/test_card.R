
test_card <- function(){

  ui <- bslib::page_fluid(
    header = shinyjs::useShinyjs(),
    theme = bslib::bs_theme( version = 5, preset = "bootstrap"),
    title = "Bootstrap Cards",

    bslib::card(
      bslib::card_header(
        bslib::popover(
          shiny::uiOutput("card_title", inline = TRUE),
          title = "Provide a new title",
          shiny::textInput("card_title", NULL, "Edit this title")
        ),
        bslib::toolbar(
          align = "right",
          bslib::toolbar_input_button(
            id = "btn",
            label = "Menu button",
            icon = bsicons::bs_icon(
              title = "Edit",
              name = "box-arrow-in-down-left")
          )
        )
      ),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::actionButton("btn1", "A button"),
          shiny::actionButton("btn2", "A button"),
          position = "right",
          open = TRUE,
          title = "Sidebar"
        )
      )
    )
  )

  server <- function(input, output, session){

    settings_modal <- shiny::modalDialog(
        bslib::navset_pill_list(
          bslib::nav_panel(title = "One", p("First tab content.")),
          bslib::nav_panel(title = "Two", p("Second tab content.")),
          bslib::nav_panel(title = "Three", p("Third tab content")),
          well = FALSE,
          widths = c(3,9)
        ),
      size = "l"
    )

    output$card_title <- renderUI({
      list(
        input$card_title,
        bsicons::bs_icon("pencil-square")
      )
    })

    shiny::observeEvent(input$btn, {
      #shinyjs::toggle("div")
      shiny::showModal(settings_modal)
    })

  }

  shiny::shinyApp(ui = ui, server = server)
}

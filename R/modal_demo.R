
modal_demo <- function(){

  ui <- bslib::page_fluid(

    theme = bslib::bs_theme( preset = "bootstrap"),

      htmltools::div( class = "d-flex flex-row",
        htmltools::div(
             toggle_button(target_id = "modal_1", label = "Press",
                           toggle_type = "modal", type = "info",
                           outline = TRUE
                           ),
             bs_button( label = "A button" ),
             bs_button( label = "Disabled", disabled = TRUE),
             bs_button( label = "Danger!", type = "danger"),
             bs_button( label = "Small warning", size = "sm", type = "warning"),
             bs_button( label = "Large outline", size = "lg", outline = TRUE, type = "secondary")
           )
      ),
      modal_dialog(
        id = "modal_1",
        easy_close = FALSE,
        esc_close = FALSE,
        header = modal_header( title = "I am modal" ),
        body = modal_body( "Here me roar"),
        footer = modal_footer( toggle_button(target_id = "modal_2",
                                             label = "Next",
                                             toggle_type = "modal"
                                             )
                              ),
      ),
      modal_dialog(
        id = "modal_2",
        centered = TRUE,
        header = modal_header( title = "I am also modal", class = "text-warning"),
        body = modal_body( "Here me roar"),
        footer = modal_footer(
          toggle_button(target_id = "modal_1",
                        label = "Back to first",
                        toggle_type = "modal"
          ),
          toggle_button(target_id = "modal_3",
                        label = "Next",
                        toggle_type = "modal"
          )
        )
      ),
    modal_dialog(
      id = "modal_3",
      header = modal_header( title = "But I am also modal", class = "text-danger"),
      body = modal_body( "Here me roar")
    ),

  )

  server <- function(input, output, session){}

  shiny::shinyApp( ui = ui, server = server )
}


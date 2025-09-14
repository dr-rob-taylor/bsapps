
run_demo <- function()
{
  shiny::shinyApp(
    ui <- bslib::page_navbar(

      theme = bslib::bs_theme( version = 5, preset = "bootstrap"),
      title = "Bootstrap Applications",

      bslib::nav_panel(
        title = "Inputs",

        bslib::navset_card_tab(

          bslib::nav_panel(
            title = "Select",

            htmltools::tags$div(
              shiny::inputPanel(
                select_input("selectId",
                             label = "Choose a letter",
                             choices = letters),
                select_input("month", "Choose a month", choices = month.name),
                select_input("monthDisabled", "Choose a month", choices = month.name, disable = T)
              )
            )
          ),

          bslib::nav_panel(
            title = "Email",
            htmltools::div(
              shiny::wellPanel(
                htmltools::tags$div( class = "mb-3", email_input(id = "email", label = "Please Enter email", value = "Testing", placeholder = "name@example.com")),
                htmltools::tags$div( class = "mb-3", email_input(id = "emailFloat", label = "Enter email", floating = TRUE, value = NULL, placeholder = "name@example.com")),
                htmltools::tags$div( class = "mb-3", email_input(id = "myID", label = "Disabled email", placeholder = "name@example.com", disabled = TRUE)),
                htmltools::tags$div( class = "mb-3", email_input(id = "myOtherID", label = "This is read only", placeholder = "name@example.com", readonly = TRUE)),
                htmltools::tags$div( class = "mb-3", email_input(id = "myOtherOtherID", label = "This is also read only...", placeholder = "name@example.com", value = "...but with some warning text!", readonly = TRUE, class = "text-warning"))
              )
            ),
            # textOutput("myEmail")
          )
        )
      )
    ),

    server <- function(session, input, output){

    }
  )
}

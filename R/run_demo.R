
run_demo <- function()
{
  shiny::shinyApp(
    ui <- bslib::page_navbar(

      theme = bslib::bs_theme( preset = "bootstrap"),
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
      ),

      bslib::nav_panel(
        title = "Offcanvas",

        bslib::page_fluid(
          htmltools::h4("Offcanvas"),
          htmltools::hr(),
          htmltools::tags$div(
            toggle_button( id = "toggle", toggle_type = "offcanvas",
                           label = "Toggle offcanvas",
                           target_id = "my_canvas"),
            offcanvas_dialog( id = "my_canvas",
                               position = "left", body_scroll = TRUE, backdrop = TRUE,
                               header = offcanvas_header( title = "My Offcanvas"),
                               body = offcanvas_body(
                                 htmltools::tagList(
                                   htmltools::h6("Welcome to this offcanvas"),
                                   shiny::selectInput("canvas_input",
                                                      label = "Select a month",
                                                      choices = month.name )
                                 )
                               )
                             )
          ),
          htmltools::tags$div(
            class = "mt-3",
            shiny::wellPanel(
              shiny::textOutput("my_month")
            )
          )
        )
      ),

      bslib::nav_panel(
        title = "Collapse",
        bslib::page_fluid(
          htmltools::h4("Collapse"),
          htmltools::hr(),
          htmltools::tags$div(
            htmltools::tags$div( class = "mb-3",
              toggle_button( id = "toggle", toggle_type = "collapse",
                             label = "Toggle collapse",
                             target_id = "my_collapse")
            ),
            htmltools::tags$div(
              class = "collapse",
              id = "my_collapse",
              htmltools::tags$div(
                class = "card card-body",
                "This is some text in a card that you couldn't see initially."
              )
            )
          )
        )
      ),

      bslib::nav_panel(
        title = "Modals",
        bslib::page_fluid(
          htmltools::h4("Modals"),
          htmltools::hr(),
          shiny::inputPanel(
            toggle_button(target_id = "modal_1", label = "Update Info",
                          toggle_type = "modal")
          ),
          htmltools::tags$div(
            modal_dialog( id = "modal_1",
              header = modal_header("This is a modal!"),
              body = modal_body(shiny::h6("Here is some text"))
            )
        )
      )
      ),

      bslib::nav_panel(
        title = "Sidebar",
        bslib::page_sidebar(

          includeCSS(list.files(pattern = "sidebars.css", recursive = T)),

          sidebar = collapsible_item_sidebar(
            id = "sidebar",
            item_list = htmltools::tagList(
              collapsible_items(
                header = "Home",
                show = TRUE,
                sidebar_item(label = "Overview"),
                sidebar_item(label = "Updates"),
                sidebar_item(label = "Reports"),
              ),
              collapsible_items(
                header = "Dashboard",
                show = FALSE,
                sidebar_item(label = "Overview"),
                sidebar_item(label = "Weekly"),
                sidebar_item(label = "Monthly"),
              )
            )
          ),

          htmltools::tags$div(
            htmltools::h4("Sidebar"),
          )
        )
      ),

      bslib::nav_panel(
        title = "Buttons",
        bslib::page_fluid(
          htmltools::h4("Dropdown buttons"),
          htmltools::hr(),
            htmltools::tags$div(
              class = "row",
              htmltools::tagList(
              lapply(c("primary", "secondary", "success",
                       "info", "warning", "danger"), function(t){
                         htmltools::div(
                           class = "col-sm-2",
                           dropdown_button(
                             id = "dropdown",
                             label = "Dropdown",
                             type = t,
                             size = "m",
                             dropdown_item( label = "Action" ),
                             dropdown_item( label = "Another action" ),
                             dropdown_item( label = "Something else here" ),
                             dropdown_divider(),
                             dropdown_item( label = "Separated link" )
                           )
                         )
                       })
            )
          ),
          htmltools::tags$div(
            class = "row mt-3",
            htmltools::tagList(
              lapply(c("primary", "secondary", "success",
                       "info", "warning", "danger"), function(t){
                         htmltools::div(
                           class = "col-sm-2",
                           dropdown_button(
                             id = "dropdown",
                             label = "Dropdown",
                             type = t,
                             outline = TRUE,
                             dropdown_item( label = "Action" ),
                             dropdown_item( label = "Another action" ),
                             dropdown_item( label = "Something else here" ),
                             dropdown_divider(),
                             dropdown_item( label = "Separated link" )
                           )
                         )
                       })
            )
          ),
          htmltools::tags$div(
            class = "row mt-3",
            htmltools::tagList(
              lapply(c("primary", "secondary", "success",
                       "info", "warning", "danger"), function(t){
                         htmltools::div(
                           class = "col-sm-2",
                           dropdown_button(
                             id = "dropdown",
                             label = "Dropdown",
                             type = t,
                             split = TRUE,
                             dark = TRUE,
                             dropdown_item( label = "Action" ),
                             dropdown_item( label = "Another action" ),
                             dropdown_item( label = "Something else here" ),
                             dropdown_divider(),
                             dropdown_item( label = "Separated link" )
                           )
                         )
                       })
              )
            ),
            htmltools::tags$div(
              class = "row mt-3",
              htmltools::div(
                class = "col-sm-2",
                dropdown_button(
                  id = "dropdown",
                  label = "Dropdown",
                  direction = "right",
                  dropdown_item( label = "Action" ),
                  dropdown_item( label = "Another action" ),
                  dropdown_item( label = "Something else here" ),
              )
            )
          )
        )
      )
    ),

    server <- function(session, input, output){

      output$my_month <- shiny::renderText({

        msg <- paste("I chose the month", input$canvas_input, "from the offcanvas")
        msg

      })

    }
  )
}


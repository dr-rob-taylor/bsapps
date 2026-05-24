
modal_dialog <- function(id = NULL, header = NULL, body = NULL,
                         footer = modal_footer( modal_button("Close")),
                         fade = TRUE)
{
  htmltools::tags$div(
    class = paste0("modal", if( fade ) " fade"),
    id = id,
    tabindex = "-1",
    `aria-hidden` = "true",
    `aria-labelledby` = "modal_label",
    htmltools::tags$div(
      class = "modal-dialog",
      htmltools::tags$div(
        class = "modal-content",
        htmltools::tagList(
          header,
          body,
          footer
        )
      )
    )
  )
}

modal_header <- function( title = NULL )
{
  htmltools::tags$div(
    class = "modal-header",
    htmltools::tags$h5( class = "modal-title", id = "modal_label", title),
    close_button()
  )
}

modal_body <- function(..., id = NULL)
{
  htmltools::tags$div(
    id = id,
    class = "modal-body",
    ...
  )
}

modal_footer <- function(..., id = NULL, class = NULL){

  htmltools::tags$div(
    id = id,
    class = "modal-footer",
    ...
  )
}

modal_button <- function( label ){

  htmltools::tags$button(type = "button", class = "btn btn-default", `data-dismiss` = "modal",
                         `data-bs-dismiss` = "modal", label )
}


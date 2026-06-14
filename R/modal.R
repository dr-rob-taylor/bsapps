
modal_dialog <- function(id = NULL, header = NULL, body = NULL,
                         footer = modal_footer( modal_button(label = "Close")),
                         fade = TRUE, centered = FALSE, scrollable = FALSE,
                         easy_close = TRUE, esc_close = TRUE
                         )
{
  htmltools::tags$div(
    class = paste0("modal", if( fade ) " fade"),
    id = id,
    `data-bs-backdrop` = if(!easy_close) "static" else NULL,
    `data-bs-keyboard` = if(esc_close) "true" else "false",
    tabindex = "-1",
    `aria-hidden` = "true",
    `aria-labelledby` = "modal_label",
    htmltools::tags$div(
      class = paste0("modal-dialog",
                     if(centered) " modal-dialog-centered",
                     if(scrollable) " modal-dialog-scrollable"
      ),
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

modal_header <- function( id = NULL, title = NULL, class = NULL, close_button = TRUE)
{
  if(is.null(id)) id <- "modal_title"
  tag_title <- htmltools::tags$h5( class = "modal-title", id = id, title)

  if(!is.null(class))
    tag_title <- htmltools::tagAppendAttributes( tag_title, class = class)

  tag_header <- htmltools::tags$div(
                  class = "modal-header",
                  tag_title,
                  if(close_button) close_button()
                )

  return( tag_header )

}

modal_body <- function(..., id = NULL, class = NULL)
{
  body_tag <- htmltools::tags$div(
                id = id,
                class = "modal-body",
                rlang::dots_list(...)
              )
  if(!is.null(class))
    body_tag <- htmltools::tagAppendAttributes( body_tag, class = class )

  return( body_tag )

}

modal_footer <- function(..., id = NULL, class = NULL){

  tag_footer <- htmltools::tags$div(
                  id = id,
                  class = "modal-footer",
                  rlang::dots_list(...)
                )

  if(!is.null(class))
    tag_footer <- htmltools::tagAppendAttributes( tag_footer, class = class )

  return( tag_footer)

}

modal_button <- function( id = NULL, label, class = NULL){

  tag_button <- htmltools::tags$button( id = id,
                                        type = "button",
                                        class = "btn btn-default",
                                        `data-dismiss` = "modal",
                                        `data-bs-dismiss` = "modal",
                                        label )
  if(!is.null(class))
    tag_button <- htmltools::tagAppendAttributes( tag_button, class = class )

  return( tag_button )
}

close_button <- function()
{
  htmltools::tags$button(
    type = "button",
    class = "btn-close",
    `data-bs-dismiss` = "modal",
    `data-dismiss` = "modal",
    `aria-label` = "Close"
  )
}



#' Create a Bootstrap modal dialog
#'
#' Creates the outer container for a Bootstrap modal dialog. Use alongside
#' [modal_header()], [modal_body()], and [modal_footer()] to compose the
#' full modal, and [toggle_button()] with `toggle_type = "modal"` to trigger it.
#'
#' @param id A unique HTML id for the modal element, used to target it with a
#'   toggle button.
#' @param header A [modal_header()] tag, or `NULL` to omit.
#' @param body A [modal_body()] tag, or `NULL` to omit.
#' @param footer A [modal_footer()] tag. Defaults to a footer with a close button.
#' @param fade If `TRUE`, the modal animates in and out with a fade transition.
#' @param centered If `TRUE`, the modal is vertically centered on the screen.
#' @param scrollable If `TRUE`, the modal body becomes independently scrollable
#'   when its content overflows.
#' @param easy_close If `TRUE`, clicking the backdrop dismisses the modal.
#' @param esc_close If `TRUE`, pressing Escape dismisses the modal.
#'
#' @returns An HTML tag that can be added to a UI definition.
#'
#' @seealso [modal_header()], [modal_body()], [modal_footer()], [modal_button()],
#'   [toggle_button()]
#'
#' @export
#'
#' @examples
#' # No run
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

#' Create a Bootstrap modal header
#'
#' Creates the header section of a Bootstrap modal, containing a title and an
#' optional close button.
#'
#' @param id An HTML id for the title element. Defaults to `"modal_title"`.
#' @param title The text to display as the modal title.
#' @param class Additional CSS classes to apply to the title element, e.g.
#'   Bootstrap text utilities such as `"text-warning"`.
#' @param show_close If `TRUE`, a close (×) button is included in the header.
#'
#' @returns An HTML tag that can be passed to the `header` argument of
#'   [modal_dialog()].
#'
#' @seealso [modal_dialog()], [close_button()]
#'
#' @export
#'
#' @examples
#' # No run
modal_header <- function( id = NULL, title = NULL, class = NULL, show_close = TRUE)
{
  if(is.null(id)) id <- "modal_title"
  tag_title <- htmltools::tags$h5( class = "modal-title", id = id, title)

  if(!is.null(class))
    tag_title <- htmltools::tagAppendAttributes( tag_title, class = class)

  tag_header <- htmltools::tags$div(
                  class = "modal-header",
                  tag_title,
                  if(show_close) close_button()
                )

  return( tag_header )

}

#' Create a Bootstrap modal body
#'
#' Creates the body section of a Bootstrap modal to hold the main content.
#'
#' @param ... Content elements to include in the modal body.
#' @param id An optional HTML id for the body element.
#' @param class Additional CSS classes to apply to the body element.
#'
#' @returns An HTML tag that can be passed to the `body` argument of
#'   [modal_dialog()].
#'
#' @seealso [modal_dialog()]
#'
#' @export
#'
#' @examples
#' # No run
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

#' Create a Bootstrap modal footer
#'
#' Creates the footer section of a Bootstrap modal, typically used to hold
#' action buttons such as [modal_button()] or [toggle_button()].
#'
#' @param ... Content elements to include in the modal footer, typically buttons.
#' @param id An optional HTML id for the footer element.
#' @param class Additional CSS classes to apply to the footer element.
#'
#' @returns An HTML tag that can be passed to the `footer` argument of
#'   [modal_dialog()].
#'
#' @seealso [modal_dialog()], [modal_button()]
#'
#' @export
#'
#' @examples
#' # No run
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

#' Create a Bootstrap modal dismiss button
#'
#' Creates a button that dismisses the modal when clicked. Intended for use
#' inside [modal_footer()].
#'
#' @param id An optional HTML id for the button element.
#' @param label The text label for the button.
#' @param class Additional CSS classes to apply to the button element.
#'
#' @returns An HTML tag that can be passed to [modal_footer()].
#'
#' @seealso [modal_footer()], [modal_dialog()]
#'
#' @export
#'
#' @examples
#' # No run
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

#' Create a Bootstrap close button
#'
#' Creates a standard Bootstrap close (×) button that dismisses a modal when
#' clicked. Used internally by [modal_header()].
#'
#' @returns An HTML button tag.
#'
#' @seealso [modal_header()]
#'
#' @export
#'
#' @examples
#' # No run
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

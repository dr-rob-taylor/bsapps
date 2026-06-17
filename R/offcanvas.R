
#' Create a Bootstrap offcanvas dialog
#'
#' Creates the outer container for a Bootstrap offcanvas panel. Use alongside
#' [offcanvas_header()] and [offcanvas_body()] to compose the full panel, and
#' [toggle_button()] with `toggle_type = "offcanvas"` to trigger it.
#'
#' @param ... Currently unused. Reserved for future use.
#' @param id A unique HTML id for the offcanvas element, used to target it with
#'   a toggle button.
#' @param header An [offcanvas_header()] tag, or `NULL` to omit.
#' @param body An [offcanvas_body()] tag, or `NULL` to omit.
#' @param footer Optional footer content, or `NULL` to omit.
#' @param position Which edge of the viewport the panel slides in from. One of
#'   `"left"`, `"right"`, `"top"`, or `"bottom"`.
#' @param body_scroll If `TRUE`, the page body remains scrollable while the
#'   offcanvas panel is open.
#' @param backdrop If `TRUE`, a backdrop is shown behind the panel and clicking
#'   it closes the panel.
#'
#' @returns An HTML tag that can be added to a UI definition.
#'
#' @seealso [offcanvas_header()], [offcanvas_body()], [toggle_button()]
#'
#' @export
#'
#' @examples
#' # No run
offcanvas_dialog <- function(..., id, header = NULL, body = NULL, footer = NULL,
                              position = c("left", "right", "top", "bottom"),
                              body_scroll = FALSE, backdrop = TRUE
)
{

  position <- rlang::arg_match(position)
  class_position <- switch(position,
                           "left" = "offcanvas-start",
                           "right" = "offcanvas-end",
                           "top" = "offcanvas-top",
                           "bottom" = "offcanvas-bottom"
  )

  htmltools::tags$div( id = id,
                       class = paste("offcanvas", class_position),
                       tabindex = "-1",
                       `aria-labelledby` = id,
                       `data-bs-scroll` = if(isTRUE(body_scroll)) "true" else NULL,
                       `data-bs-backdrop` = if(isFALSE(backdrop)) "false" else NULL,
                       htmltools::tagList(
                         header,
                         body,
                         footer
                       )
  )
}

#' Create a Bootstrap offcanvas header
#'
#' Creates the header section of a Bootstrap offcanvas panel, containing a
#' title and a close button.
#'
#' @param ... Currently unused. Reserved for future use.
#' @param id An optional HTML id for the title element.
#' @param title The text to display as the offcanvas panel title.
#'
#' @returns An HTML tag that can be passed to the `header` argument of
#'   [offcanvas_dialog()].
#'
#' @seealso [offcanvas_dialog()]
#'
#' @export
#'
#' @examples
#' # No run
offcanvas_header <- function(..., id = NULL, title = NULL)
{
  htmltools::tags$div( class = "offcanvas-header",
                       shiny::h5( id = id, class = "offcanvas-title", title),
                       htmltools::tags$button( type = "button", class = "btn-close text-reset",
                                               `data-bs-dismiss` = "offcanvas", `aria-label` = "Close"
                       )
  )
}

#' Create a Bootstrap offcanvas body
#'
#' Creates the body section of a Bootstrap offcanvas panel to hold the main
#' content.
#'
#' @param ... Content elements to include in the offcanvas body.
#' @param id An optional HTML id for the body element.
#' @param class Additional CSS classes to apply to the body element.
#'
#' @returns An HTML tag that can be passed to the `body` argument of
#'   [offcanvas_dialog()].
#'
#' @seealso [offcanvas_dialog()]
#'
#' @export
#'
#' @examples
#' # No run
offcanvas_body <- function(..., id = NULL, class = NULL)
{
  htmltools::tags$div( id = id, class = "offcanvas-body",
                       ...
  )
}

#' Create a Bootstrap offcanvas toggle button
#'
#' Creates a button that opens an offcanvas panel when clicked. For a more
#' flexible alternative that also supports collapse and modal toggling, see
#' [toggle_button()].
#'
#' @param target_id The HTML id of the [offcanvas_dialog()] to toggle.
#' @param id An optional HTML id for the button element.
#' @param label The text label for the button.
#' @param class Additional CSS classes to apply to the button element.
#' @param toggle_type Currently unused. Reserved for future use.
#'
#' @returns An HTML button tag that can be added to a UI definition.
#'
#' @seealso [offcanvas_dialog()], [toggle_button()]
#'
#' @export
#'
#' @examples
#' # No run
offcanvas_button <- function(target_id, id = NULL, label = NULL, class = NULL,
                             toggle_type = c("offcanvas", "collapse")
)
{
  htmltools::tags$button( class = "btn btn-primary",
                          type = "button",
                          `data-bs-toggle` = "offcanvas",
                          `data-bs-target` = paste0("#", target_id),
                          `aria-controls` = target_id,
                          label
  )
}


#' Create a Bootstrap button
#'
#' Creates a styled Bootstrap button. This is the core button constructor used
#' by [toggle_button()] and [action_button()].
#'
#' @param id An optional HTML id for the button element.
#' @param label The text label for the button.
#' @param class Additional CSS classes to apply to the button element.
#' @param icon Currently unused. Reserved for future icon support.
#' @param type The Bootstrap contextual colour of the button. One of
#'   `"primary"`, `"secondary"`, `"success"`, `"info"`, `"warning"`, or
#'   `"danger"`.
#' @param outline If `TRUE`, renders an outline variant of the button style.
#' @param size The size of the button. One of `"md"` (default), `"sm"`
#'   (small), or `"lg"` (large).
#' @param disabled If `TRUE`, the button is rendered in a disabled state and
#'   cannot be clicked.
#'
#' @returns An HTML button tag that can be added to a UI definition.
#'
#' @seealso [toggle_button()], [action_button()]
#'
#' @export
#'
#' @examples
#' # No run
bs_button <- function(id = NULL, label = NULL, class = NULL, icon = NULL,
                      type = c("primary", "secondary", "success", "info",
                               "warning", "danger"), outline = FALSE,
                      size = c("md", "sm", "lg"), disabled = FALSE
                      ){

  value <- shiny::restoreInput(id = id, default = NULL)

  type <- rlang::arg_match(type)
  size <- rlang::arg_match(size)

  tag_button <- htmltools::tags$button(
    id = id,
    class = "btn",
    type = "button",
    disabled = if(isTRUE(disabled)) NA else NULL,
    `data-val` = value,
    htmltools::span(label, .noWS = c("outside", "inside"))
  )

  size_class <- NULL
  if( size %in% c("sm", "lg")){
    size_class <- switch(size,
                         "sm" = "btn-sm",
                         "lg" = "btn-lg"
                         )
  }

  btn_class <- switch(type,
                      "primary" = if(outline) "btn-outline-primary" else "btn-primary",
                      "secondary" = if(outline) "btn-outline-secondary" else "btn-secondary",
                      "success" = if(outline) "btn-outline-success" else "btn-success",
                      "info" = if(outline) "btn-outline-info" else "btn-info",
                      "warning" = if(outline) "btn-outline-warning" else "btn-warning",
                      "danger" = if(outline) "btn-outline-danger" else "btn-danger"
  )

  tag_button <- htmltools::tagAppendAttributes( tag_button, class = c(btn_class, size_class) )

  return( tag_button )

}

#' Create a Bootstrap toggle button
#'
#' Creates a Bootstrap button that toggles an offcanvas panel, a collapsible
#' element, or a modal dialog. Inherits all styling arguments from [bs_button()].
#'
#' @param target_id The HTML id of the element to toggle.
#' @param toggle_type The Bootstrap component to toggle. One of `"offcanvas"`,
#'   `"collapse"`, or `"modal"`.
#' @param id An optional HTML id for the button element.
#' @param label The text label for the button.
#' @param class Additional CSS classes to apply to the button element.
#' @param icon Currently unused. Reserved for future icon support.
#' @param type The Bootstrap contextual colour of the button. One of
#'   `"primary"`, `"secondary"`, `"success"`, `"info"`, `"warning"`, or
#'   `"danger"`.
#' @param outline If `TRUE`, renders an outline variant of the button style.
#' @param size The size of the button. One of `"md"` (default), `"sm"`
#'   (small), or `"lg"` (large).
#' @param disabled If `TRUE`, the button is rendered in a disabled state and
#'   cannot be clicked.
#' @param ... Currently unused.
#'
#' @returns An HTML button tag that can be added to a UI definition.
#'
#' @seealso [bs_button()], [offcanvas_dialog()], [modal_dialog()]
#'
#' @export
#'
#' @examples
#' # No run
toggle_button <- function(target_id,
                          toggle_type = c("offcanvas", "collapse", "modal"),
                          ... ){

  toggle_type <- rlang::arg_match(toggle_type)
  target_ref  <- paste0("#", target_id)

  # Capture full call including defaults, strip toggle-specific args
  btn_args <- as.list(match.call(expand.dots = TRUE))[-1]
  btn_args <- btn_args[!names(btn_args) %in% c("target_id", "toggle_type")]

  tag_button <- do.call(bs_button, btn_args)

  tag_button <- htmltools::tagAppendAttributes(
    tag_button,
    `data-toggle` = toggle_type,
    `data-target` = target_ref,
    `data-bs-toggle` = toggle_type,
    `data-bs-target` = target_ref,
    `aria-controls` = target_id,
  )

  return( tag_button )
}

formals(toggle_button) <- c(
  formals(toggle_button)[c("target_id", "toggle_type")],  # keep original toggle args
  formals(bs_button),                                       # inherit button args
  list(... = quote(expr=))
)


#' Create a Bootstrap action button
#'
#' Creates a Bootstrap-styled button that integrates with Shiny's reactivity
#' system. Equivalent to [shiny::actionButton()] but uses [bs_button()] for
#' styling, giving access to all Bootstrap button variants, sizes, and the
#' outline style.
#'
#' @param id An optional HTML id for the button element.
#' @param label The text label for the button.
#' @param class Additional CSS classes to apply to the button element.
#' @param icon Currently unused. Reserved for future icon support.
#' @param type The Bootstrap contextual colour of the button. One of
#'   `"primary"`, `"secondary"`, `"success"`, `"info"`, `"warning"`, or
#'   `"danger"`.
#' @param outline If `TRUE`, renders an outline variant of the button style.
#' @param size The size of the button. One of `"md"` (default), `"sm"`
#'   (small), or `"lg"` (large).
#' @param disabled If `TRUE`, the button is rendered in a disabled state and
#'   cannot be clicked.
#' @param ... Currently unused.
#'
#' @returns An HTML button tag that can be added to a UI definition.
#'
#' @seealso [bs_button()], [shiny::actionButton()]
#'
#' @export
#'
#' @examples
#' # No run
action_button <- function(...){

  btn_args   <- as.list(match.call(expand.dots = TRUE))[-1]
  tag_button <- do.call(bs_button, btn_args)

  tag_button <- htmltools::tagAppendAttributes(
    tag_button, class = "action-button" )

  if(!is.null(btn_args$label))
    tag_button <- htmltools::tagAppendAttributes(tag_button, class = "action-label", .cssSelector = "span")

  return( tag_button )
}

formals(action_button) <- c(
  formals(bs_button),
  list(... = quote(expr=))
)


#' Create a Bootstrap dropdown button
#'
#' Creates a Bootstrap dropdown button with a menu of items. Supports all
#' Bootstrap contextual colours, outline variants, split buttons, dark menus,
#' and directional drop positioning.
#'
#' @param ... [dropdown_item()], [dropdown_header()], and [dropdown_divider()]
#'   elements to include in the dropdown menu.
#' @param id A unique HTML id for the dropdown container element.
#' @param label The text label for the dropdown toggle button.
#' @param class Additional CSS classes to apply to the container element.
#' @param split If `TRUE`, renders a split button where the label and the
#'   dropdown arrow are separate clickable elements.
#' @param outline If `TRUE`, renders an outline variant of the button style.
#' @param dark If `TRUE`, renders the dropdown menu with a dark background.
#' @param size The size of the button. One of `"m"` (default), `"s"` (small),
#'   or `"l"` (large).
#' @param type The Bootstrap contextual colour of the button. One of
#'   `"primary"`, `"secondary"`, `"success"`, `"info"`, `"warning"`, or
#'   `"danger"`.
#' @param direction The direction the menu opens relative to the button. One of
#'   `"down"` (default), `"up"`, `"left"`, or `"right"`.
#'
#' @returns An HTML tag that can be added to a UI definition.
#'
#' @seealso [dropdown_item()], [dropdown_header()], [dropdown_divider()]
#'
#' @export
#'
#' @examples
#' # No run
dropdown_button <- function(..., id, label = NULL, class = NULL, split = FALSE,
                            outline = FALSE,  dark = FALSE,
                            size = c("m", "s", "l"),
                            type = c("primary", "secondary", "success", "info",
                                     "warning", "danger"),
                            direction = c("down", "up", "left", "right") ){

  type <- rlang::arg_match(type)
  size <- rlang::arg_match(size)
  direction <- rlang::arg_match(direction)

  item_list <- rlang::dots_list(...)

  size_class <- switch(size,
                       "m" = "btn-default",
                       "s" = "btn-sm",
                       "l" = "btn-lg"
                       )

  btn_class <- switch(type,
                      "primary" = if(outline) "btn-outline-primary" else "btn-primary",
                      "secondary" = if(outline) "btn-outline-secondary" else "btn-secondary",
                      "success" = if(outline) "btn-outline-success" else "btn-success",
                      "info" = if(outline) "btn-outline-info" else "btn-info",
                      "warning" = if(outline) "btn-outline-warning" else "btn-warning",
                      "danger" = if(outline) "btn-outline-danger" else "btn-danger"
                      )

  direction_class <- switch(direction,
                            "down" = NULL,
                            "up" = "dropup",
                            "left" = "dropstart",
                            "right" = "dropend"
                            )

  htmltools::tags$div(
    id = id,
    class = paste("btn-group", direction_class),
    if(split) htmltools::tags$button( class = paste("btn", btn_class),
                                      type = "button", label),
    htmltools::tags$button(
      class = paste("btn", btn_class, size_class, "dropdown-toggle", if(split) "dropdown-toggle-split"),
      type = "button",
      `data-bs-toggle` = "dropdown",
      `aria-expanded` = FALSE,
      if(!split) label else htmltools::tags$span( class = "visually-hidden", "Toggle dropdown")
    ),
    htmltools::tags$ul(
      class = paste("dropdown-menu", if(dark) "dropdown-menu-dark"),
      item_list
    )
  )
}

#' Create a Bootstrap dropdown menu item
#'
#' Creates a single clickable item for use inside [dropdown_button()].
#'
#' @param input_id Currently unused. Reserved for future Shiny input integration.
#' @param label The text label for the menu item.
#' @param href An optional URL the item links to. Defaults to `"#"`.
#' @param disabled If `TRUE`, the item is rendered in a disabled, non-clickable
#'   state.
#' @param active If `TRUE`, the item is highlighted as the currently active
#'   selection.
#'
#' @returns An HTML list item tag for use inside [dropdown_button()].
#'
#' @seealso [dropdown_button()], [dropdown_header()], [dropdown_divider()]
#'
#' @export
#'
#' @examples
#' # No run
dropdown_item <- function(input_id, label = NULL, href = NULL, disabled = FALSE, active = FALSE){

  if(is.null(href)) href = "#"

  htmltools::tags$li(
    htmltools::tags$a(
      class = paste("dropdown-item", if(disabled) "disabled", if(active) "active"),
      href = href,
      label
    )
  )
}

#' Create a Bootstrap dropdown menu header
#'
#' Creates a non-interactive header label to group related items inside a
#' [dropdown_button()] menu.
#'
#' @param label The text to display as the group header.
#'
#' @returns An HTML list item tag for use inside [dropdown_button()].
#'
#' @seealso [dropdown_button()], [dropdown_item()], [dropdown_divider()]
#'
#' @export
#'
#' @examples
#' # No run
dropdown_header <- function(label){
  htmltools::tags$li(
    htmltools::h6( class = "dropdown-header", label)
  )
}

#' Create a Bootstrap dropdown menu divider
#'
#' Creates a horizontal rule to visually separate groups of items inside a
#' [dropdown_button()] menu.
#'
#' @returns An HTML list item tag for use inside [dropdown_button()].
#'
#' @seealso [dropdown_button()], [dropdown_item()], [dropdown_header()]
#'
#' @export
#'
#' @examples
#' # No run
dropdown_divider <- function(){
  htmltools::tags$li(
    htmltools::tags$hr(
      class = "dropdown-divider"
    )
  )
}

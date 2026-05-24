
#' Dropdown button
#'
#' @param ...
#' @param id
#' @param label
#' @param class
#' @param split
#' @param outline
#' @param dark
#' @param size
#' @param type
#' @param direction
#'
#' @returns
#' @export
#'
#' @examples
#'
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

dropdown_header <- function(label){
  htmltools::tags$li(
    htmltools::h6( class = "dropdown-header", label)
  )
}

dropdown_divider <- function(){
  htmltools::tags$li(
    htmltools::tags$hr(
      class = "dropdown-divider"
    )
  )
}


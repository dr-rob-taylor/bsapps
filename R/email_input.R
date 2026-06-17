
#' Create an email input control
#'
#' Creates an input control for entry of email addresses. Supports Bootstrap
#' sizing, floating labels, disabled and read-only states, and control over
#' when the value is sent to the Shiny server.
#'
#' @param id The input slot that will be used to access the value.
#' @param label Display label for the control, or `NULL` for no label.
#' @param value Initial value of the input field.
#' @param floating If `TRUE`, the label floats over the input field when it has
#'   focus or a value (Bootstrap floating label style).
#' @param placeholder Placeholder text shown when the field is empty. When
#'   `floating = TRUE` and no placeholder is supplied, defaults to
#'   `"Enter email"`.
#' @param size Controls the size of the input field. One of `"m"` (default),
#'   `"s"` (small), or `"l"` (large).
#' @param disabled If `TRUE`, the input is shaded out and cannot be modified.
#' @param class Additional CSS classes to apply to the input element, e.g.
#'   Bootstrap utility classes such as `"text-warning"`.
#' @param readonly If `TRUE`, the input cannot be modified but is not greyed
#'   out and its value is still submitted.
#' @param update_on Controls when the input value is sent to the Shiny server.
#'   `"change"` sends on every keystroke; `"blur"` sends only when the field
#'   loses focus.
#'
#' @returns An email input control that can be added to a UI definition.
#'
#' @seealso [shiny::textInput()]
#'
#' @export
#'
#' @examples
#' # No run
email_input <- function(id, label = NULL, value = NULL, floating = FALSE,
                        placeholder = NULL,
                        size = c("m", "s", "l"), disabled = FALSE, class = NULL,
                        readonly = FALSE, update_on = c("change", "blur")
                        )
{
  size <- rlang::arg_match(size)
  update_on <- rlang::arg_match(update_on)
  value <- shiny::restoreInput(id = id, default = value)

  classes <- switch(size,
                    "s" = "form-control form-control-sm",
                    "m" = "form-control",
                    "l" = "form-control form-control-lg"
  )

  if(!is.null(class)) classes <- c(classes, class)
  if(floating && is.null(placeholder) ) placeholder <- "Enter email"

  tag_label <- htmltools::tags$label(`for` = id, class = "form-label", label)
  tag_input <- htmltools::tags$input(type = "email",
                          id = id,
                          class = c(classes, "shiny-input-text"),
                          value = value,
                          placeholder = placeholder,
                          disabled = if(isTRUE(disabled)) NA else NULL,
                          readonly = if(isTRUE(readonly)) NA else NULL,
                          `data-update-on` = update_on )

  htmltools::tagList(
    htmltools::tags$div( class = if(floating) "form-floating",
      if(floating){
        htmltools::tagList( tag_input, tag_label )
      } else {
        htmltools::tagList( tag_label, tag_input )
      }
    )
  )
}

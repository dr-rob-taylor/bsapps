
#' Create a email input control
#'
#' Create an input control for entry of unstructured text values for email
#'
#' @inheritParams shiny::textInput
#' @param id The input slot that will be used to access the value.
#' @param floating If true, the label is floated over the input field.
#' @param size Controls the size of the input field.
#' @param disabled If true, the input field is shaded out and cannot be modified.
#' @param class Additional Bootstrap utilities to manipulate the input control.
#' @param readonly If true, the input field cannot be modified but appears active.
#' @param update_on something....
#'
#' @returns
#' An email input control that can be added to a UI definition.
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

  

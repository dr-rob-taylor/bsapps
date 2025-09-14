
#' Create a select list form control
#'
#' A wrapper around `inputSelect` that creates a select list to choose a
#' single or multiple items from a list of values. Includes an option to
#' `disable` the input.
#'
#' @inheritParams shiny::selectInput
#' @param id The input slot that will be used to access the value.
#' @param disable Boolean toggle to disable/enable the input.
#'
#' @returns
#' A select list control that can be added to a UI definition.
#' @seealso
#' [shiny::selectInput()]
#'
#' @export
#'
#' @examples
#' # Do not run
#'
select_input <- function(id, label, choices, selected = NULL, multiple = FALSE,
                         selectize = TRUE, width = NULL, size = NULL, disable = FALSE )
{
  select_tag <- shiny::selectInput(inputId = id, label = label, choices = choices,
                                   selected = selected, multiple = multiple, selectize = selectize,
                                   width = width, size = size )

  select_tag <- htmltools::tagAppendAttributes( select_tag,
                                                .cssSelector = "select",
                                                disabled = if(isTRUE(disable)) NA else NULL)

  htmltools::tagList( htmltools::tags$div( select_tag ) )
}

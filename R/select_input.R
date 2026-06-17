
#' Create a select list input control
#'
#' A wrapper around [shiny::selectInput()] that creates a select list to choose
#' a single or multiple items from a list of values, with an added `disable`
#' option not available in the base function.
#'
#' @inheritParams shiny::selectInput
#' @param id The input slot that will be used to access the value.
#' @param label Display label for the control.
#' @param choices List of values to select from. If elements of the list are
#'   named, those names are used as the displayed labels.
#' @param selected The initially selected value. If `NULL`, defaults to the
#'   first element of `choices` (or no selection for `multiple = TRUE`).
#' @param multiple If `TRUE`, multiple items can be selected at once.
#' @param selectize If `TRUE`, the select widget is enhanced using the
#'   selectize.js library.
#' @param width The width of the input, e.g. `"200px"` or `"50%"`.
#' @param size Number of items to show in the list box when `selectize = FALSE`.
#' @param disable If `TRUE`, the input is shaded out and cannot be modified.
#'
#' @returns A select list control that can be added to a UI definition.
#'
#' @seealso [shiny::selectInput()]
#'
#' @export
#'
#' @examples
#' # No run
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

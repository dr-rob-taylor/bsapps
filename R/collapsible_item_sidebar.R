

collapsible_item_sidebar <- function(id = NULL, item_list){

  htmltools::tags$div(
    id = id,
    class = "flex-shrink-0 p-3 bg-white",
    style = "width: 280px;",

    htmltools::tags$ul(
      class = "list-unstyled ps-0",
      item_list
    )
  )
}

collapsible_items <- function(header = NULL, show = TRUE, ...){

  htmltools::tags$li(
    class="mb-1",
    htmltools::tags$button(
      class="btn btn-toggle align-items-center rounded collapsed",
      `data-bs-toggle` = "collapse",
      `data-bs-target` = paste0("#", header, "-collapse"),
      `aria-expanded` = show,
      header
    ),
    htmltools::tags$div(
      id = paste0(header, "-collapse"),
      class = paste("collapse",  if(show) "show"),
      htmltools::tags$ul(
        class="btn-toggle-nav list-unstyled fw-normal pb-1 small",
        rlang::dots_list(...)
      )
    )
  )
}


sidebar_item <- function(id = NULL, label = NULL, icon = NULL){
  htmltools::tags$li(
    shiny::actionLink(
      inputId = id,
      label = label,
      class = "link-dark rounded"
    )
  )
}


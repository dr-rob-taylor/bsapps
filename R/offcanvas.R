
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

offcanvas_header <- function(..., id = NULL, title = NULL)
{
  htmltools::tags$div( class = "offcanvas-header",
                       shiny::h5( id = id, class = "offcanvas-title", title),
                       htmltools::tags$button( type = "button", class = "btn-close text-reset",
                                               `data-bs-dismiss` = "offcanvas", `aria-label` = "Close"
                       )
  )
}

offcanvas_body <- function(..., id = NULL, class = NULL)
{
  htmltools::tags$div( id = id, class = "offcanvas-body",
                       ...
  )
}

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

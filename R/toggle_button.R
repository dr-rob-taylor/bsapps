
toggle_button <- function( target_id, id = NULL, label = NULL, class = NULL,
                           toggle_type = c("offcanvas", "collapse", "modal")
                          )
{
  toggle_type <- rlang::arg_match(toggle_type)

  htmltools::tags$button( class = "btn btn-primary",
                          type = "button",
                          `data-bs-toggle` = toggle_type,
                          `data-bs-target` = paste0("#", target_id),
                          `aria-controls` = target_id,
                          label
  )
}

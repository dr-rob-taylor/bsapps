
close_button <- function()
{
  htmltools::tags$button(
    type = "button",
    class = "btn-close",
    `data-bs-dismiss` = "modal",
    `data-dismiss` = "modal",
    `aria-label` = "Close"
  )
}

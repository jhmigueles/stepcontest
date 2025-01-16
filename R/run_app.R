# Launch Shiny App
#'
#' @return nothing, run the app.
#' @export
#' @import shiny
run_app <- function() {
  options(shiny.maxRequestSize = 1024 * 1024^2)  # Set upload limit to 1TB
  shiny::shinyApp(
    ui = app_ui(),
    server = app_server
  )
}
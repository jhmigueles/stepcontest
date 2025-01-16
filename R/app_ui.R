#' User interface
#'
#' @return
#' @export
#'
#' @import shiny
#' @import plotly
#' @import shinythemes
app_ui <- function() {
  shiny::fluidPage(
    theme = shinythemes::shinytheme("flatly"),  # Example theme
    shiny::titlePanel("Concurso de Pasos"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput("file", "Sube los archivos aquí",
                         multiple = TRUE,
                         accept = c(".csv", ".gt3x", ".bin", ".cwa")),
        shiny::actionButton("process", "¡Calcular pasos!"),
        shiny::hr()
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          # shiny::tabPanel("Summary", 
          #                 shiny::tableOutput("summary_table")),
          shiny::tabPanel("¿Qué equipo camina más?", 
                          plotly::plotlyOutput("bar_plot")),
          shiny::tabPanel("¿Qué equipo camina más rápido?", 
                          plotly::plotlyOutput("cadence_plot")),
          shiny::tabPanel("Clasificación", 
                          shiny::tableOutput("leaderboard"))
        )
      )
    )
  )
}
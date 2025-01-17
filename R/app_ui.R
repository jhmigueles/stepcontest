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
    ),
    # Footer
    tags$footer(
      HTML('Developed by <a href="https://www.jhmigueles.com" target="_blank" style="color: blue; text-decoration: none;">jhmigueles</a>'),
      align = "center",
      style = "
      position: absolute;
      bottom: 0;
      width: 100%;
      height: 50px;
      color: white;
      background-color: #95A5A6;
      padding: 10px;
      font-size: 12px;"
    )
  )
}
#' User interface
#'
#' @return
#' @export
#'
#' @import shiny
#' @import plotly
#' @import shinythemes
#' @import DT
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
          shiny::tabPanel("Número de pasos", 
                          plotly::plotlyOutput("bar_plot")),
          shiny::tabPanel("Minutos en intensidad moderada", 
                          plotly::plotlyOutput("Modcadence_plot")),
          shiny::tabPanel("Minutos en intensidad vigorosa", 
                          plotly::plotlyOutput("Vigcadence_plot")),
          shiny::tabPanel("Clasificaciones",
                          DT::DTOutput("ranks"))
        )
      )
    ),
    # Footer
    tags$footer(
      HTML('Developed by Jairo H. Migueles at <a href="https://www.jhmigueles.com" target="_blank" style="color: blue; text-decoration: none;">jhmigueles</a>'),
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
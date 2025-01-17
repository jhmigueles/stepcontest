#' Server
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' @export
#'
#' @import shiny
#' @import ggplot2
#' @import dplyr
app_server <- function(input, output, session) {
  require(ggplot2)
  require(plotly)
  require(dplyr)
  ds <- reactiveVal(NULL)
  ps <- reactiveVal(NULL)
  
  # Create a dedicated directory for GGIR output
  ggir_temp_dir <- file.path(tempdir(), "GGIR_output")
  dir.create(ggir_temp_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Clean up temporary files on app close
  onStop(function() {
    if (dir.exists(ggir_temp_dir)) {
      unlink(ggir_temp_dir, recursive = TRUE)
    }
  })
  
  observeEvent(input$process, {
    req(input$file)
    
    # Use withProgress to display a processing message
    withProgress(message = "Procesando los datos", value = 0, {
      # Increment the progress bar
      incProgress(0.2, detail = "Preparando los datos...")
      
      # Create a dedicated directory for storing uploaded files with original names
      upload_dir <- file.path(tempdir(), "uploaded_files")
      dir.create(upload_dir, recursive = TRUE, showWarnings = FALSE)
      
      # Copy uploaded files to the upload_dir with their original names
      uploaded_files <- lapply(seq_len(nrow(input$file)), function(i) {
        original_name <- input$file$name[i]
        temp_path <- input$file$datapath[i]
        new_path <- file.path(upload_dir, original_name)
        file.copy(temp_path, new_path)
        return(new_path)
      })
      
      # Increment progress bar
      incProgress(0.4, detail = "Analizando... Esto puede tardar varios minutos.")
      
      # revised parameters in Rowlands et al.
      step_count = list(FUN = count_steps,
                        parameters = c(4, 4, 20, -1.0, 4, 4, 0.01, 1.25),
                        expected_sample_rate = 15,
                        expected_unit = "g",
                        colnames = c("steps"),
                        outputres = 1,
                        minlength = 1,
                        outputtype = "numeric",
                        aggfunction = sum,
                        timestamp = F,
                        reporttype = "event")
      GGIR::GGIR(
        datadir = uploaded_files,  # Directory with uploaded files
        outputdir = ggir_temp_dir,                # Temporary output directory
        studyname = "stepcontest",
        idloc = 6, 
        mode = 1:2,                              # Only mode 1 is needed for step and cadence
        do.cal = FALSE,                        # to speed up the process
        myfun = step_count,
        includedaycrit = 2,
        do.report = 2)
      
      # Increment progress bar
      incProgress(0.7, detail = "Finalizando resultados...")
      
      # in case output is not generated...
      if (!dir.exists(ggir_temp_dir)) {
        showNotification("Error: Summary file not found. Please check your input files.", type = "error")
        return()
      }
      
      # Load processed GGIR output
      ggir_output_dir = dir(ggir_temp_dir, full.names = T)[1]
      stepmetrics::step.metrics(datadir = ggir_output_dir,
                                outputdir = file.path(ggir_output_dir, "results"),
                                cadence_MOD = 100, cadence_VIG = 140,
                                cadence_bands = c(0, 100, 140, Inf),
                                idloc = ".")
      summary_files <- list.files(file.path(ggir_temp_dir, "output_stepcontest", "results", "daySummary"), full.names = T)
      summary_data <-  do.call(rbind, lapply(summary_files, read.csv))
      
      # Update the reactive value
      ds(summary_data)
      PS = aggregate(stepsperday ~ ID, FUN = sum, data = ds()[, c("ID", "stepsperday")])
      ps(PS)
      
      # Complete progress bar
      incProgress(1, detail = "Hecho!")
    })
  })
  
  # output$summary_table <- renderTable({
  #   req(ds())
  #   data()
  # })
  
  output$bar_plot <- renderPlotly({
    req(ds())
    # Create the ggplot2 plot
    p <- ggplot(ds(), aes(x = weekday, y = stepsperday, fill = ID)) +
      geom_bar(stat = "identity", width = 0.6) + 
      coord_flip() +  
      labs(title = "Pasos Totales", x = "", y = "Pasos Totales") +
      labs(fill = "Equipo") +  # Cambiar el título de la leyenda
      theme_minimal() + 
      theme(
        legend.position = "bottom",
        legend.margin = margin(t = 15),        # Space between plot and legend
        legend.box.margin = margin(t = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 10)
      )
    
    # Convert ggplot2 plot to plotly
    ggplotly(p) %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.3,               # Add extra spacing below the plot
          yanchor = "top",
          tracegroupgap = 10
        ),
        margin = list(b = 100)    # Add extra margin to the bottom of the plot
      )
  })
  
  output$cadence_plot <- renderPlotly({
    req(ds())
    
    # Create the ggplot2 plot
    p <- ggplot(ds(), aes(x = ID)) +
      geom_bar(aes(x = weekday, y = CAD_band_100_139_spm, group = ID, fill = "Cadencia Moderada"), stat = "identity", width = 0.6) +
      geom_bar(aes(x = weekday, y = CAD_band_140_Inf_spm, group = ID, fill = "Cadencia Alta"), stat = "identity", width = 0.6) +
      coord_flip() +
      labs(title = "Tiempo acumulado en cadencias", x = "", y = "Minutos") +
      scale_fill_manual(values = c("Cadencia Moderada" = "blue", "Cadencia Alta" = "red")) +
      labs(fill = "Equipo") +  
      theme_minimal() + 
      theme(
        legend.position = "bottom",
        legend.margin = margin(t = 15),        # Space between plot and legend
        legend.box.margin = margin(t = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 10)
      )
    
    # Convert ggplot2 plot to plotly
    ggplotly(p) %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.3,               # Add extra spacing below the plot
          yanchor = "top",
          tracegroupgap = 10
        ),
        margin = list(b = 100)    # Add extra margin to the bottom of the plot
      )
  })
  
  output$leaderboard <- renderTable({
    req(ps())
    leaderboard <- ps() %>%
      dplyr::arrange(dplyr::desc(stepsperday)) %>%
      dplyr::select(ID, stepsperday)
    colnames(leaderboard) = c("Equipo", "Pasos/día")
    leaderboard
  })
}

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
#' @import plotly
#' @import DT
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
      
      # function to count steps
      count_steps <- function(input_data=runif(500,min=-1.5,max=1.5), coeffs=c(0,0,0)) {
        # by Matthew R Patterson, mpatterson@shimmersensing.com
        ## Find peaks of RMS acceleration signal according to Gu et al, 2017 method
        # This method is based off finding peaks in the summed and squared acceleration signal
        # and then using multiple thresholds to determine if each peak is a step or an artefact.
        # An additional magnitude threshold was added to the algorithm to prevent false positives 
        # in free living data. 
        #
        # returns sample location of each step
        fs = 15 # temporary for now, this is manually set
        acc <- sqrt(input_data[,1]^2 + input_data[,2]^2 + input_data[,3]^2)
        
        if (sd(acc) < 0.025) {
          # acceleration too low, no steps
          num_seconds = round(length(acc) / fs)
          steps_per_sec = rep(0,num_seconds)
        } else {
          # Search for steps
          # Thresholds
          k <- coeffs[[1]]
          period_min <- coeffs[[2]]
          period_max <- coeffs[[3]]
          sim_thres <- coeffs[[4]]   # similarity threshold
          cont_win_size <- coeffs[[5]]  # continuity window size
          cont_thres <- coeffs[[6]]     # continuity threshold
          var_thres <- coeffs[[7]]  # variance threshold
          mag_thres <- coeffs[[8]]
          
          # find the peak rms value is every range of k
          half_k <- round(k/2)
          segments <- floor(length(acc) / k)
          peak_info <- matrix(NA,nrow=segments,ncol=5)
          # peak_info[,1] - peak location
          # peak_info[,2] - acc magnitude
          # peak_info[,3] - periodicity (samples)
          # peak_info[,4] - similarity
          # peak_info[,5] - continuity
          
          # for each segment find the peak location
          for (i in 1:segments) {
            start_idx <- (i-1) * k + 1
            end_idx <- start_idx + (k-1)
            tmp_loc_a <- which.max(acc[start_idx:end_idx])
            tmp_loc_b <- (i-1) * k + tmp_loc_a
            # only save if this is a peak value in range of -k/2:+K/2
            start_idx_ctr <- tmp_loc_b - half_k
            if (start_idx_ctr < 1) {
              start_idx_ctr <- 1
            }
            end_idx_ctr <- tmp_loc_b + half_k
            if (end_idx_ctr > length(acc)) {
              end_idx_ctr <- length(acc)
            }
            check_loc <- which.max(acc[start_idx_ctr:end_idx_ctr])
            if (check_loc == (half_k + 1)) {
              peak_info[i,1] <- tmp_loc_b
              peak_info[i,2] <- max(acc[start_idx:end_idx])
            }
          }
          peak_info <- peak_info[is.na(peak_info[,1])!=TRUE,] # get rid of na rows
          
          # filter peak_info[,2] based on mag_thres
          peak_info <- peak_info[peak_info[,2] > mag_thres,]
          if (length(peak_info) > 10) {  # there must be at least two steps
            num_peaks <- length(peak_info[,1])
            
            no_steps = FALSE
            if (num_peaks > 2) {
              # Calculate Features (periodicity, similarity, continuity)
              peak_info[1:(num_peaks-1),3] <- diff(peak_info[,1]) # calculate periodicity
              peak_info <- peak_info[peak_info[,3] > period_min,] # filter peaks based on period_min
              peak_info <- peak_info[peak_info[,3] < period_max,]   # filter peaks based on period_max 
            } else {
              no_steps = TRUE
            }
          } else {
            no_steps = TRUE
          }
          
          if ( length(peak_info)==0 || length(peak_info) == sum(is.na(peak_info)) || no_steps == TRUE) {
            # no steps found
            num_seconds = round(length(acc) / fs)
            steps_per_sec = rep(0,num_seconds)
          } else {
            # calculate similarity
            num_peaks <- length(peak_info[,1])
            peak_info[1:(num_peaks-2),4] <- -abs(diff(peak_info[,2],2)) # calculate similarity
            peak_info <- peak_info[peak_info[,4] > sim_thres,]  # filter based on sim_thres
            peak_info <- peak_info[is.na(peak_info[,1])!=TRUE,] # previous statement can result in an NA in col-1
            
            # calculate continuity
            if (length(peak_info[,3]) > 5) {
              end_for <- length(peak_info[,3])-1
              for (i in cont_thres:end_for) {
                # for each bw peak period calculate acc var
                v_count <- 0 # count how many windows were over the variance threshold
                for (x in 1:cont_thres) {
                  if (var(acc[peak_info[i-x+1,1]:peak_info[i-x+2,1]]) > var_thres) {
                    v_count = v_count + 1
                  }
                }
                if (v_count >= cont_win_size) {
                  peak_info[i,5] <- 1 # set continuity to 1, otherwise, 0
                } else {
                  peak_info[i,5] <- 0
                }
              }
            } 
            peak_info <- peak_info[peak_info[,5]==1,1] # continuity test - only keep locations after this
            peak_info <- peak_info[is.na(peak_info)!=TRUE] # previous statement can result in an NA in col-1
            
            if (length(peak_info)==0) {
              # no steps found
              num_seconds = round(length(acc) / fs)
              steps_per_sec = rep(0,num_seconds)
            } else {
              
              # debug plot
              # is_plot = F
              # if (is_plot) {
              #   library(ggplot2)
              #   library(plotly)
              #   acc.df <- data.frame(acc=acc, det_step=integer(length(acc)))
              #   acc.df$det_step[peak_info] <- 1  # to plot annotations, prepare a 0/1 column on dataframe
              #   acc.df$idx <- as.numeric(row.names(acc.df))
              #   pl <- ggplot(data=acc.df,aes(x=idx,y=acc)) 
              #   pl2 <- pl + geom_line()
              #   pl3 <- pl2 + geom_point(data=subset(acc.df,det_step==1),aes(x=idx,y=acc),color='red',size=1,alpha=0.7)
              #   pl4 <- ggplotly(pl3)
              #   print(pl4)  
              # }
              
              # for GGIR, output the number of steps in 1 second chunks
              start_idx_vec <- seq(from=1,to=length(acc),by=fs)
              steps_per_sec <- table(factor(findInterval(peak_info, start_idx_vec), levels = seq_along(start_idx_vec)))
              steps_per_sec <- as.numeric(steps_per_sec)
            }
          }
        }
        
        return(steps_per_sec)
      }
      
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
      PS = aggregate(ds()[, c("stepsperday", "CAD_band_100_139_spm", "CAD_band_140_Inf_spm")], 
                     by = list(ds()$ID), FUN = sum)
      colnames(PS) = c("Equipo", "Pasos", "Moderada", "Alta")
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
      geom_bar(stat = "identity", width = 0.6, position = "dodge") +  
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
  
  output$Modcadence_plot <- renderPlotly({
    req(ds())
    
    # Create the ggplot2 plot
    p <- ggplot(ds(), aes(x = ID)) +
      geom_bar(aes(x = weekday, y = CAD_band_100_139_spm, fill = ID), 
               stat = "identity", width = 0.6, position = "dodge") +
      labs(title = "Tiempo caminando a intensidad moderada", x = "", y = "Minutos") +
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
  
  output$Vigcadence_plot <- renderPlotly({
    req(ds())
    
    # Create the ggplot2 plot
    p <- ggplot(ds(), aes(x = ID)) +
      geom_bar(aes(x = weekday, y = CAD_band_140_Inf_spm, fill = ID), 
               stat = "identity", width = 0.6, position = "dodge") +
      labs(title = "Tiempo caminando a intensidad vigorosa", x = "", y = "Minutos") +
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
  
  output$ranks <- DT::renderDT({
    req(ps())
    leaderboard <- ps() %>%
      dplyr::arrange(dplyr::desc(Pasos)) %>%
      dplyr::select(Equipo, Pasos, Moderada, Alta)
    DT::datatable(leaderboard) %>%
      DT::formatRound(columns = c("Pasos", "Moderada", "Alta"), 
                                  digits = 0)
  })
}

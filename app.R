options(shiny.maxRequestSize = 1024 * 1024^2)  # Set upload limit to 1TB

# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
    titlePanel("Step Contest"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Upload Accelerometer Data",
                      multiple = TRUE,
                      accept = c(".csv", ".gt3x", ".bin", ".cwa")),
            actionButton("process", "Process Data"),
            hr(),
            helpText("Make sure you upload the correct accelerometer files.")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Summary", 
                         tableOutput("summary_table")),
                tabPanel("Visualizations", 
                         plotOutput("bar_plot"),
                         plotOutput("cadence_plot")),
                tabPanel("Leaderboard", 
                         tableOutput("leaderboard"))
            )
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    data <- reactiveVal(NULL)
    
    # Create a temporary directory for GGIR output
    temp_dir <- tempdir()
    
    # Clean up temporary files on app close
    onStop(function() {
        unlink(temp_dir, recursive = TRUE)
    })
    
    # Process uploaded data
    observeEvent(input$process, {
        req(input$file)
        # Path to store GGIR output
        output_dir <- file.path(temp_dir, "GGIR_output")
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
        
        # List of uploaded files
        uploaded_files <- input$file$datapath
        
        # Run GGIR to process accelerometer data
        source("R/verisense_step_count.R")
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
            datadir = dirname(uploaded_files[1]),  # Directory with uploaded files
            outputdir = output_dir,                # Temporary output directory
            studyname = "stepcontest",
            mode = 1:2,                              # Only mode 1 is needed for step and cadence
            myfun = step_count,
            includedaycrit = 2,
            do.report = 2
        )
        
        # Load processed GGIR output
        summary_file <- file.path(output_dir, "output_stepcontest", "results", "part2_daysummary.csv")
        processed_data <- read.csv(summary_file)
        
        # Summarize data for each group
        summary_data <- processed_data %>%
            group_by(Group = input$file$name) %>%
            summarize(
                TotalSteps = sum(Steps, na.rm = TRUE),
                HighCadenceMinutes = sum(Cadence > 140, na.rm = TRUE),
                ModCadenceMinutes = sum(Cadence > 100 & Cadence <= 140, na.rm = TRUE)
            )
        
        data(summary_data)
    })
    
    # Display summary table
    output$summary_table <- renderTable({
        req(data())
        data()
    })
    
    # Bar plot of total steps
    output$bar_plot <- renderPlot({
        req(data())
        ggplot(data(), aes(x = Group, y = TotalSteps, fill = Group)) +
            geom_bar(stat = "identity") +
            labs(title = "Total Steps by Group", x = "Group", y = "Total Steps") +
            theme_minimal()
    })
    
    # Cadence comparison plot
    output$cadence_plot <- renderPlot({
        req(data())
        ggplot(data(), aes(x = Group)) +
            geom_bar(aes(y = ModCadenceMinutes, fill = "Moderate Cadence"), stat = "identity") +
            geom_bar(aes(y = HighCadenceMinutes, fill = "High Cadence"), stat = "identity") +
            labs(title = "Time Spent in Cadence Ranges", x = "Group", y = "Minutes") +
            scale_fill_manual(values = c("Moderate Cadence" = "blue", "High Cadence" = "red")) +
            theme_minimal()
    })
    
    # Leaderboard
    output$leaderboard <- renderTable({
        req(data())
        leaderboard <- data() %>%
            arrange(desc(TotalSteps)) %>%
            select(Group, TotalSteps)
        leaderboard
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
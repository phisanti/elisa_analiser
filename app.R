#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(ggplot2)
library(magrittr)
library(data.table)
library(rhandsontable)
library(shinydashboard)

import::from("R/app_utils.R", env_vars, empty_dt, agg_tables, extrapolate_data)
# Define UI for the application
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  titlePanel("ClarioStar Reshaping tool"),
  sidebarLayout(
    sidebarPanel(
      h1("How to use this tool"),
      br(),
      p(HTML(env_vars$text$introduction)),
      actionButton(inputId="run_data_processing",
                   label="Run Data Processing"),
      conditionalPanel(condition = "input.run_data_processing > 0",
                       downloadButton("download_data", "Download Processed Data")),
      br(),
      p("If you encounter any issues, please email at:", 
        a("cms206@cam.ac.uk", href="mailto:cms206@cam.ac.uk"), ".")
      
    ),
    mainPanel(
      fluidRow(
        column(6, 
               box(
                 title = "Input data",
                 width = NULL
                 )
               )
        
      ),
      # Use do.call to pass the list of tabPanels as separate arguments
      do.call(tabsetPanel,
              c(id = "plate_tabs",
                lapply(env_vars$plate_names, function(plate) {
                  tabPanel(plate,
                           rHandsontableOutput(plate)
                           )
                }))),
      
      uiOutput('explanation'),

      dataTableOutput(outputId="longtable")
    )
  )
)

server <- function(input, output, session) {
  
  # Load variables
  plates <- env_vars$plate_names
  values <- vector("list", length = length(plates))
  names(values) <- plates
  
  # Create 96-template tabs
  for (i in seq_along(plates)) {
    local({
      plate <- plates[i]
      plate_type <- env_vars$plate_types[plate]
      
      values[[plate]] <- reactiveValues(data = empty_dt(plate_type))
      output[[plate]] <- renderRHandsontable({
        rhandsontable(empty_dt(plate_type),
                                     stretchH = "all", 
                                     useTypes = TRUE, 
                                     colHeaders = paste("col", 1:12, sep = "_"),
                                     rowHeaders = LETTERS[1:8])
      })
      
      # Make tabs reactive so that the user can input data
      observeEvent(input[[plate]], {
        values[[plate]]$data <- hot_to_r(input[[plate]])
      })
    })
  }
  
  # Retreive the data 
  observeEvent(input$run_data_processing, {
    
    out_data <- vector("list", length = length(plates))
    names(values) <- plates
    
    for (i in seq_along(plates)) {
      plate <- plates[i]
      isolate({
        out_data[[plate]] <- hot_to_r(input[[plate]])
      })
    }
    
    # Do not allow to continue if the user has not filled all the tabs
    out_data <- Filter(Negate(is.null), out_data)
    req(length(out_data) == length(plates))
    
    # Process the data
    d <- agg_tables(out_data, env_vars)
    d <- extrapolate_data(d)
    
    # Display output and allow download
    output$explanation <- renderUI({
      h1("Output data")
      p("Here is the transformed table")
    })
    output$longtable <- renderDataTable({
      datatable(d)
    })
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("processed_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(d, file, row.names = FALSE)
      }
    )

  })
}

# Run the application 
shinyApp(ui = ui, server = server)

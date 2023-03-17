#
# This is the main script for the shiny application. It contains the server
# and the IU functions. 
# 

library(shiny)
library(DT)
library(ggplot2)
library(magrittr)
library(data.table)
library(rhandsontable)
library(shinydashboard)

source("R/app_utils.R")
#import::from("app_utils.R", env_vars, empty_dt, agg_tables, extrapolate_data, lm_to_latex)

# Define UI for the application
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  titlePanel("ClarioStar Reshaping tool"),
  sidebarLayout(
    sidebarPanel(
      h1("How to use this tool"),
      br(),
      p(HTML(env_vars$text$introduction)),
      fluidRow(
        column(6,
               selectInput("reg_type", label = "Choose a fitting model:",
                           choices = c("Linear Regression", "4-Parameters Logistic"))
        )),
      fluidRow(
        column(6, 
               actionButton(inputId="run_data_processing",
                            label="Run Data Processing"),
               conditionalPanel(
                 condition = "input.run_data_processing > 0 & (output.check != 'ok')",
                 tags$p(
                   style = "color: red",
                   "Please fill all the tabs before processing the data"
                 )
               )
        ),
        column(6,
               conditionalPanel(condition = "input.run_data_processing > 0 & output.check == 'ok'",
                                downloadButton("download_data", "Download Processed Data")
               )
        )
      ),
      fluidRow(
        column(3,
               p("Analysis status:"),
        ),
        column(9,
               textOutput('check')
        )
      ),
      br(),
      fluidRow(
        column(12,
               p("If you encounter any issues, please email at:", 
                 a("cms206@cam.ac.uk", href="mailto:cms206@cam.ac.uk"), ".")
        )
      )
    ),
    mainPanel(h1("Input data"),
      # Use do.call to pass the list of tabPanels as separate arguments
      do.call(tabsetPanel,
              c(id = "plate_tabs",
                lapply(env_vars$plate_names, function(plate) {
                  tabPanel(plate,
                           rHandsontableOutput(plate)
                           )
                }))),
      fluidRow(
        column(6, 
                uiOutput('explanation')
               ),
        column(6, 
               plotOutput('regplot')
        )
        
        ),
      dataTableOutput(outputId="longtable")
    )
  )
)

server <- function(input, output, session) {
  
  # Load variables
  plates <- env_vars$plate_names
  values <- vector("list", length = length(plates))
  names(values) <- plates
  output$check <- renderText("Waiting for input")
  
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
    output$check <- renderText(
      if (length(out_data) == length(plates)) {
        "ok"
      } else {
          "not ok"
        }
      )
    
    # Process the data
    d <- agg_tables(out_data, env_vars)
    extra_d <- reactive({

      extrapolate_data(d, reg_type = input$reg_type)})
    d <- extra_d()$data
    model <- extra_d()$model
    reg_plot <- extra_d()$plot
    latex <- extra_d()$latex
    
    # Display output and allow download
    output$explanation <- renderUI({
      tagList(h1("Output data"),
              p("The data has been interpolated using the following equation:"),
              p(""),
              p(withMathJax(latex[1])),
              p(withMathJax(latex[2])),
              p(),
      p("Here is the transformed table")
      )
    })
    
    output$regplot <- renderPlot({reg_plot})

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

### Check Submission Module

library(shiny)
library(DT)
library(dplyr)
library(purrr)


# UI ----------------------------------------------------------------------


check_sub_UI <- function(id) {
  ns <- NS(id)
  tagList(



            # Descriptions ------------------------------------------------------------
            
            fluidRow(
              column(9,
                     h2("Check Student's Submission")
                     ),
              column(3,
                     ### Download
                     download_xlsx_UI(ns("download"), "Download Excel")
                     )
            ),
            
            #tags$blockquote(helpText("Check student's submission from SELECx by \"State\" column (i.e., Finished vs In progress).")),
            h5(helpText("\"Check student's submission from SELECx by \"State\" column (i.e., Finished vs In progress).\"")),
            br(),
            helpText("This module receives input from any ", tags$a(href="https://docs.moodle.org/311/en/Quiz_reports", "Moodle Quiz report")),
            
            h3("Guides"),
            helpText("Get more details: ",
                     tags$a(href="https://docs.google.com/document/d/1F4Sbdz6TAYDq9YloOJOFm4eht5524166LGGfW_5GRGU/edit?usp=sharing", "here")   
            ),
            helpText("1) ","Download Moodle Quiz report, i.e. Grades or Responses file(s), from SELECx."),
            helpText("2) ", "Rename that file(s) to English, short name is preferred."),

           
            # Upload Report --------------------------------------------------------------------
            br(),
            fluidRow(
              column(6,
                     helpText("3) ", "Upload file (multiple accepted)"),
                     br(),
                     read_UI(ns("file"), buttonLabel = "Upload Reports", width = validateCssUnit("fit-content"), multiple = T),
                     htmlOutput(ns("validate_msg")),
                     ),
              column(6,
                     # Extract ID from ---------------------------------------------------------
                     br(),
                     extract_id_col_UI(ns("extract_id_col")),
                     )
            ),
            

            # Upload File ID ----------------------------------------------------------
            
            helpText("4) ", "Upload ID file that has column \"Name\" for student's names and \"ID\" for student's id numbers."),
            br(),
            fileInput(ns("file_id"), NULL, accept = c(".csv", ".xls",".xlsx"),buttonLabel = "Upload ID",
                      placeholder = "choose file .csv or .xlsx"),
  
            select_id_cols_UI(ns("choose_cols")),



     

 
    hr(),
   
    h3("Check Submission"),
    helpText("State encoding: \"Finished\" = 1, \"In progress\" = 0"),
    br(),
    DT::DTOutput(ns("table")),
   
    hr(),
    h3("Missing Names"),
    DT::DTOutput(ns("table_miss")),
    
    verbatimTextOutput(ns("raw")),
    verbatimTextOutput(ns("raw2"))
    
  )
}


# Server ------------------------------------------------------------------



check_sub_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Read Report  --------------------------------------------------------------------
      
      data_input <- read_Server("file", warning = T, multiple = T, 
                                warning_react = not_all_report)
      
      data_raw <- reactive({ data_input()$data })
      file_name <- reactive({ data_input()$file_name })
      
      # Validate Report ----------------------------------------------------------------
      
      ### Check if Every element is Moodle Quiz report
      is_all_report <-  reactive({
        
        data_raw() %>% purrr::every(is_report)
        
      })
      
      not_all_report <- reactive({ !is_all_report() })
      
      ### Error Msg
      output$validate_msg <- renderText({
        
        if(not_all_report()){
          
          HTML("<p style='color:#b30000;'>",
               "All file(s) must be ", 
               "<a href='https://docs.moodle.org/311/en/Quiz_reports'>Moodle Quiz report</a>",
               "</p>")
        }
        
      })

      # Read ID file ------------------------------------------------------------
      
      id_df <- reactive({ 
        
        req(input$file_id) # Require - code wait until file uploaded
        read_single(file_name = input$file_id$name,
                    file_path = input$file_id$datapath) 
        
      })
      

      # Validate ID file ------------------------------------------------------------
      
      is_valid_id <- reactive({
        
        id_regex <- c("Name", "ID")
        all( is_regex_in_names(id_df(), regex = id_regex) )
        
      })
      
      observeEvent(input$file_id,
                   shinyFeedback::feedbackWarning(
                     "file_id",
                     !is_valid_id(),
                     "ID file must have column 'Name' and 'ID'"
                   )
      )
      
      # Select ID column --------------------------------------------------------------------
      
      id_df_selected <- select_id_cols_Server("choose_cols", id_df)
      
      

      # Process -----------------------------------------------------------------
      
      ## Extract ID from which column
      
      id_col <- extract_id_col_Server("extract_id_col")
      
      data_processed <- reactive({
        
        req(is_all_report())
        moodleQuiz::check_sub(data_raw(), 
                              extract_id_from = id_col(),
                              id_regex = "[:digit:]+",
                              choose_encode = "max",
                              choose_time = "first"
                              )
        
      })
      


      
      # Join ID --------------------------------------------------------------------
      
      data_joined <- reactive({
        
        # Is ID file is ready (uploaded & valid)
        is_id_ready <- c( isTruthy(input$file_id) && is_valid_id() )

        # ID is not ready yet
        if( !is_id_ready ){
          
          data_processed()
          
        # ID is ready
        }else if( is_id_ready ){
   
          join_id2(ids = id_df_selected(), df = data_processed())

        }
        
        
      })
      

      # Missing Names -----------------------------------------------------------

      
      data_missing <- reactive({
        
        data_joined() %>% 
           filter(if_any(starts_with("Name"), is.na))
      })
      

      # Show Table --------------------------------------------------------------

      output$table <- DT::renderDT({
        
        data_joined()
        
      }, options = list(lengthMenu = c(5,10,20,50), pageLength = 5 ), 
      selection = 'none',
      filter = "top")
      
      
      output$table_miss <- DT::renderDT({
        
        data_missing()
        
      }, options = list(lengthMenu = c(5,10,20,50), pageLength = 5 ), 
      selection = 'none')
    
      
      # Download ----------------------------------------------------------------
      
      download_xlsx_Server("download", 
                           list("Check Submission" = data_joined(), "Missing Names" = data_missing()), 
                           filename = "Check_Submission.xlsx")

      
      # output$raw <- renderPrint({
      # 
      #   id_col()
      # 
      # })
      # 
      # 
      # output$raw2 <- renderPrint({
      #   
      #   c( isTruthy(input$file_id) && is_valid_id() )
      #   
      #})
  
  
    }
  )
}

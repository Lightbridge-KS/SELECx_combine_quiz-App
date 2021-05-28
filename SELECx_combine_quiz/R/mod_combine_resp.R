### Combine Responses Module

library(shiny)
library(dplyr)
library(purrr)


# UI ----------------------------------------------------------------------


combine_resp_UI <- function(id) {
  ns <- NS(id)
  tagList(
    # Descriptions ------------------------------------------------------------
    fluidRow(
      column(9,
             h2("Combine Student's Responses")
             ),
      column(3,
             ### Download 
             download_xlsx_UI(ns("download"), "Download Excel")
             )
    ),

    tags$blockquote(helpText("Combine & Count student's responses from SELECx.")),
    h5(helpText("This module receives input from ", tags$a(href="https://docs.moodle.org/311/en/Quiz_reports", "Moodle Responses report"), 
                " (not Grades report).")),
    
    h3("Guides"),
    helpText("Get more details: ",
             tags$a(href="https://docs.google.com/document/d/1F4Sbdz6TAYDq9YloOJOFm4eht5524166LGGfW_5GRGU/edit?usp=sharing", "here")   
    ),
    helpText("1) ","Download Moodle Responses report from SELECx."),
    helpText("2) ", "Rename that file(s) to English, short name is preferred."),
    helpText("3) ", "Upload file (multiple accepted)"),
    
    # Upload --------------------------------------------------------------------
    
    read_UI(ns("file"), buttonLabel = "Upload Reports", multiple = T),
    htmlOutput(ns("validate_msg")),
    
    helpText("4) ", "Upload ID file that has column \"Name\" for student's names and \"ID\" for student's id numbers."),
    fileInput(ns("file_id"), NULL, accept = c(".csv", ".xls",".xlsx"),buttonLabel = "Upload ID",
              placeholder = "choose file .csv or .xlsx"),
    select_id_cols_UI(ns("choose_cols")),
    
    uiOutput(ns("split_cloze_checkbox")),
    
    hr(),
    h3("Combine Responses"),
    dataTableOutput(ns("table")),
    
    hr(),
    h3("Count Responses"),
    helpText("This table counts how many responses that student answered from each quiz."),
    helpText("Blanked or dashed ( `-` ) responses will not be counted."),
    dataTableOutput(ns("table_counted")),
    
    hr(),
    h3("Missing Names"),
    dataTableOutput(ns("table_miss")),
    
    verbatimTextOutput(ns("raw"))
  )
}


# Server ------------------------------------------------------------------


combine_resp_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Read Report  --------------------------------------------------------------------
      
      data_input <- read_Server("file", warning = T, multiple = T, 
                                warning_react = not_all_resp_report)
      
      data_raw <- reactive({ data_input()$data })
      file_name <- reactive({ data_input()$file_name })
      
      
      # Validate Responses Report ----------------------------------------------------------------
    
      
      ### Check if Every element is Moodle Responses report
      
      is_all_resp_report <- reactive({
        
        data_raw() %>% purrr::every(is_responses_report)
        
      })
      
      not_all_resp_report <- reactive({ !is_all_resp_report() })
      
      ### Check if Any element is Moodle Grades report
      
      is_any_grades_report <- reactive({
        
        data_raw() %>% purrr::every(is_grades_report)
        
      })
      
      ### Error Msg
      output$validate_msg <- renderText({
        
        # If "Grades" report 
        if(is_any_grades_report()){
          
          HTML("<p style='color:#b30000;'>",
               "Moodle Grades report(s) is/are founded. All file(s) must be ", 
               "<a href='https://docs.moodle.org/311/en/Quiz_reports'>Moodle Responses report</a>",
               "</p>")
        # If other report
        }else if(not_all_resp_report()){
          
          HTML("<p style='color:#b30000;'>",
               "All file(s) must be ", 
               "<a href='https://docs.moodle.org/311/en/Quiz_reports'>Moodle Responses report</a>",
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
      
      ### Check box to Split Cloze (if any)
      output$split_cloze_checkbox <- renderUI({

        if( purrr::some(data_raw(), has_cloze_col)){
          checkboxInput(session$ns("split_cloze"),"Split Embedded Answer (Cloze) Columns?",  value = F)
        }

      })
      
      data_processed <- reactive({
        
        req(is_all_resp_report())
        
        moodleQuiz::combine_resp(data_raw(),
                                 id_regex = "[:digit:]+",
                                 choose_encode = "max",
                                 choose_time = "first",
                                 # If NULL or FALSE -> not split, TRUE -> Split
                                 split_cloze = isTruthy(input$split_cloze),
                                 part_glue = "."
                                 )

        
      })
      

      # Count Responses ---------------------------------------------------------

      
      data_counted <- reactive({
        
        req(is_all_resp_report())
        
          moodleQuiz::count_resp(data_raw(),
                                 id_regex = "[:digit:]+",
                                 choose_encode = "max",
                                 choose_time = "first",
                                 # If NULL or FALSE -> not split, TRUE -> Split
                                 count_cloze_parts = isTruthy(input$split_cloze)
        )
        
        
      })
      
      

      
      # Processed Join ID --------------------------------------------------------------------
      
      data_processed_joined <- reactive({
        
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
      

      # Counted Join ID ---------------------------------------------------------
      
      data_counted_joined <- reactive({
        
        # Is ID file is ready (uploaded & valid)
        is_id_ready <- c( isTruthy(input$file_id) && is_valid_id() )
        
        # ID is not ready yet
        if( !is_id_ready ){
          
          data_counted()
          
          # ID is ready
        }else if( is_id_ready ){
          
          join_id2(ids = id_df_selected(), df = data_counted())
          
        }
        
        
      })
      
      
      
      # Missing Names -----------------------------------------------------------
      
      
      data_processed_missing <- reactive({
        
        data_processed_joined() %>% 
          filter(if_any(starts_with("Name"), is.na))
      })
      
      # Show Table --------------------------------------------------------------
      
      output$table <- renderDataTable({
        
        data_processed_joined()
        
      }, options = list(lengthMenu = c(5,10,20,50), pageLength = 5 ))
      
      
      output$table_counted <- renderDataTable({
        
        data_counted_joined()
        
      }, options = list(lengthMenu = c(5,10,20,50), pageLength = 5 ))
      
      output$table_miss <- renderDataTable({
        
        data_processed_missing()
        
      }, options = list(lengthMenu = c(5,10,20,50), pageLength = 5 ))
      
      # Download ----------------------------------------------------------------
      
      download_xlsx_Server("download", 
                           list(Responses = data_processed_joined(), 
                                Count = data_counted_joined(),
                                Missing = data_processed_missing()), 
                           filename = "Combined_Responses.xlsx")
      
      
      # output$raw <- renderPrint({
      #   
      #   input$split_cloze
      #   
      # })
  
  
    }
  )
}

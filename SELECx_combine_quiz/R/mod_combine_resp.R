### Combine Responses Module

library(shiny)
library(DT)
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

    #tags$blockquote(helpText("Combine & Count student's responses from SELECx.")),
    h5(helpText("\"Combine & Count student's responses from SELECx.\"")),
    br(),
    helpText("This module receives input from ", tags$a(href="https://docs.moodle.org/311/en/Quiz_reports", "Moodle Responses report"), 
                " (not Grades report)."),
    
    h3("Guides"),
    helpText("Get more details: ",
             tags$a(href="https://docs.google.com/document/d/1F4Sbdz6TAYDq9YloOJOFm4eht5524166LGGfW_5GRGU/edit?usp=sharing", "here")   
    ),
    helpText("1) ","Download Moodle Responses report from SELECx."),
    helpText("2) ", "Rename that file(s) to English, short name is preferred."),

    
    # Upload Report --------------------------------------------------------------------
    br(),
    fluidRow(
      column(6,
             helpText("3) ", "Upload file (multiple accepted)"),
             br(),
             read_UI(ns("file"), buttonLabel = "Upload Reports", width = validateCssUnit("fit-content"), multiple = T),
             htmlOutput(ns("validate_msg")),
             helpText("4) ", "Upload ID file that has column \"Name\" for student's names and \"ID\" for student's id numbers."),
             br()
             ),
      column(6,
             # Extract ID from ---------------------------------------------------------
             br(),
             extract_id_col_UI(ns("extract_id_col")),
             encode_type_select_UI(ns("encode_type"))
             )
    ),

    

    fileInput(ns("file_id"), NULL, accept = c(".csv", ".xls",".xlsx"),buttonLabel = "Upload ID",
              placeholder = "choose file .csv or .xlsx"),
    
    select_id_cols_UI(ns("choose_cols")),
    uiOutput(ns("split_cloze_checkbox")),

    
    hr(),
    h3("Combine Responses"),
    DT::DTOutput(ns("table")),
    
    hr(),
    h3("Count Responses"),
    helpText("This table counts how many responses that student answered from each quiz."),
    helpText("Blanked or dashed ( `-` ) responses will not be counted."),
    br(),
    DT::DTOutput(ns("table_counted")),
    
    hr(),
    h3("Missing Names"),
    DT::DTOutput(ns("table_miss")),
    
    hr(),
    
    h3("Caution"),
    helpText("This table detects responses that might be inappropriate or incomplete."),
    helpText("The conspicuous responses will be reported in 'Caution' column."),
    br(),
    DT::DTOutput(ns("table_inapp")),
    
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
      

      # Select Encoding ---------------------------------------------------------
      
      choose_enc  <- encode_type_select_Server("encode_type")
      
      
      # Process -----------------------------------------------------------------
      
      ### Extract ID from which column
      
      id_col <- extract_id_col_Server("extract_id_col")
      
      ### Check box to Split Cloze (if any)
      output$split_cloze_checkbox <- renderUI({

        if( purrr::some(data_raw(), has_cloze_col)){
          checkboxInput(session$ns("split_cloze"),"Split Embedded Answer (Cloze) Columns?",  value = F)
        }

      })
      
      data_processed <- reactive({
        
        req(is_all_resp_report())
        
        moodleQuiz::combine_resp(data_raw(),
                                 extract_id_from = id_col(),
                                 id_regex = "[:digit:]+",
                                 choose_encode = choose_enc()$encode,
                                 choose_time =  choose_enc()$time,
                                 # If NULL or FALSE -> not split, TRUE -> Split
                                 split_cloze = isTruthy(input$split_cloze),
                                 part_glue = "."
                                 ) %>% 
          # Report Inappropriate Words (by add column "Caution")
          report_spam_words(contains("Response"), patterns = unlist(inapp_word_ls),
                            name = "Caution")

        
      })
      

      # Count Responses ---------------------------------------------------------

      
      data_counted <- reactive({
        
        req(is_all_resp_report())
        
          moodleQuiz::count_resp(data_raw(),
                                 extract_id_from = id_col(),
                                 id_regex = "[:digit:]+",
                                 choose_encode = choose_enc()$encode,
                                 choose_time = choose_enc()$time,
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
      
      # Inappropriate Word (Caution) -----------------------------------------------------------
      
      data_processed_inapp <- reactive({
        
        data_processed_joined() %>% 
          relocate(Caution, .before = contains("Response")) %>% 
          #select(-contains("Response")) %>%
          filter(!is.na(Caution))
        
      })
      
      
      # Show Table --------------------------------------------------------------
      
      output$table <- DT::renderDT({
        
        data_processed_joined()
        
      }, options = list(lengthMenu = c(5,10,20,50), pageLength = 5 ), 
      selection = 'none',
      filter = "top")
      
      
      output$table_counted <- DT::renderDT({
        
        data_counted_joined()
        
      }, options = list(lengthMenu = c(5,10,20,50), pageLength = 5 ), 
      selection = 'none',
      filter = "top")
      
      output$table_miss <- DT::renderDT({
        
        data_processed_missing()
        
      }, options = list(lengthMenu = c(5,10,20,50), pageLength = 5 ), selection = 'none')
      
      
      output$table_inapp <- DT::renderDT({
        
        data_processed_inapp()
        
      }, options = list(lengthMenu = c(5,10,20,50), pageLength = 5 ), selection = 'none')
      
      # Download ----------------------------------------------------------------
      
      download_xlsx_Server("download", 
                           list("Combine Responses" = data_processed_joined(), 
                                "Count Responses" = data_counted_joined(),
                                "Missing Names" = data_processed_missing(),
                                "Caution" = data_processed_inapp()), 
                           filename = "Combined_Responses.xlsx")
      
      
      # output$raw <- renderPrint({
      # 
      #   inapp_word
      # 
      # })
  
  
    }
  )
}

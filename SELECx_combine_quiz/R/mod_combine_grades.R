### Combine Grades Module

library(shiny)
library(DT)
library(dplyr)
library(purrr)

library(shiny)

# UI ----------------------------------------------------------------------


combine_grades_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    # Descriptions ------------------------------------------------------------
    fluidRow(
      column(9,
             h2("Combine Student's Grades")
      ),
      column(3,
             ### Download 
             download_xlsx_UI(ns("download"), "Download Excel")
      )
    ),
    
    #tags$blockquote(helpText("Combine, filter, and adjust student's score from SELECx.")),
    h5(helpText("\"Combine, filter, and adjust student's score from SELECx.\"")),
    br(),
    helpText("This module receives input from any ", tags$a(href="https://docs.moodle.org/311/en/Quiz_reports", "Moodle Quiz report"), 
                " that have all numeric grades for every students (no essay questions)."),
    
    h3("Guides"),
    helpText("Get more details: ",
             tags$a(href="https://docs.google.com/document/d/1F4Sbdz6TAYDq9YloOJOFm4eht5524166LGGfW_5GRGU/edit?usp=sharing", "here")   
    ),
    
    fluidRow(
      column(7,
             helpText("1) ","Download Moodle Quiz report from SELECx."),
             helpText("2) ", "Rename that file(s) to English, short name is preferred."),
             helpText("3) ", "Upload file (multiple accepted)"),
             # Upload --------------------------------------------------------------------
             br(),
             read_UI(ns("file"), buttonLabel = "Upload Reports", width = validateCssUnit("fit-content"), multiple = T),
             htmlOutput(ns("validate_msg")),
             
             helpText("4) ", "Upload ID file that has column \"Name\" for student's names and \"ID\" for student's id numbers."),
             br(),
             fileInput(ns("file_id"), NULL, accept = c(".csv", ".xls",".xlsx"),buttonLabel = "Upload ID",
                       placeholder = "choose file .csv or .xlsx"),
             
             ),
      column(5,
             # Extract ID from ---------------------------------------------------------
             extract_id_col_UI(ns("extract_id_col")),
             grades_type_select_UI(ns("grades_type")),
             adj_max_grades_UI(ns("adj_grades")),
             select_id_cols_UI(ns("choose_cols")),
             # Arrange Rows by which Col
             arrange_UI(ns("arrange"))
             )
    ),
    

    

    hr(),
    h3("Combined Grades"),
    DT::DTOutput(ns("table")),
    
    hr(),
    h3("Missing Names"),
    DT::DTOutput(ns("table_miss")),
    
    
    #verbatimTextOutput(ns("raw"))
    
  )
}


# Server ------------------------------------------------------------------


combine_grades_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Read Report  --------------------------------------------------------------------
      
      data_input <- read_Server("file", warning = T, multiple = T, 
                                warning_react = not_valid_data)
      
      data_pre <- reactive({ data_input()$data })
      file_name <- reactive({ data_input()$file_name })
      
      # Validate Responses Report ----------------------------------------------------------------
      
      ### Check if Every element is Moodle Quiz report
      
      is_all_report <- reactive({
        
        data_pre() %>% purrr::every(is_report)
        
      })
      
      ### Check if "Grade" column is presented
      
      has_grade_col <- reactive({
        
        data_pre() %>% purrr::every(~is_regex_in_names(.x, "Grade"))
        
      })
      
      ### Check if Some element has "Not yet graded" in "Grade" column
      
      is_some_nyg_presented <- reactive({
        
        # If it's has "Grade" column; check if TRUE
        if( isTruthy(has_grade_col()) ){
          
          data_pre() %>% purrr::some(is_some_grade_nyg)
          
        }else{ FALSE }  
        
      })
      
      ### Valid Data if All report is Moodle Quiz, has "Grade" column, and No "Not yet graded" 
      is_valid_data <- reactive({
        
       all( is_all_report(), has_grade_col(), !is_some_nyg_presented() )
        
      })
      not_valid_data <- reactive({  !is_valid_data()  })
      
      
      ### Error Msg
      output$validate_msg <- renderText({
        
          ## Not a Moodle Quiz
        if(!is_all_report()){
         
           HTML("<p style='color:#b30000;'>",
               "All file(s) must be ", 
               "<a href='https://docs.moodle.org/311/en/Quiz_reports'>Moodle Quiz report</a>",
               "</p>")
        
          ## No "Grade" column
        }else if( !has_grade_col()){
          
          HTML("<p style='color:#b30000;'>",
               "All file(s) must have \"Grade\" column",
               "</p>")
          
          ## Some "Not yet graded"
        }else if( is_some_nyg_presented()){
          
          HTML("<p style='color:#b30000;'>",
               "\"Grade\" column must not contain \"Not yet graded\".", 
               "</p>",
               
               "<p style='color:#0000b3;'>",
               "(All students must have a numeric grades. Essay questions are not allowed.)",
               "</p>"
          )
          
        }
       
        
      })
      

     # Data Validated ----------------------------------------------------------

      
      data_raw <- reactive({
        
        req( isTruthy(is_valid_data()) )
        data_pre()
        
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
      
      ### Extract ID from which column
      
      id_col <- extract_id_col_Server("extract_id_col")
      
      ### Filter Grades Server
      choose_gt <- grades_type_select_Server("grades_type")
      
      ### Adjust Maxiumum Grades
      new_max_grade <- adj_max_grades_Server("adj_grades", data_raw)
      
      
      ### Compute
      data_processed <- reactive({
        
        req(is_valid_data())
        
        dat <- if (length(data_raw()) == 1) {
         # If upload only 1 Moodle Quiz, nake list of 1 DF to DF.
          data_raw()[[1]]
        }else{
         # If upload Multiple Moodle Quiz, passed as list of DF.
          data_raw()
        }
        
        moodleQuiz::combine_grades(dat,
                                   extract_id_from = id_col(),
                                   id_regex = "[:digit:]+",
                                   new_max_grade = new_max_grade(),
                                   round_digits = 3,
                                   choose_grade = choose_gt()$grade,
                                   choose_time = choose_gt()$time,
                                   force_grade = FALSE
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
      

      # Arrange Rows ------------------------------------------------------------

      data_joined_arranged <- arrange_Server("arrange", data_react = data_joined)
      
      # Missing Names -----------------------------------------------------------
      
      
      data_missing <- reactive({
        
        data_joined_arranged() %>% 
          filter(if_any(starts_with("Name"), is.na))
      })
      

      # Show Table --------------------------------------------------------------

      output$table <- DT::renderDT({
        
        data_joined_arranged()
        
      }, options = list(lengthMenu = c(5,10,20,50), pageLength = 5 ), 
      selection = 'none',
      filter = "top")
      
      output$table_miss <- DT::renderDT({
        
        data_missing()
        
      }, options = list(lengthMenu = c(5,10,20,50), pageLength = 5 ), selection = 'none')
      
      
      # output$raw <- renderPrint({
      #   
      #   id_df()
      #   
      # })
      
      # Download ----------------------------------------------------------------
      
      download_xlsx_Server("download", 
                           list("Combine Grades" = data_joined_arranged(),                               
                                "Missing Names" = data_missing()), 
                           filename = "Combined_Grades.xlsx")
  
  
    }
  )
}

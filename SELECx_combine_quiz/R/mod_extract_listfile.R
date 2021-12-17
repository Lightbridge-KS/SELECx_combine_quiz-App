### Module: Extract list file (Must list file via cmd line already)

library(shiny)
library(dplyr)
library(stringr)


# Function ----------------------------------------------------------------


moodle_extract_listfile <- function(df) {
  
  is_colnm_correct <- c(is.data.frame(df) & all(names(df) == "File"))
  if(!is_colnm_correct) stop("`data frame` must contain one column named 'File'", 
                             call. = F)
  
  df_mod <- df %>%
    dplyr::mutate(
      Name = stringr::str_extract(File, "[^_]+"),
      File_Name = stringr::str_extract(File, "(?<=_assignsubmission_file_).+"),
      ID = stringr::str_extract(File_Name, "[:digit:]{7}"),
      Extension = stringr::str_extract(File, "[^\\.]+$")
    ) %>%
    dplyr::select(Name, ID, File, File_Name, Extension)
  
  df_mod %>% 
    dplyr::mutate(Encode = as.numeric(!is.na(ID)))
  
}

# UI ----------------------------------------------------------------------


extract_listfile_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column(9,
             h2("Check File Submission"),
      ),
      column(3,
    ### Download
             download_xlsx_UI(ns("download"), "Download Excel")
      )
    ),
    
    h5(helpText("Check student's file submission in various format (pdf, jpeg, etc.).")),
    
    h3("Guides"),
    helpText("Get more details: ",
             tags$a(href="https://docs.google.com/document/d/1FxIjdSgyZ1d1D7JGS9gOA2gEzn5UIfYV6150tZWZju4/edit?usp=sharing", "here")   
    ),
    helpText("1) ","Before class, inform the students to include student's ID (7 digits) in the submission file names."),
    helpText("2) ","Download student's submission files from SELECx (not in subfolder). Do not rename any files."),
    
    helpText("3) ","Use ", tags$a(href="https://en.wikipedia.org/wiki/Command-line_interface","Command-line"),
             " to list files and redirect Standard output to .csv file"),
    helpText("4) ","Open that .csv file in Excel/Google Sheet, You should see list of student's submission files in a single column."),
    helpText("5) ", "Named the column name (in cell A1) as:", tags$code("File")),
    helpText("6) ", "Save it to .csv or excel"),
    br(),
    
    helpText("7)", "Then, Upload it here:"),
    br(),
    ### Upload List File
    read_UI(ns("file"), multiple = F, 
            buttonLabel = "Upload List File", 
            placeholder = "choose file .csv or .xlsx", 
            width = validateCssUnit("fit-content")
            ),
    htmlOutput(ns("validate_msg")),
    
    br(),
    ### Upload File ID
    helpText("8) ", "Upload ID file that has column \"Name\" for student's names and \"ID\" for student's id numbers."),
    br(),
    fileInput(ns("file_id"), NULL, accept = c(".csv", ".xls",".xlsx"),buttonLabel = "Upload ID",
              placeholder = "choose file .csv or .xlsx"),
    select_id_cols_UI(ns("choose_cols")),
    ### Arrange rows by which col
    arrange_UI(ns("arrange")),
    
    
    ### Table
    h3("Check File Submission"),
    tags$ul(
      tags$li(tags$b("File:"), " Original \"File\" column from uploaded list files."),
      tags$li(tags$b("File_Name:"), " Names of the student's submission files."),
      tags$li(tags$b("Extension:"), " File extension of the student's submission files."),
      tags$li(tags$b("Encode")),
        tags$ul(
          tags$li(tags$b("Encode = 1:"), " Student submitted file to SELECx and 7 consecutive numbers (student ID) was found in ", tags$b("File_Name")),
          tags$li(tags$b("Encode = 0:"), " Student submitted file to SELECx but 7 consecutive numbers (student ID) was ", tags$b("not"), " found in ", tags$b("File_Name")),
          tags$li(tags$b("Blank:"), "Student ", tags$b("not"), " submitted file to SELECx", tags$i(" or "), tags$b("not"), " included 7 consecutive numbers (student ID) in ", tags$b("File_Name"))
        )
    ),
    
    br(),
    DT::DTOutput(ns("table")),
    

    
    ### Table Missing Name
    hr(),
    h3("Missing Names"),
    DT::DTOutput(ns("table_miss")),
    
    verbatimTextOutput(ns("raw"))
    
    
  )
}


# Server ------------------------------------------------------------------


extract_listfile_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      

      # Read List File --------------------------------------------------------------------

      data_input <- read_Server("file", multiple = F,
                                warning = T, 
                                warning_react = not_correct_df)
      
      df_raw <- reactive({ data_input()$data })
      file_name <- reactive({ data_input()$file_name })
      

      # Validate List File  --------------------------------------------------------------------
      
      is_correct_df <- reactive({
        
        c(is.data.frame(df_raw()) & all(names(df_raw()) == "File"))
        
      })
      
      not_correct_df <- reactive({ !is_correct_df() })
      
      output$validate_msg <- renderText({
        
        if(not_correct_df()){
          
          HTML("<p style='color:#b30000;'>",
               "CSV or Excel file must have exactly 1 column that named \"File\"", 
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
      
      
      # Process List File DF --------------------------------------------------------------------
      
      df_processed <- reactive({
        req(is_correct_df())
        moodle_extract_listfile(df_raw())
      })
      
      # Join ID --------------------------------------------------------------------
      
      df_joined <- reactive({
        
        # Is ID file is ready (uploaded & valid)
        is_id_ready <- c( isTruthy(input$file_id) && is_valid_id() )
        
        # ID is not ready yet
        if( !is_id_ready ){
          
          df_processed()
          
          # ID is ready
        }else if( is_id_ready ){
          
          join_id2(ids = id_df_selected(), df = df_processed())
          
        }
        
        
      })
      

      # Arrange -----------------------------------------------------------------
      df_joined_arranged <- arrange_Server("arrange", data_react = df_joined)

      
      
      # Missing Names -----------------------------------------------------------
      
      
      df_missing <- reactive({
        
        df_joined_arranged() %>% 
          filter(if_any(starts_with("Name"), is.na))
      })
      
      # Show Table --------------------------------------------------------------
      
      output$table <- DT::renderDT({
        
        df_joined_arranged()
        
      }, options = list(lengthMenu = c(5,10,20,50), pageLength = 5 ), 
      selection = 'none',
      filter = "top")
      
      
      output$table_miss <- DT::renderDT({
        
        df_missing()
        
      }, options = list(lengthMenu = c(5,10,20,50), pageLength = 5 ), 
      selection = 'none')
      
      

      
      # Download ----------------------------------------------------------------
      
      download_xlsx_Server("download", 
                           list("Check File Submission" = df_joined_arranged(), "Missing Names" = df_missing()), 
                           filename = "Check_File_Submission.xlsx")
  
      #output$raw <- renderPrint({ df_joined() })
    }
  )
}

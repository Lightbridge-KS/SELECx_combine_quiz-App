### Module:  Extract PDF file names from SELECx (Deprecated)


library(shiny)
library(DT)

options(shiny.maxRequestSize = 100*1024^2)

img_size <- c(width = 950, height = 76) * 0.5

# UI ----------------------------------------------------------------------


list_files_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column(9,
             h2("List Files"),
      ),
      column(3,
             ### Download
             download_xlsx_UI(ns("download"), "Download Excel")
      )
    ),

    
    h5(helpText("\"List and extract information from student's submission file names.\"")),
    
    h3("Guides"),
    helpText("1) ","Before class, inform the students to include student's ID (7 digits) in the submission file names."),
    helpText("1) ","Download student's submission files from SELECx (not in subfolder). Do not rename any files."),
    
    helpText("2) ","Just upload all of them, any file type can be accepted (e.g. PDF, JPEG, etc.)."),
    br(),
    
    h5(helpText("Example file from SELECx")),
    fluidRow(
      column(12, 
             tags$img(src = "input_file_ex.png", align = "left", height = img_size["height"], width = img_size["width"])
      )
    ),
    br(),
    
    fileInput(ns("file"), NULL, multiple = TRUE,
              buttonLabel = "Upload files", 
              placeholder = "choose submission files", width = validateCssUnit("fit-content")),
    


    
    hr(),
    
    h3("Output"),
    
    tags$ul(
      tags$li(tags$b("Input:"), " Uploaded file names"),
      tags$li(tags$b("Student_ID:"), " 7 digits extracted from student's file names"),
      tags$li(tags$b("Student_Names:"), " Student's names"),
      tags$li(tags$b("File_Names:"), " File names that each students assigned"),
      tags$li(tags$b("Extension:"), " File extension")
    ),
    br(),
    
    DT::dataTableOutput(ns("table")),
    
   # verbatimTextOutput(ns("raw"))
    
  )
}


# Server ------------------------------------------------------------------


list_files_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      df <- reactive({
        
        req(input$file)
        moodle_list_files(input$file$name)
        
        
      })

# Display Table -----------------------------------------------------------

      
      output$table <- DT::renderDT({
        
        df()
        
      }, options = list(lengthMenu = c(5,10,20,50), pageLength = 5 ), selection = 'none')
      

# Download ----------------------------------------------------------------

      
      download_xlsx_Server("download", 
                           list("List Files" = df()), 
                           filename = "List_Files.xlsx")
      
      # output$raw <- renderPrint({
      #   
      # })
  
  
    }
  )
}

### Read xlsx or csv file
library(shiny)

# Fun: Read single file ---------------------------------------------------

read_single <- function(file_name, file_path) {
  
  ext <- tools::file_ext(file_name)
  switch(ext,
         csv = readr::read_csv(file_path),
         xls = readxl::read_excel(file_path),
         xlsx = readxl::read_excel(file_path),
         validate("Invalid file; Please upload a .csv, .xls or .xlsx file")
  )
  
}

# Fun: Read multiple files -----------------------------------------------------

read_multi <- function(file_name,file_path){
  
  df <- tibble::tibble(ext = tools::file_ext(file_name), path = file_path)
  
  read <- function(ext,path){  
    
    switch(ext,
           csv = readr::read_csv(path),
           xls = readxl::read_excel(path),
           xlsx = readxl::read_excel(path),
           validate("Invalid file; Please upload a .csv, .xls or .xlsx file")
           
    )
  }
  
  data_ls <- df %>% purrr::pmap(read)
  
  names(data_ls) <- stringr::str_remove(file_name,"\\.[^\\.]+$") #remove .xxx (content after last dot)
  
  data_ls
  
}

# UI - read ---------------------------------------------------------------

read_UI <- function(id, 
                    multiple = F,
                    width = NULL,
                    buttonLabel = "Upload file", 
                    placeholder = "choose file .csv or .xlsx"
                    ) {
  ns <- NS(id)
  
  tagList(
    shinyFeedback::useShinyFeedback(),
    fileInput(ns("file"), NULL, accept = c(".csv", ".xls",".xlsx"), 
              buttonLabel = buttonLabel,
              placeholder = placeholder, multiple = multiple, width = width)
    
  )
}


# Server - read -----------------------------------------------------------

#' Read CSV or Excel File(s)
#'
#' @param id Shiny id
#' @param multiple Logical: Multiple files accepted or not
#' @param warning Logical: Show shinyFeedback::feedbackWarning or not
#' @param warning_react Reactive value: If TRUE = show warning
#' @param warning_text Character: To display warning message
#'
#' @return List with 2 components
#' $file_name: file name with no extension
#' $data: 
#'   If `multiple = TRUE`, return list of data.frame named by file names with no ext.
#'   If `multiple = FALSE`, return data.frame
read_Server <- function(id, multiple = F, 
                        warning = F,
                        warning_react = NULL, # Reactive value To Warn: if TRUE = show warning
                        warning_text = "Incorrect file specification") {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Upload file  ---------------------------------------------------------------
      
      data <- reactive({
        
        req(input$file) # Require - code wait until file uploaded
        
        if(multiple){
          
          ### Read multiple files to list of DFs
          read_multi(file_name = input$file$name, file_path = input$file$datapath)
        }else{
          
          ### Read single file to df
          read_single(file_name = input$file$name, file_path = input$file$datapath)
        }
        
      })
      
      observeEvent(input$file, {
        
        if(warning){
        show_warning <- isTruthy(warning_react())
        shinyFeedback::feedbackWarning("file", show_warning, text = warning_text)
        }
        
      })
      

# Return: list containing input data,  file name ---------------------------

      out <- reactive({ 
        
        file_name <- stringr::str_remove(input$file$name, "\\.[^\\.]+$") #remove .xxx (content after last dot)
        
        list(data = data(),
             file_name = file_name)
        })
      
      return(out)
  
  
    }
  )
}

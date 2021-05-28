### Grades Filtering Type Module

library(shiny)
library(dplyr)


# Fun ---------------------------------------------------------------------

score_filt_table <- c("Maximum score of each student" = c("max_first"),
                      "Minimum score of each student" = c("min_first"),
                      "First attempt of each student" = c("all_first"),
                      "Last attempt of each student" = c("all_last"),
                      "Every attempts of each student" = c("all_all")
)

score_filt_split <- function(input_chr) {
  
  mod_chr <- input_chr %>% 
    stringr::str_split("_") %>% 
    unlist() 
  
  list(grade = mod_chr[[1]], time = mod_chr[[2]])
  
}
# UI ----------------------------------------------------------------------



grades_type_select_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    selectInput(ns("grades_type"), "Score filtering type:", choices = score_filt_table)
    
    
  )
}


# Server ------------------------------------------------------------------



#' Give named charater vector for `combine_grades` argument
#'
#' @param id shiny ID
#'
#' @return A list with names "grade", "time"
grades_type_select_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      chr_splited <- reactive({
        
        score_filt_split(input$grades_type)
        
      })
      
      return( chr_splited )
  
  
    }
  )
}

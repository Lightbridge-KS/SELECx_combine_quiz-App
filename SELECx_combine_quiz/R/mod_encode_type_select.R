### Responses Filtering Type Module

library(shiny)
library(dplyr)


# Fun ---------------------------------------------------------------------

encode_filt_table <- c("First finished attempt of each student" = c("max_first"),
                       "Last finished attempt of each student" = c("max_last"),
                       "All finished attempt(s) of each student" = c("max_all"),
                       "All finished or in-progress attempt(s) of each student" = c("all_all")
)

encode_filt_split <- function(input_chr) {
  
  mod_chr <- input_chr %>% 
    stringr::str_split("_") %>% 
    unlist() 
  
  list(encode = mod_chr[[1]], time = mod_chr[[2]])
  
}



# UI ----------------------------------------------------------------------



encode_type_select_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    selectInput(ns("encode_type"), "1. Choose attempt:", choices = encode_filt_table)
    
    
  )
}


# Server ------------------------------------------------------------------



#' Give named charater vector to `choose_encode` and `choose_time` argument of 
#' function `combine_resp` and `count_resp`.
#'
#' @param id shiny ID
#'
#' @return A list with names "encode", "time"
encode_type_select_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      chr_splited <- reactive({
        
        encode_filt_split(input$encode_type)
        
      })
      
      return( chr_splited )
      
      
    }
  )
}

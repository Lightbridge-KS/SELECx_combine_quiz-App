### Module Select ID column

library(shiny)
library(dplyr)

# Module UI ---------------------------------------------------------------


select_id_cols_UI <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(ns("add_cols"), "Add more columns from ID file?", value = F),
    uiOutput(ns("select_cols")),
    
    verbatimTextOutput(ns("raw"))
  )
}


# Module Server -----------------------------------------------------------


#' Select Columns from ID data.frame
#'
#' @param id inputID
#' @param id_df_react ID data.frame that has "Name" and "ID" column
#'
#' @return An ID data.frame that has at least "Name" and "ID" column
select_id_cols_Server <- function(id, id_df_react ) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ### Select other columns that is not `Name` and `ID` 
      id_df_no_Name_ID <- reactive({
        
        id_df_react() %>% dplyr::select(-Name, -ID)
        
        })
      
      
      output$select_cols <- renderUI({
        
        if(input$add_cols == T){
          varSelectInput(session$ns("cols"),"Choose columns:",  id_df_no_Name_ID() , multiple = TRUE)
        }
        
      })
      
      id_df_selected <- reactive({
        
        id_df_react() %>%
          dplyr::select(Name, ID, !!!input$cols)
        
      })
      
      

      # Return ID df that has column Name,  ID,  and more -----------------------

      return(id_df_selected)
      
      
      # output$raw <- renderPrint({
      #   
      #   #input$cols
      #   id_df_react() %>%
      #     select(Name, ID, !!!input$cols)
      #   
      # })
  
  
    }
  )
}

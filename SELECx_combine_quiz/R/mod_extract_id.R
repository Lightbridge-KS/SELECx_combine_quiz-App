library(shiny)



# UI ----------------------------------------------------------------------


extract_id_col_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    # checkboxInput(ns("check"),"Extract student's ID from other column?", value = F),
    # tabsetPanel(
    #   id = ns("binary"),
    #   type = "hidden",
    #   tabPanel("not_show"),
    #   tabPanel("show", 
    # 
    #            )
    # )
    
    # Extract ID from ---------------------------------------------------------
    selectInput(ns("extract_id_col"), "Column to extract student's ID: ", 
                choices = c("Email address","Institution", "Department"), 
                width = validateCssUnit("fit-content")) # 300px

  )
}


# Server ------------------------------------------------------------------


extract_id_col_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # check_string <- reactive({  
      #   
      #   if(input$check == T){"show"}else{"not_show"}
      #   
      # })
      # 
      # observeEvent(input$check,{
      #   updateTabsetPanel(session, "binary", selected = check_string())
      #   
      # })
  
      id_col <- reactive({ input$extract_id_col })
      
      return(id_col)
  
    }
  )
}

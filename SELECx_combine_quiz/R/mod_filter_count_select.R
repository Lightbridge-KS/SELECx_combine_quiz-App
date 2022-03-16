### Select Filter Count

library(shiny)
library(dplyr)

count_select_table <- c(
  "No filter applied" = "all",
  "Filter maximum responses count per student" = "max"
)

# Mod: mod_filter_count_select UI -----------------------------------------------------------------
mod_filter_count_select_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("filter"), "2. Choose filter by count:", choices = count_select_table),
    helpText("'Choose filter by count' only applies for rows in count responses table.")
    
  )
}


# Mod: mod_filter_count_select Server -----------------------------------------------------------------
mod_filter_count_select_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      filter_react <- reactive({
        input$filter
      })
      
      return(filter_react)
  
    }
  )
}

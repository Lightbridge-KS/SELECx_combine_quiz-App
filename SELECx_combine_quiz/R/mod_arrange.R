### Module Arrange Data Frame by Column
library(dplyr)
library(shiny)

# Mod: arrange UI -----------------------------------------------------------------

#' Mod UI Arrange DF by column
#'
#' @param id shiny ID
#' @param label_checkbox label of checkbox that ask to sort data
#'
arrange_UI <- function(id,
                       label_checkbox = "Sort Data?"
) {
  ns <- NS(id)
  tagList(
    checkboxInput(ns("to_sort"), label = label_checkbox, value = FALSE),
    uiOutput(ns("varSelect")),
    #verbatimTextOutput(ns("raw")),
    #verbatimTextOutput(ns("raw2"))
  )
}

# Mod: arrange Server -----------------------------------------------------------------

#' Mod Server Arrange DF by column
#'
#' @param id shiny ID
#' @param data_react A reactive Data Frame
#' @param label label of dropdown to select variable
#' @param selected The initially selected value if `NULL` first colum will be used.
#' @param multiple Multiple select or not
#'
#' @return An arranged data frame (if checkbox is not checked return input data frame)
#' 
arrange_Server <- function(id,
                           data_react,
                           label = "Choose column to sort rows by:",
                           selected = NULL,
                           multiple = FALSE
) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ### Choose Column
      output$varSelect <- renderUI({
        
        req(data_react())
        
        if(input$to_sort){
          
          varSelectInput(session$ns("col"), label = label, data = data_react(),
                         selected = selected, multiple =  multiple)
        }
        
      })
      
      ### Arrange
      data_arranged <- reactive({
        
        req(data_react())
        
        if(isTruthy(input$to_sort) && isTruthy(input$col)){
          
          if(multiple == FALSE){
            # Sort Single
            dplyr::arrange(data_react(), !!input$col)
            
          } else {
            # Sort Multi
            dplyr::arrange(data_react(), !!!input$col)
          }
          
          
        } else{
          # Do nothing
          data_react()
        }
      })
      
      # output$raw <- renderPrint({
      #   
      #   list(col = input$col, class = class(input$col), data_react = isTruthy(data_react()))
      #   
      # })
      
      # output$raw2 <- renderPrint({
      #   head(data_arranged())
      # })
      
      ### Return Sorted DF
      return(data_arranged)
      
    }
  )
}

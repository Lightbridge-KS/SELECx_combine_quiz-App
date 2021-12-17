### Module Adjust Maximum Grades

library(shiny)
library(purrr)



# UI ----------------------------------------------------------------------


adj_max_grades_UI <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(ns("check_readjust"), "Readjust maximum grades?", value = F),
    uiOutput(ns("readjust_disp"))
    # tabsetPanel(
    #   id = ns("tab_readjust"),
    #   type = "hidden",
    #   tabPanel("not_show"),
    #   tabPanel("show", 
    #            helpText("Input new maximum score below:"),
    #            br(),
    #            uiOutput(ns("readjust"))
    #   )
    # )
    
    #verbatimTextOutput(ns("raw"))
  )
}



# Server ------------------------------------------------------------------


#' Readjust Maxium grades
#'
#' @param id shiny id
#' @param data_raw_react (react) Named List of data.frame of Moodle Quiz Reports
#'
#' @return 
#'  If check box is tick: return Numeric vector for new grades
#'  If check box in not tick: return `NULL`
#' 
adj_max_grades_Server <- function(id, 
                                  data_raw_react) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Checkbox : Re-adjust ----------------------------------------------------
      
      # check_string <- reactive({  
      #   
      #   if(input$check_readjust){"show"}else{"not_show"}
      #   
      # })
      

      # Update Tabset when Tick -------------------------------------------------

      # observeEvent(input$check_readjust,{
      #   
      #   updateTabsetPanel(session, "tab_readjust", selected = check_string())
      #   
      # })
      
      output$readjust_disp <- renderUI({
        
        if(isTruthy(input$check_readjust)){
          
         UI_out <- list(
           helpText("Input new maximum score below:"),
           br(),
           uiOutput(session$ns("readjust"))
         )
         
         UI_out
        }
          
      })
      

      # Multiple Numeric Input --------------------------------------------------

      
      ids <- reactive({ names(data_raw_react()) })
      
      old_max <- reactive({ 
        
        purrr::map(data_raw_react(), get_max_grade) 
        
        })
      
      output$readjust <- renderUI({
        
        purrr::map2(.x = ids(), .y = old_max(), 
            ~shiny::numericInput(session$ns(.x), label = paste0("Max score of ", .x, ":"), value = .y, min = 0)
        )   
        
      })
      
      new_max <- reactive({ 
        
        if(input$check_readjust){
          
        purrr::map_dbl(ids(), ~input[[.x]]) 
          
        }else{
          NULL
        }
        
        })
      
      #output$raw <- renderPrint({ data_raw_react() })
      
      
      return(new_max)
      
  
  
    }
  )
}

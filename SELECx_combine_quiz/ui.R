### App: Combine Moodle Quiz Report
library(shiny)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(moodleQuiz)


ui <- navbarPage("SELECx Combine Quiz", inverse = F,
                 theme = shinythemes::shinytheme("cerulean"),
                 #theme = shinythemes::themeSelector(),
                 
                 tabPanel("Check Submission",
                          
                          check_sub_UI("check_sub")
                          
                          ),
                 tabPanel("Combine Responses",
                          
                          combine_resp_UI("combine_resp")
                          
                          ),
                 tabPanel("Combine Grades",
                          
                          combine_grades_UI("combine_grades")
                          )
    
  
)




### App: Combine Moodle Quiz Report
library(shiny)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(moodleQuiz)



server <- function(input, output, session) {
  
    check_sub_Server("check_sub")
  
    combine_resp_Server("combine_resp")
    
    combine_grades_Server("combine_grades")
    
}

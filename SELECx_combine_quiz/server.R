### App: Combine Moodle Quiz Report
library(shiny)
library(DT)
library(bslib)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(moodleQuiz)

library(markdown) # To include Markdown



server <- function(input, output, session) {
  
    #bslib::bs_themer(gfonts = TRUE, gfonts_update = FALSE)
  
    check_sub_Server("check_sub")
  
    combine_resp_Server("combine_resp")
    
    combine_grades_Server("combine_grades")
    
    #list_files_Server("list_files")
    extract_listfile_Server("extract_listfile")
    
}




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


ui <- navbarPage("SELECx Combine Quiz", inverse = F,
                 #theme = shinythemes::shinytheme("cerulean"),
                 #theme = shinythemes::themeSelector(),
                 theme = bslib::bs_theme(bootswatch = "cerulean", 
                                         "enable-gradients" = TRUE, "enable-shadows" = TRUE,
                                         primary = "#277CBC",
                                         secondary = "#E14D77",
                                         info = "#73036E"),    
                 
                 tabPanel("Check Submission",
                          
                          check_sub_UI("check_sub")
                          
                          ),
                 tabPanel("Combine Responses",
                          
                          combine_resp_UI("combine_resp")
                          
                          ),
                 tabPanel("Combine Grades",
                          
                          combine_grades_UI("combine_grades")
                          ),
                 
                 tabPanel("Check File Submission",
                          
                          #list_files_UI("list_files")
                          extract_listfile_UI("extract_listfile")
                          ),
                 
                 tabPanel("About",
                          
                          includeMarkdown("about.md")
                          )
                 
            
    
  
)




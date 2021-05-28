### Download list of DF as excel file

library(shiny)
library(purrr)

# Function: Write Custom Excel -------------------------------------------------------------

write_custom.xlsx <- function(x, filename){
  
  
  # Create Header Style
  head_style <- openxlsx::createStyle(textDecoration = "bold", 
                                      halign = "center", valign = "center", 
                                      fgFill = "#d9ead3", 
                                      border = "TopBottomLeftRight")
  
  wb <- openxlsx::write.xlsx(x, filename, 
                             headerStyle = head_style, 
                             borders = "columns")
  # Freeze First Row
  purrr::walk(names(x) , ~openxlsx::freezePane(wb, sheet = .x ,firstRow = T) )
  
  openxlsx::saveWorkbook(wb,  filename, overwrite = T)
  
}




# UI: Download botton -----------------------------------------------------


download_xlsx_UI <- function(id, botton_labs = "Download") {
  ns <- NS(id)
  tagList(
    downloadButton(ns("download_xlsx"), botton_labs)
  )
}


# Server: write excel -----------------------------------------------------


download_xlsx_Server <- function(id,
                                 x,
                                 filename = "report.xlsx" # file name in download wizard
) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$download_xlsx <- downloadHandler(
        
        filename = filename,
        content = function(file){
          
          write_custom.xlsx(x, file)

          
        }
        
      )
      
    }
  )
}
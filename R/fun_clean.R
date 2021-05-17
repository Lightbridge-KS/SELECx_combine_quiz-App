### General cleaning for Moodle Report

clean_moodle <- function(df_raw, 
                         extract_id = TRUE, id_regex = ".*", # Extract ID from Email
                         sep_name = " ", # Separate First name and Surname
                         dash_na = TRUE, # Format Dash "-" to NA
                         force_numeric = TRUE # Force format Grade and Q column to numeric
) {
  
  if(!is_report(df_raw)) stop("This is not a moodle quiz report.", call. = F)
  quiz_attr <- get_quiz_attr(df_raw)
  
  df_cleaned_1 <- df_raw %>% 
    # Filter out "Overall average" in the last row of Grade report
    dplyr::filter(!is.na(`Email address`)) %>% 
    # Select Column that use in all type of Moodler Function
    dplyr::select(tidyselect::all_of(c("First name", "Surname", 
                                       "Email address", "State", "Started on")),
                  # Select Grade column (if any)
                  tidyselect::starts_with("G"),
                  # Select Response column (if any)
                  tidyselect::starts_with("R"),
                  # Select Q. column (if any)
                  tidyselect::starts_with("Q")) %>% 
    tidyr::unite("First name", "Surname", col = "Name", sep = sep_name) %>% 
    dplyr::rename(Email = "Email address", Started ="Started on") 
  
  # Replace dash "-" with NA
  if(dash_na){
    df_cleaned_1 <- df_cleaned_1 %>% purrr::map_df(~dplyr::na_if(.x, "-"))
  }
  
  df_cleaned_1 <- df_cleaned_1 %>% 
    # Reformat Stated Date to POSIXct
    dplyr::mutate(Started = lubridate::dmy_hm(Started)) %>% 
    # Replace "/" at Grade/xx column
    dplyr::rename_with(.fn = ~str_replace(.x, "/", "_")) 
  
  # Format Grade column to numeric; Even if it's "Not yet graded" or dashed
  if(force_numeric){
    df_cleaned_1 <- df_cleaned_1 %>% 
      dplyr::mutate(
        dplyr::across(tidyselect::starts_with("G"), 
                      ~dplyr::na_if(.x, "Not yet graded")),
        dplyr::across(tidyselect::starts_with("G"), 
                      ~dplyr::na_if(.x, "-")),
        dplyr::across(tidyselect::starts_with("G"), as.numeric)
      )
  }  
  
  # If Grades Report, rename Q column and remove max
  if(is_grades_report(df_raw)){
    
    df_cleaned_2 <- df_cleaned_1 %>% 
      dplyr::rename_with(.fn = ~paste0("Q", quiz_attr$q_no), 
                         .cols =  tidyselect::starts_with("Q")) 
    if(force_numeric){
      # Format Q_xx column to numeric; Even if it's "Requires grading" or dashed
      df_cleaned_2 <- df_cleaned_2 %>% 
        dplyr::mutate(
          dplyr::across(tidyselect::starts_with("Q"), 
                        ~dplyr::na_if(.x, "Requires grading")),
          dplyr::across(tidyselect::starts_with("Q"), 
                        ~dplyr::na_if(.x, "-")),
          dplyr::across(tidyselect::starts_with("Q"), as.numeric)
        ) 
    }
  }
  # If Responses Report, rename R column
  if(is_responses_report(df_raw)){
    
    df_cleaned_2 <- df_cleaned_1 %>% 
      dplyr::rename_with(.fn = ~paste0("Response_", quiz_attr$resp_no), 
                         .cols =  tidyselect::starts_with("R"))
  }
  
  if(!extract_id) return(df_cleaned_2)
  # Extract Numeric ID from Email
  df_cleaned_2 %>% 
    dplyr::mutate(ID = as.character(stringr::str_extract(Email, id_regex)), 
                  .keep = "unused", .after = Name)
  
}
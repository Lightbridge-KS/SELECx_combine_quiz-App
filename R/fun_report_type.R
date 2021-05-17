### Moodle Report type testing: Grades vs Responses report


# Regex for testing moodle report -----------------------------------------

report_col_regex <- list(
  moodle = c("Surname", "First name", "Email address", "State", "Grade", "(Q)|(Response)"),
  grades = c("Surname", "First name", "Email address", "State", "Grade", "Q"),
  responses = c("Surname", "First name", "Email address", "State", "Grade", "Response")
)



# Get type of report: "Grades" vs "Responses", otherwise return NA -------

get_report_type <- function(data) {
  
  if(!is.data.frame(data)) stop("`data` must be a data.frame", call. = F)
  is_gr <- is_grades_report(data)
  is_resp <- is_responses_report(data)
  out <- if(is_gr){ 
    "Grades"
  }else if(is_resp){
    "Responses"
  }else{
    NA_character_
  }
  out
}


# Test if object is moodle quiz report ------------------------------------


is_report <- function(data) {
  
  if(!is.data.frame(data)) stop("`data` must be a data.frame", call. = F)
  all(is_regex_in_names(data, report_col_regex$moodle))
}

# Test if object is moodle "Grades" report ------------------------------------

is_grades_report <- function(data) {
  
  if(!is.data.frame(data)) stop("`data` must be a data.frame", call. = F)
  all(is_regex_in_names(data, report_col_regex$grades))
  
}

# Test if object is moodle "Responses" report ------------------------------------


is_responses_report <- function(data) {
  
  if(!is.data.frame(data)) stop("`data` must be a data.frame", call. = F)
  all(is_regex_in_names(data, report_col_regex$responses))
  
}


# Helper: Check if all regex in names of object -----------------------------------

### check if all elecment of regular expression are presented in names of the object

is_regex_in_names <- function(x, regex, verbose = F){
  
  nm <- names(x)
  lgl_ls <- purrr::map(nm, ~stringr::str_detect(.x, regex))
  lgl_ls_t <- purrr::map(purrr::transpose(lgl_ls), ~unlist(.x, recursive = F))
  lgl_vctr <- purrr::map_lgl(lgl_ls_t, any)
  
  if(verbose && !all(lgl_vctr)){
    no_match <- regex[which(!lgl_vctr)]
    message("The following not presented in `x`")
    print_msg(no_match, sep = ", ")
  }
  
  lgl_vctr
  
}


# Check at least one numeric in Grade column ------------------------------


is_some_grade_numeric <- function(data){
  
  require(dplyr)
  data %>% 
    dplyr::select(tidyselect::starts_with("Grade")) %>% 
    unique() %>% 
    dplyr::pull() %>% 
    # Detect start with one or more digit can be followed by dot and end with digit
    stringr::str_detect("^[:digit:]+\\.?[:digit:]+$") %>% 
    any()
  
}

# Check at least one "Not yet graded" in Grade column ------------------------------


is_some_grade_nyg <- function(data){
  
  require(dplyr)
  data %>% 
    dplyr::select(tidyselect::starts_with("Grade")) %>% 
    unique() %>% 
    dplyr::pull() %>% 
    # Detect any "Not yet graded"
    stringr::str_detect("Not yet graded") %>% 
    any()
  
}

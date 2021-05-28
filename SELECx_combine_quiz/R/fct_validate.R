

# Type of Report ----------------------------------------------------------

report_col_regex <- list(
  moodle = c("Surname", "First name", "Email address", "State", "Grade", "(Q)|(Response)"),
  grades = c("Surname", "First name", "Email address", "State", "Grade", "Q"),
  responses = c("Surname", "First name", "Email address", "State", "Grade", "Response")
)


#' Get Type of Moodle Quiz Report
#'
#' @param data A data.frame to test.
#'
#' @return Character vector: `"Grades"` for Moodle Grades Report, `Responses`  for Moodle Responses Report,
#'  or `NA` if it's not a Moodle Quiz report
#'
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


#' Is it a Moodle Quiz Report?
#'
#' @param data A data.frame to test.
#'
#' @return logical: `TRUE` if it is a Moodle Quiz Report
#'
is_report <- function(data) {
  
  if(!is.data.frame(data)) stop("`data` must be a data.frame", call. = F)
  all(is_regex_in_names(data, report_col_regex$moodle))
}


#' Is it a Moodle Grades Report?
#'
#' @param data A data.frame to test.
#'
#' @return logical: `TRUE` if it is a Moodle Grades Report
#'
is_grades_report <- function(data) {
  
  if(!is.data.frame(data)) stop("`data` must be a data.frame", call. = F)
  all(is_regex_in_names(data, report_col_regex$grades))
  
}


#' Is it a Moodle Responses Report?
#'
#' @param data A data.frame to test.
#'
#' @return logical: `TRUE` if it is a Moodle Responses Report
#'
is_responses_report <- function(data) {
  
  if(!is.data.frame(data)) stop("`data` must be a data.frame", call. = F)
  all(is_regex_in_names(data, report_col_regex$responses))
  
}


#' Is Regular Expressions presented in object names
#'
#' Vectorized testing for regular expressions.
#' Are all of the regular expression can be matched to object names or not?
#'
#' @param x An object to test
#' @param regex Character vector, specify regular expressions
#' @param verbose If `TRUE`, message you that in which components of the object names is/are not match by `regex`
#'
#' @return Logical, if `TRUE` all of the `regex` can be matched to at least one element of names of `x`.
#'
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

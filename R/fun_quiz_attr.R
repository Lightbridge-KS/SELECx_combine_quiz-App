### Moodle Quiz Attributes


# Moodle quiz report attribute ----------------------------------------


get_quiz_attr <- function(data) {
  
  if(!is_report(data)) stop("`data` is not a moodle quiz report", call. = F)
  # Maximum Grade
  max_gr <- get_max_grade(data)
  some_nyg <- is_some_grade_nyg(data) # Is some student not yet graded?
  # If data is Grade Report, Get Questions Number and Max 
  if(is_grades_report(data)){
    q_no_max <- get_questions_no_max(data)
    quiz_attr <- list(
      report_type = "Grades",
      some_nyg = some_nyg,
      grade_max = max_gr, 
      q_no = q_no_max$q_no, 
      q_max = q_no_max$q_max
    )
  }
  # If data is Responses Report, Get Responses Number
  if(is_responses_report(data)){
    resp_no <- get_responses_no(data)
    cloze_cols <- get_cloze_col_names(data)
    quiz_attr <- list(report_type = "Responses", 
                      some_nyg = some_nyg,
                      grade_max = max_gr, 
                      resp_no = resp_no,
                      cloze_cols = cloze_cols) # If NA = no cloze column
    # If response has cloze col, append them
    # if(has_cloze_col(data)){
    # cloze_cols <- get_cloze_col_names(data)
    # quiz_attr <- append(quiz_attr, list(cloze_cols = cloze_cols))
    # }
  }
  
  quiz_attr
  
}




# Maximum Grade of quiz -----------------------------------------------


get_max_grade <- function(df_raw) {
  
  # Every moodle report has Maximum in Grade column name
  nm <- names(df_raw)
  gr_colnm <- stringr::str_subset(nm, "Grade")
  # Extract digits (including decimal)
  max_gr <- stringr::str_extract(gr_colnm, "[:digit:]+\\.?[:digit:]+$") 
  as.numeric(max_gr)
  
}


# Question number and maximum score from "Grades" report --------------


get_questions_no_max <- function(df_gr) {
  
  nm <- names(df_gr)
  q_colnm <- stringr::str_subset(nm, "Q")
  # Question No: Extract first set of digits before / 
  q_number <- as.integer(stringr::str_extract(q_colnm, "[:digit:]+"))
  # Question Max: Extract everything after /
  q_max <- as.numeric(stringr::str_extract(q_colnm, "(?<=/)(.+)"))
  
  data.frame(q_no = q_number, q_max = q_max)
}


# Responses number from "Responses" report ----------------------------


get_responses_no <- function(df_resp) {
  
  nm <- names(df_resp)
  resp_colnm <- stringr::str_subset(nm, "R")
  resp_no <-  stringr::str_extract(resp_colnm, "[:digit:]+")
  as.integer(resp_no)
  
}

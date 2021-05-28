### Get Quiz Attributes

# Has cloze column --------------------------------------------------------


#' Has any Cloze column?
#'
#' @param df A data.frame of Moodle Responses report
#'
#' @return Logical: `TRUE` if `df` has at least one Cloze column.
#'
has_cloze_col <- function(df) {
  
  regex <- "part [:digit:]+:"
  is_cloze_lgl <- df %>%
    purrr::map(~stringr::str_detect(.x, regex)) %>%
    purrr::map_lgl(any)
  
  any(is_cloze_lgl, na.rm = T)
}


# Maxium Grades of Quiz -----------------------------------------------------------


#' Get a maxium grade of any Moodle quiz report.
#'
#' @param df_raw A data.frame of Moodle Quiz report
#'
#' @return Numeric vector of length 1 indicate maximum quiz grade
#'
get_max_grade <- function(df_raw) {
  
  # Every moodle report has Maximum in Grade column name
  nm <- names(df_raw)
  gr_colnm <- stringr::str_subset(nm, "Grade")
  # Extract digits (including decimal)
  max_gr <- stringr::str_extract(gr_colnm, "[:digit:]+\\.?[:digit:]+$")
  as.numeric(max_gr)
  
}

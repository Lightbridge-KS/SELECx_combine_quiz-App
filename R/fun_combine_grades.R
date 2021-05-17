### Combine Grades Moodle Report


# Generic: Combine Grade --------------------------------------------------

combine_grades <- function(data,
                           # Clean
                           id_regex = ".*", 
                           sep_name = " ", # Separate First name and Surname
                           # Adjust Grade
                           new_max_grade = NULL, 
                           ### NULL = no adjust,
                           ### (DF method): length 1 Numeric
                           ### (list method): Numeric vector same length as length `data` 
                           round_digits = 3, # NULL = no round
                           # Filter Grades
                           choose_grade = c("max", "min", "mean", "all"),
                           choose_time = c("first", "last", "all"),
                           force_grade = F,
                           ... # passed to `sep_col` of list method
) {
  
  UseMethod("combine_grades")
  
}



# List method -------------------------------------------------------------

combine_grades.list <- function(data,
                                # Clean
                                id_regex = ".*", 
                                sep_name = " ", # Separate First name and Surname
                                sep_col = "_", # Separation for State and Grade column names
                                # Adjust Grade
                                new_max_grade = NULL,
                                round_digits = 3, # If NULL = no round
                                # Filter Grades
                                choose_grade = c("max", "min", "mean", "all"),
                                choose_time = c("first", "last", "all"),
                                force_grade = F
) {
  
  if(!is_named_list_data.frame(data)) stop("`data` must be named list of data.frame", call. = F)
  is_data_rep <- purrr::every(data, is_report)
  if(!is_data_rep) stop("Elements of `data` must be Moodle Quiz report",call. = F)
  
  is_nyg_err <- all(!force_grade, purrr::every(data, is_some_grade_nyg))
  if(is_nyg_err) stop("Some students are 'Not yet graded'. If you want to grade anyway, choose `force_grade = TRUE`.", call. = F)
  
  # If new_max_grade = NULL
  if(is.null(new_max_grade)){
    new_max_grade <- replicate(length(data), NULL)
    tot_max_grade <- purrr::map_dbl(data, get_max_grade) %>% sum()
    
  }else{
    # If supply new_max_grade as numeric vector
    if(length(new_max_grade) != length(data)) stop("length of `new_max_grade` must equal to length of `data`", call. = F)
    tot_max_grade <- sum(new_max_grade)
  }
  
  data_ls <- data %>% 
    purrr::map2(.y = new_max_grade,
                ~combine_grades.data.frame(.x, 
                                           id_regex = id_regex, 
                                           sep_name = sep_name, 
                                           new_max_grade = .y, 
                                           round_digits = round_digits, 
                                           choose_grade = choose_grade, 
                                           choose_time = choose_time,
                                           force_grade = force_grade
                )
    )
  
  data_ls %>% 
    purrr::map(~dplyr::select(.x, Name, ID, State, tidyselect::starts_with("G"))) %>% 
    rename_with_ls_df_names(tidyselect::starts_with("S"), sep = sep_col) %>% 
    rename_with_ls_df_names(tidyselect::starts_with("G"), sep = sep_col) %>% 
    # Join
    purrr::reduce(dplyr::full_join, by = c("Name", "ID")) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate("Total_{tot_max_grade}" := sum(
      dplyr::c_across(tidyselect::vars_select_helpers$where(is.numeric)), na.rm = T)
    ) %>% 
    dplyr::ungroup()
  
}


# DF method ---------------------------------------------------------------

combine_grades.data.frame <- function(data,
                                      # Clean
                                      id_regex = ".*", 
                                      sep_name = " ", # Separate First name and Surname
                                      # Adjust Grade
                                      new_max_grade = NULL,
                                      round_digits = 3, # If NULL = no round
                                      # Filter Grades
                                      choose_grade = c("max", "min", "mean", "all"),
                                      choose_time = c("first", "last", "all"),
                                      force_grade = F
) {
  
  if(!is_report(data)) stop("`data` is not a Moodle Quiz report.", call. = F)
  
  is_nyg_err <- all(!force_grade, is_some_grade_nyg(data))
  if(is_nyg_err) stop("Some students are 'Not yet graded'. If you want to grade anyway, choose `force_grade = TRUE`.", call. = F)
  # Clean
  data %>% 
    clean_moodle(id_regex = id_regex, 
                 sep_name = sep_name, force_numeric = TRUE) %>% 
    # Adjust Grade
    adj_grades_moodle(new_max_grade = new_max_grade, 
                      round_digits = round_digits) %>% 
    # Filter Grade
    filter_grades_moodle(choose_grade = choose_grade, choose_time = choose_time)
  
}

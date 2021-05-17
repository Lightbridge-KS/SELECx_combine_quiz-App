### Adjust and Filter Grades Report


# Adjust Grades -----------------------------------------------------------

adj_grades_moodle <- function(data_cleaned, 
                              new_max_grade = NULL,
                              round_digits = 3 # If NULL, no round
) {
  ## Check Report type
  is_resp_rep <- stringr::str_detect(names(data_cleaned), "R") %>% any()
  is_grades_rep <- stringr::str_detect(names(data_cleaned), "Q") %>% any()
  ## Symbol for Grade_xx column
  Grade_col <- names(data_cleaned) %>% stringr::str_subset("Grade") %>% rlang::sym()
  ## Old maximum Grade
  old_max_grade <- get_max_grade(data_cleaned)
  ## If not adjust grade -> New = Old 
  if(is.null(new_max_grade)){
    new_max_grade <- old_max_grade
  }
  ## Grade adjust factor
  adj_factor <- c(new_max_grade/old_max_grade) 
  ## New Grade col name
  Grade_col_new <- rlang::sym(paste0("Grade_", new_max_grade))
  # Adjust Grade (Responses Report)
  if(is_resp_rep){
    data_adj_gr <- data_cleaned %>% 
      dplyr::mutate(!!Grade_col_new := !!Grade_col * adj_factor, 
                    .keep = "unused", .after = State)
  }
  # Adjust Grade (Grades Report)
  if(is_grades_rep){
    data_adj_gr <- data_cleaned %>% 
      # Remove Old Grade_xx Column
      dplyr::select(!tidyselect::starts_with("G")) %>% 
      # Adjust New Score for Each Questions
      dplyr::mutate(dplyr::across(tidyselect::starts_with("Q"), ~.x * adj_factor)) %>% 
      # Row Sum those Questions to New Grade_xx column
      dplyr::rowwise() %>% 
      dplyr::mutate(!!Grade_col_new := sum(
        dplyr::c_across(tidyselect::starts_with("Q")), na.rm = T
      ), .after = State) %>% 
      dplyr::ungroup() 
  }
  # Round
  if(is.null(round_digits)) return(data_adj_gr)
  data_adj_gr %>% 
    dplyr::mutate(dplyr::across(
      c(tidyselect::starts_with("G"), tidyselect::starts_with("Q")), 
      ~round(.x, digits = round_digits))
    )
  
}

# Filter Grades -----------------------------------------------------------

filter_grades_moodle <- function(data_cleaned,
                                 choose_grade = c("max", "min", "mean", "all"),
                                 choose_time = c("first", "last", "all")
) {
  
  choose_grade <- rlang::arg_match(choose_grade)
  choose_time <- rlang::arg_match(choose_time)
  # Get Grades Column name
  Grade_col <- stringr::str_subset(names(data_cleaned), "G") %>% rlang::sym()
  
  # Grouped filter by Score of each student
  filt_expr_1 <- switch (choose_grade,
                         "max" = { rlang::expr(!!Grade_col == max(!!Grade_col)) },
                         "min" = { rlang::expr(!!Grade_col == min(!!Grade_col))},
                         "mean" = { rlang::expr(!!Grade_col == mean(!!Grade_col))},
                         "all" = { rlang::expr(!!Grade_col == !!Grade_col)},
                         stop("`choose_grade` must be one of 'max', 'min', 'mean', 'all'", call. = F)
  )
  # Grouped filter by Started Time of each student
  filt_expr_2 <- switch (choose_time,
                         "first" = { rlang::expr(Started == min(Started)) },
                         "last" = { rlang::expr(Started == max(Started)) },
                         "all" = { rlang::expr(Started == Started) },
                         stop("`choose_time` must be one of 'first', 'last', 'all'", call. = F)
  )
  
  data_cleaned %>%
    dplyr::group_by(Name, ID) %>%
    dplyr::filter(!!filt_expr_1) %>% 
    dplyr::filter(!!filt_expr_2) %>%
    dplyr::ungroup()
  
}

### Moodler: Check Student's Submission


# Generic: check_sub  -----------------------------------------------------

check_sub <- function(data,
                      id_regex = ".*", # Extract ID from Email
                      sep_name = " ", # Separate First name and Surname
                      state = c("Finished", "In progress"),  
                      # value in "State" column to encode
                      encode = c(1,0),
                      # encode argument `state` to 
                      choose_encode = c("max", "min", "all"),
                      choose_time = c("first", "last", "all"),
                      ... # passed to `sep_col`
) {
  
  UseMethod("check_sub")
  
}

# Method: list for check_sub --------------------------------------------


check_sub.list <- function(data,
                           id_regex = ".*", # Extract ID from Email
                           sep_name = " ", # Separate First name and Surname
                           sep_col = "_", # Separation of State and Encode column names
                           # Encode
                           state = c("Finished", "In progress"),
                           encode = c(1,0),
                           choose_encode = c("max", "min", "all"),
                           choose_time = c("first", "last", "all")         
) {
  
  if(!is_named_list_data.frame(data)) stop("`data` must be named list of data.frame", call. = F)
  is_data_moodle_rep <- purrr::every(data, is_report)
  if(!is_data_moodle_rep) stop("Elements of `data` must be Moodle quiz report",call. = F)
  
  nm <- names(data)
  
  data_ls <- data %>% 
    purrr::map(~check_sub.data.frame(.x,
                                     id_regex = id_regex,
                                     sep_name = sep_name,
                                     state = state,
                                     encode = encode,
                                     choose_encode = choose_encode, 
                                     choose_time = choose_time))
  
  data_df <-  data_ls %>% 
    purrr::map(~dplyr::select(.x, Name, ID, State, Encode)) %>% 
    # Prefix State and Encode column with names(list)
    rename_with_ls_df_names(State, sep = sep_col) %>% 
    rename_with_ls_df_names(Encode, sep = sep_col) %>% 
    # Join list
    purrr::reduce(dplyr::full_join, by = c("ID", "Name"))
  
  # Compute Total
  data_df %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(Total = sum(dplyr::c_across(
      tidyselect::vars_select_helpers$where(is.numeric)), na.rm = T)
    ) %>% 
    dplyr::ungroup()
}

# Method: data.frame for check_sub ----------------------------------------


check_sub.data.frame <- function(data,
                                 # Clean
                                 id_regex = ".*", # Extract ID from Email
                                 sep_name = " ", # Separate First name and Surname
                                 # Encode
                                 state = c("Finished", "In progress"),
                                 encode = c(1,0),
                                 choose_encode = c("max", "min", "all"),
                                 choose_time = c("first", "last", "all")
) {
  
  if(!is_report(data)) stop("`data` is not a moodle quiz report", call. = F)
  
  data %>% 
    clean_moodle(extract_id = TRUE, id_regex = id_regex, sep_name = sep_name, 
                 dash_na = FALSE, force_numeric = FALSE) %>% 
    encode_moodle(state = state, 
                  encode = encode, 
                  choose_encode = choose_encode, 
                  choose_time = choose_time)
  
}


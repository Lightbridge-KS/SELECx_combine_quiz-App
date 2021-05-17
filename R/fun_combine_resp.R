### Moodler: Combine Response



# Generic: Combine Responses ----------------------------------------------


combine_resp <- function(data,
                         # Clean
                         id_regex = ".*", # Extract ID from Email
                         sep_name = " ", # Separate First name and Surname
                         # Encode
                         state = c("Finished", "In progress"),
                         encode = c(1,0),
                         choose_encode = c("max", "min", "all"),
                         choose_time = c("first", "last", "all"),
                         # Split cloze
                         split_cloze = F, 
                         part_glue = "_part_",
                         ... # passed to sep_col arg
) {
  
  UseMethod("combine_resp")
}



# List Method: Combine Responses ------------------------------------------

combine_resp.list <- function(data,
                              # Clean
                              id_regex = ".*", # Extract ID from Email
                              sep_name = " ", # Separate First name and Surname
                              sep_col = "_", # Separation for State and Response column names
                              # Encode
                              state = c("Finished", "In progress"),
                              encode = c(1,0),
                              choose_encode = c("max", "min", "all"),
                              choose_time = c("first", "last", "all"),
                              # Split cloze
                              split_cloze = F, 
                              part_glue = "_part_"
) {
  
  if(!is_named_list_data.frame(data)) stop("`data` must be named list of data.frame", call. = F)
  is_data_resp_rep <- purrr::every(data, is_responses_report)
  if(!is_data_resp_rep) stop("Elements of `data` must be Moodle Responses report",call. = F)
  data %>% 
    purrr::map(~combine_resp.data.frame(.x,
                                        # Clean
                                        id_regex = id_regex, # Extract ID from Email
                                        sep_name = sep_name, # Separate First name and Surname
                                        # Encode
                                        state = state,
                                        encode = encode,
                                        choose_encode = choose_encode, 
                                        choose_time = choose_time,
                                        # Split cloze
                                        split_cloze = split_cloze, 
                                        part_glue = part_glue
    )) %>% 
    purrr::map(~dplyr::select(.x, Name, ID, State, starts_with("R"))) %>% 
    # Prefix column "Responses" and "State" with names(list)
    rename_with_ls_df_names(.cols = starts_with("R"), sep = sep_col) %>% 
    rename_with_ls_df_names(.cols = starts_with("S"), sep = sep_col) %>% 
    
    purrr::reduce(dplyr::full_join, by = c("ID", "Name"))
  
}
# Data Frame Method: Combine Responses ------------------------------------------

combine_resp.data.frame <- function(data,
                                    # Clean
                                    id_regex = ".*", # Extract ID from Email
                                    sep_name = " ", # Separate First name and Surname
                                    # Encode
                                    state = c("Finished", "In progress"),
                                    encode = c(1,0),
                                    choose_encode = c("max", "min", "all"),
                                    choose_time = c("first", "last", "all"),
                                    # Split cloze
                                    split_cloze = F, 
                                    part_glue = "_part_"
) {
  
  if(!is_responses_report(data)) stop("`data` is not a Moodle Responses report.", call. = F)
  
  data_enc <- data %>% 
    clean_moodle(id_regex = id_regex, sep_name = sep_name, 
                 force_numeric = FALSE, dash_na = FALSE) %>% 
    encode_moodle(state = state, encode = encode, 
                  choose_encode = choose_encode, 
                  choose_time = choose_time)
  
  # If not split cloze column; return
  if(!split_cloze) return(data_enc) 
  # If choose to split_cloze and not found cloze col; Error
  if(!has_cloze_col(data_enc)) stop("This Moodle Responses report has no Cloze column to split")
  
  cloze_col_nm <- get_cloze_col_names(data_enc)
  split_cloze(data_enc, part_glue = part_glue)
  
}
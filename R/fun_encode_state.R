### Encode State from Cleaned DF

# Encode "State" column from cleaned report -------------------------------

encode_moodle <- function(df_cleaned, 
                          state = c("Finished", "In progress"),
                          encode = c(1,0),
                          choose_encode = c("max", "min", "all"),
                          choose_time = c("first", "last", "all")
) {
  
  if(length(state) != length(encode)) stop("`state` and `encode` must have same length", call. = F)
  
  choose_encode <- rlang::arg_match(choose_encode)
  choose_time <- rlang::arg_match(choose_time)
  
  df_encoded <- df_cleaned %>% 
    dplyr::mutate(Encode = encoder(State, state, encode), .after = "State")
  
  # "max", "min", "all" = maximum, minimum or all of encoding 
  filt_expr_1 <- switch (choose_encode,
                         "max" = {rlang::expr(Encode == max(Encode))},
                         "min" = {rlang::expr(Encode == min(Encode))},
                         "all" = {rlang::expr(Encode == Encode)},
                         stop("Must be one of `max`, `min`, `all`", 
                              call. = F)
  )
  # "first", "last" = first or last attempt  
  filt_expr_2 <- switch (choose_time,
                         "first" = {rlang::expr(Started == min(Started))},
                         "last" = {rlang::expr(Started == max(Started))},
                         "all" = {rlang::expr(Started == Started)},
                         stop("Must be one of `max`, `min`, `all`", 
                              call. = F)
  )
  
  df_encoded %>% 
    dplyr::group_by(Name, ID) %>% 
    dplyr::filter(!!filt_expr_1) %>% 
    dplyr::filter(!!filt_expr_2) %>% 
    dplyr::ungroup()
  
}

# Encoder: general encoding function --------------------------------------


encoder <- function(x, # Any vector
                    match, 
                    encode = match # Encode that pair with match
) {
  
  if(length(match) != length(encode)) stop("`match` and `encode` must have same length", call. = F)
  
  df <- data.frame(match = match, encode = encode)
  index <- match(x, df$match)
  
  df$encode[index]
  
}
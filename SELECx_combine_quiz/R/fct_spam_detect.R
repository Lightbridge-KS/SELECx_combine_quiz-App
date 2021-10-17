### Spam Word Detection from Responses


# Inappropriate Words -----------------------------------------------------

### GG Sheet: https://docs.google.com/spreadsheets/d/15OoP48G5YwnLz7YEn01VFlpd2tGkYlo89UlTVaxwW5o/edit#gid=0

inapp_word_ls <- list(
  pattern = c("^[:blank:]+$", "^[:punct:]$"), 
  th_word =  c( "ไม่รู้","ไม่ทราบ","ไม่เข้าใจ","ทำไม่ได้","ตอบไม่ได้"),
  recur_letters = c(paste0(letters,"{4,}"), paste0(LETTERS,"{4,}"))
)


# Report Spam Word --------------------------------------------------------



#' Report Spam Word from Data Frame
#'
#' @param data A data frame
#' @param .cols <tidyselect> Column(s) to extract spam word from
#' @param patterns (Characters) Character vector comprises regular expression(s) of spam word for matching
#' @param name (Character) Name of the output column containing spam word report
#'
#' @return An input data frame with spam word report added to a new column
#' 
report_spam_words <- function(data, .cols, patterns, name = "Caution") {
  
  name <- dplyr::ensym(name)
  
  # Extract matches into DF
  matches_df <- data %>% 
    dplyr::select({{.cols}}) %>% 
    purrr::map_df(~str_extract_all_regexes(.x, patterns))
  
  # Transpose to get column names to each rows 
  matches_ls <- purrr::transpose(matches_df) 
  
  # Construct spam report row by row
  report_chr <- character(nrow(data))
  
  for (i in 1:nrow(data)) {
    
    matches_row_i <- matches_ls[[i]]
    report_chr[[i]] <- paste_values_to_keys(matches_row_i)
  }
  
  # Add spam report to a new column
  data %>% 
    dplyr::mutate(!!name := report_chr)
  
}


# Extract All Regexes -----------------------------------------------------



#' Extract all Regexes as Character Vectors
#'
#' @param string Text to search
#' @param patterns Character vector (length can be > 1) of regexes to match `string`
#' @param quote (Character) Quote that wrap individual matched character
#' @param collapse (Character) If multiple matches to `regexes` were found, how it will be collapsed.
#'
#' @return A matched character vector
#' 
str_extract_all_regexes <- function(string, 
                                    patterns, 
                                    quote = "`",
                                    collapse = ", "
) {
  
  ## Surround parenthesis and collapse with `|` to regexes
  pat <- paste(paste0("(", patterns, ")" ), collapse = "|")
  double_quote <- paste0(quote, quote)
  NA_wrap_quote <- paste0(quote, "NA", quote)
  
  
  ## Extracted Character Vector (multiple matches will collapse with `,`)
  texts <- stringr::str_extract_all(string, pat) %>% 
    purrr::map(~paste0(quote, .x, quote)) %>% 
    purrr::map(~dplyr::na_if(.x, double_quote)) %>% 
    purrr::map(~dplyr::na_if(.x, NA_wrap_quote)) %>% 
    purrr::map(~paste(.x, collapse = collapse)) %>% 
    purrr::map_chr(~dplyr::na_if(.x, "NA"))
  
  texts
}


# Paste Values and Keys ---------------------------------------------------


#' Paste Values and Keys
#'
#' @param x A vector
#' @param nm Name or Key corresponding to `x`
#' @param na_omit (Logical) `TRUE` omit values that is `NA`
#'
#' @return length 1 character vector
paste_values_to_keys <- function(x, 
                                 nm = names(x), 
                                 na_omit = TRUE
) {
  
  if(is.list(x)) x <- unlist(x) # if x is list, flatten it
  if(all(is.na(x))) return(NA_character_) # All element is `NA` return `NA`
  
  lgl <- if (na_omit) !is.na(x) else TRUE
  y <- if (na_omit)  na.omit(x) else x
  
  paste0("found: ",
         paste0(y,  " in ", nm[lgl], collapse = "; ")
  )
}

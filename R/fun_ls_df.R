### Logical Utils


# Test if object is list of data.frame ------------------------------------


is_list_data.frame <- function(x, quiet = FALSE) {
  
  # Check if outer layer is data.frame
  if(is.data.frame(x)){
    if(!quiet) message("`x` is a data.frame, not a list of data.frame")
    return( FALSE )
  }
  
  # Check if outer layer is list
  is_list <- inherits(x, "list") # this return TRUE if list, otherwise FALSE even if DF
  if(!is_list){
    if(!quiet) message("`x` is not a list")
    return( is_list )
  }
  
  # Check if every elements of that list inherit data.frame class
  is_element_df <- all(purrr::map_lgl(x, ~inherits(.x, "data.frame")))
  if(!is_element_df){
    if(!quiet) message("`x` is a list but its element is not all data.frame")
    return( is_element_df )
  }
  
  is_list & is_element_df
}



# Test if object is named list of data.frame ------------------------------


is_named_list_data.frame <- function(x, quiet = FALSE) {
  
  # Check if outer layer is data.frame
  if(is.data.frame(x)){
    if(!quiet) message("`x` is a data.frame, not a list of data.frame")
    return( FALSE )
  }
  
  # Check if outer layer is list
  is_list <- inherits(x, "list") # this return TRUE if list, otherwise FALSE even if DF
  if(!is_list){
    if(!quiet) message("`x` is not a list")
    return( is_list )
  }
  # Check if list has names
  if(is.null(names(x))){
    if(!quiet) message("`x` is list but has no names")
    return( FALSE )
  }
  
  # Check if every elements of that list inherit data.frame class
  is_element_df <- all(purrr::map_lgl(x, ~inherits(.x, "data.frame")))
  if(!is_element_df){
    if(!quiet) message("`x` is a named list but its element is not all data.frame")
    return( is_element_df )
  }
  
  is_list & is_element_df
}
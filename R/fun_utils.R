### Misc Helper fn

# Print character vector to message with custom sep -----------------------


print_msg <- function(x, ... , sep = "\n", domain = NULL, appendLF = TRUE){
  
  x_comb <- purrr::reduce(x, paste, sep = sep)
  message(x_comb, ..., domain = domain, appendLF = appendLF)
  
}
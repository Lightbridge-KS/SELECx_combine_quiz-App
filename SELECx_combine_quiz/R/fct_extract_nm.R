### Extract file name


# Bind to Tibble: input & extracted names ---------------------------------

moodle_list_files <- function(path
) {
  require(purrr)
  
  input_files <- fs::path_file(path)
  ext <- fs::path_ext(path) 
  
  # Column bind vector
  df_1 <- c(Student_ID = "digits7", 
            Student_Names = "student_nm",
            File_Names = "file_nm") %>% 
    
    purrr::map_dfc(~moodle_extract_file_nm(path, type = .x, remove_ext = TRUE))
  
  # Bind Input files and Extension
  dplyr::bind_cols(Input = input_files,
                   df_1,
                   Extension = ext)
  
}

# Extract file names ------------------------------------------------------


moodle_extract_file_nm <- function(path, 
                                   regex = ".*", 
                                   type = c("file_nm", 
                                            "student_nm", 
                                            "file_nm_student_nm",
                                            "student_nm_file_nm",
                                            "digits7",
                                            "digits7_pm",
                                            "digits7_student_nm",
                                            "custom"),
                                   remove_ext = FALSE
){
  
  
  type <- match.arg(type)
  
  path <- fs::path_file(path) # Get base name
  ext <- fs::path_ext(path)
  name_noext <- fs::path_ext_remove(path)
  
  file_nm  <- stringr::str_extract(name_noext, "(?<=_assignsubmission_file_).*")
  student_name <- stringr::str_extract(
    name_noext, ".*(?=_[:digit:]*_assignsubmission_file_)"
  )
  
  # Student ID's
  digits7_pure <- tidyr::replace_na(stringr::str_extract(file_nm, "([:digit:]{7})"), "")
  non_digits7 <- tidyr::replace_na(stringr::str_extract(file_nm,"[^([:digit:]{7})]+"), "")
  digits7_pm <- paste0(digits7_pure, non_digits7)
  
  name_noext_filt <- switch (type,
                             "file_nm" = file_nm,
                             "student_nm" = student_name,
                             "file_nm_student_nm" = stringr::str_c(file_nm, student_name, sep = "_"),
                             "student_nm_file_nm" = stringr::str_c(student_name, file_nm, sep = "_"),
                             "digits7" = digits7_pure,
                             "digits7_pm" = digits7_pm,
                             "digits7_student_nm" = stringr::str_c(digits7_pure, student_name, sep = "_"),
                             "custom" = stringr::str_extract(name_noext, regex)
  )
  
  name_out <- fs::path(name_noext_filt, ext = ifelse(remove_ext, "", ext))
  name_out
  
}
### Rename list of Data.frame



# Prefix/Suffix names of ls_df to multiple columns ------------------------

rename_with_ls_df_names <- function(ls_df, 
                                    .cols,
                                    name_position = c("prefix", "suffix"),
                                    sep = "_"
){
  
  name_position <- match.arg(name_position)
  nm <- names(ls_df)
  ls <- vector("list", length(ls_df))
  
  name_fun <- switch (name_position,
                      "prefix" = { ~paste0(nm[[i]], sep ,.x)  },
                      "suffix" = { ~paste0(.x, sep , nm[[i]]) }
  )
  
  for (i in seq_along(nm)) {
    
    ls[[i]] <- dplyr::rename_with(ls_df[[i]], .cols = {{.cols}}, name_fun)
    
  }
  
  names(ls) <- nm
  ls
  
}

# Vectorized naming of column names in list of DF  ------------------------

rename_with_ls_df <- function(ls_df, .cols, 
                              new_names = names(ls_df) # Or character length = length(ls_df)
){
  
  ls <- vector("list", length(ls_df))
  
  for(i in seq_along(ls_df)){
    ls[[i]] <- dplyr::rename_with(ls_df[[i]], .fn = ~new_names[[i]],  .cols = {{.cols}})
  }
  
  attributes(ls) <- attributes(ls_df)
  ls
}


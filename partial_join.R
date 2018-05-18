partial_join <- function(x, y, by_x, pattern_y, excl){
  idx_x <- sapply(y[[pattern_y]], grep, x[[by_x]], ignore.case = TRUE)
  idx_y <- sapply(seq_along(idx_x), function(i) rep(i, length(idx_x[[i]])))
  
  df <- dplyr::bind_cols(x[unlist(idx_x), , drop = F],
                         y[unlist(idx_y), , drop = F])
  df <- df %>% distinct(medcode, readcode, .keep_all = TRUE) %>% filter(!grepl(paste(excl,collapse="|"), desc, ignore.case = TRUE)) %>% 
    arrange(group, medcode) %>% select(-descr)
  colnames(df)[3] <- "Description"
  return(df)
}

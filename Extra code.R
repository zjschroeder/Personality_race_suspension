############ Not working significant values filter function - doesn't save to global env

sig_values_funct <- function(df){
  sig_values <- list(NULL)
  for(i in seq_along(df)){
    sig_values[[i]] <- summary(df[[i]]$contrasts) %>% 
      as_tibble(.) %>% 
      filter(`p.value` < .05)
  }
  name <- deparse(substitute(df))
  names(sig_values) <- paste0(names(df))
  assign(
    x = paste0(name, "_rxg"),
    value = sig_values,
    envir = .GlobalEnv)
}
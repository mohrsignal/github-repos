contains_R <- function(df) {
  
  any(df$name == "R")
}

detect_cooccurence <- function(language1, language2) {
  
  print(paste0(language1, " and ", language2))
  
  sum(map_lgl(repos$language, 
              ~ language1 %in% .x$name & language2 %in% .x$name))
}
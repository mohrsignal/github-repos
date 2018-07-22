# library(tidyverse)

byte_compare_histogram <- function(df, compare = TRUE) {
  
  g <- ggplot(df) +
    geom_histogram(aes(x=bytes)) + 
    scale_x_log10() +
    theme_minimal() +
    theme(strip.text = element_text(size = 14))

  if (compare) {
    g <- g +
      facet_wrap(~name, scales = "free_y") +
      labs(title = "Comparison of Repository Size",
           subtitle = "x scale is logarithmic; note different y scales")
    
  } else {
    g <- g +
      labs(title = "R Repo Sizes",
           subtitle = "x scale is logarithmic")
  }
  
  
  g
}

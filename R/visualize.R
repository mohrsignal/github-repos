# library(tidyverse)

# labs(title = "Comparison of Repository Size",
#      subtitle = "x scale is logarithmic; note different y scales")
# labs(title = "R Repo Sizes",
#      subtitle = "x scale is logarithmic")
compare_histogram <- function(df, x,
                              compare = TRUE,
                              log_scale = TRUE,
                              title = NULL,
                              subtitle = NULL,
                              caption = NULL) {
  
  x <- enquo(x)
  
  g <- ggplot(df) +
    geom_histogram(aes(x = !!x)) + 
    labs(title = title,
         subtitle = subtitle) +
    theme_minimal() +
    theme(strip.text = element_text(size = 14))

  if (log_scale) {
    
    g <- g + scale_x_log10()
  }
  
  if (compare) {
    
    g <- g + facet_wrap(~name)
  }
  
  g
}

barchart <- function(df, x, y,
                     title = NULL,
                     subtitle = NULL,
                     caption = NULL) {

  x <- enquo(x)
  y <- enquo(y)

  ggplot(df) + 
    geom_col(aes(x = reorder(!!x, !!y), y = !!y)) +
    coord_flip() +
    labs(title = title,
         subtitle = subtitle,
         caption = caption) +
    theme_minimal()
}

network_graph <- function(network) {
  
  ggraph(network) + 
    #geom_edge_link(aes(edge_colour = weight)) +
    geom_edge_link(aes(edge_width = weight), alpha = 0.5) +
    geom_node_point() +
    geom_node_text(aes(label = label), repel = TRUE) +
    scale_edge_color_gradient(low = "gray90",
                              high = "gray20") +
    scale_edge_width_continuous(range = c(0.1, 2)) +
    theme_graph()
}
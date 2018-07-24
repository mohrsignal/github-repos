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
                     caption = NULL,
                     xlab = NULL,
                     ylab = NULL) {
  
  x <- enquo(x)
  y <- enquo(y)
  
  ggplot(df) + 
    geom_col(aes(x = reorder(!!x, !!y), y = !!y),
             fill = rgb(136, 189, 230, max = 255)) +
    coord_flip() +
    labs(title = title,
         subtitle = subtitle,
         caption = caption,
         x = xlab,
         y = ylab) +
    theme_minimal() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(panel.background = element_blank(),
          axis.line = element_line(color = rgb(140, 140, 140, max = 255)),
          strip.text = element_blank(),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 11, angle = 0),
          axis.text = element_text(size = 11),
          axis.ticks = element_blank())
}

dotplot <- function(df, x, y,
                    title = NULL,
                    subtitle = NULL,
                    caption = NULL,
                    xlab = NULL,
                    ylab = NULL) {
  
  x <- enquo(x)
  y <- enquo(y)
  
  ggplot(df) + 
    geom_point(aes(x = reorder(!!x, !!y), y = !!y),
               color = rgb(93, 165, 218, max=255),
               size = 2) +
    coord_flip() +
    labs(title = title,
         subtitle = subtitle,
         caption = caption,
         x = xlab,
         y = ylab) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme_minimal() +
    theme(panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(linetype = "dashed",
                                            color = "grey80"), 
          axis.line = element_line(color = rgb(140, 140, 140, max = 255)),
          strip.text = element_blank(),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 11, angle = 0),
          axis.text = element_text(size = 11),
          axis.ticks = element_blank())
}

network_graph <- function(network) {
  
  ggraph(network) + 
    geom_edge_link(aes(edge_width = weight), alpha = 0.5) +
    geom_node_point() +
    geom_node_text(aes(label = label), repel = TRUE) +
    scale_edge_color_gradient(low = "gray90",
                              high = "gray20") +
    scale_edge_width_continuous(range = c(0.1, 2)) +
    theme_graph()
}
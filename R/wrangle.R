contains_language <- function(df, language) {
  
  any(df$name == language)
}

detect_cooccurence <- function(language1, language2, df) {
  
  if (is.null(df$language)) {
    stop("Data frame has no langauge column")
  }
  
  print(paste0(language1, " and ", language2))
  
  sum(map_lgl(df$language, 
              ~ language1 %in% .x$name & language2 %in% .x$name))
}

make_edge_list <- function(x, repos = r_repos) {
  
  gtools::combinations(length(x), 2, x) %>% 
    as.tibble() %>%
    mutate(weight = map2_int(V1, V2, detect_cooccurence, r_repos))
}

make_node_list <- function(x, with_id = FALSE) {
  
  if (with_id) {
    
    data.frame(label = x)%>% 
      rowid_to_column()
  } else {
    
    data.frame(label = x) 
  }
}

make_network <- function(nodes,
                         edges,
                         cutoff) {
  
  edges_cutoff <-
    edges %>%
    filter(weight > cutoff)
  
  nodes_cutoff <-
    nodes %>%
    filter(label %in% edges_cutoff$V1
           | label %in% edges_cutoff$V2) %>% 
    rowid_to_column("id")
  
  edges_cutoff <-
    edges_cutoff %>% 
    left_join(nodes_cutoff, by=c("V1" = "label")) %>%
    rename(from = id) %>%
    left_join(nodes_cutoff, by=c("V2" = "label")) %>%
    rename(to = id) %>%
    select(from, to, weight)
  
  tbl_graph(nodes = nodes_cutoff, edges = edges_cutoff, directed = FALSE)
}

count_r_cooccurences(x) <- function(x) {
  
  data.frame(name = x) %>% 
    mutate(r_count = map_int(name, detect_cooccurence, "R", r_repos))
}

create_connection <- function() {
  
  project_id <- bigrquery::bq_projects()
  
  con <- DBI::dbConnect(bigrquery::bigquery(), 
                        project = "bigquery-public-data", 
                        dataset = "github_repos", 
                        billing = project_id)
}

get_langauge_repos <- function(con, language_name) {
  
  languages <- tbl(con, "languages")
  
  languages %>% 
    as.tibble() %>% 
    filter(map_lgl(language,
                   contains_language,
                   language_name))
}

# https://www.jessesadler.com/post/network-analysis-with-r/

create_connection <- function() {
  
  project_id <- bigrquery::bq_projects()
  
  con <- DBI::dbConnect(bigrquery::bigquery(), 
                        project = "bigquery-public-data", 
                        dataset = "github_repos", 
                        billing = project_id)
}

# https://www.jessesadler.com/post/network-analysis-with-r/
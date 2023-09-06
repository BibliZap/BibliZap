#v2.0 
#Now relying on Lens.org

library(httr)
library(tidyr)
library(dplyr)
library(maditr)
library(tibble)

citation_network <- function(article_list, type, token='TdUUUOLUWn9HpA7zkZnu01NDYO1gVdVz71cDjFRQPeVDCrYGKWoY')
{
  lens_request <- paste0('{\n\t"query": {\n\t\t"terms": {\n\t\t\t"',type,'": ["', paste0('', paste(article_list, collapse = '", "'), '"'),']\n\t\t}\n\t},\n\t"size":1000\n}')  
  data <- getLENSData(token, lens_request)
  
  
  # report requests remaining within the limit (currently 50/min)
  requests_remaining <- data[["headers"]][["x-rate-limit-remaining-request-per-minute"]]
  print(paste0('Remaining requests = ', requests_remaining))
  
  # extract the JSON content of the response
  record_json <- httr::content(data, "text")
  
  # convert json output from article search to list
  record_list <- jsonlite::fromJSON(record_json) 
  print('Results converted to df from JSON output:')
  
  # report number of input articles returned
  input_number <- record_list[["total"]]
  
  # list input article lens IDs (for later use)
  articles_id <- record_list[["data"]][["lens_id"]]
  print('Returned records from input request:')

  
  ### search for references of input articles
  
  # references per article
  
  cit_by <- unlist(record_list[["data"]][["scholarly_citations"]])
  references <- unlist(record_list[["data"]][["references"]])
  corpus<-c(cit_by,references)
  print(length(corpus))
  return(corpus)
  
}


SnowBall <- function(article_list, ndisp=50, depth=2, token='TdUUUOLUWn9HpA7zkZnu01NDYO1gVdVz71cDjFRQPeVDCrYGKWoY') {
  print(depth)
  if (depth>5)
  {type="error"}
  article_list<-toString(article_list)
  if (grepl("10.",article_list, fixed=TRUE))
      {type='DOI'}
  else if (grepl("-", article_list, fixed=TRUE))
  {type="lens_id"}
  else
  {type='PMID'}
  print(type)
  article_list <- trimws(unlist(strsplit(article_list, '[,]')))
  ## input article search
  # build query for input article search
  corpus<- citation_network(article_list, type)
  type='lens_id'
  i=1
  print(i)
  while(i< depth)
  {corpus<- citation_network(corpus, type)
  i<-i+1
  print(i)}
  
  
  
  df_final<- as.data.frame(table(corpus))
  df_final <- df_final %>% arrange(desc(Freq))
  df_final <- df_final[1:ndisp,]
  
  article_list<-as.character((df_final$corpus))
  colnames(df_final)<-c("lens_id", "Freq")
  
  url <- 'https://api.lens.org/scholarly/search'
  headers <- c('Authorization' = token, 'Content-Type' = 'application/json')
  includes = c("lens_id","title", "authors", "abstract", "external_ids", "scholarly_citations", "references")
  request <- paste0('{\n\t"query": {\n\t\t"terms": {\n\t\t\t"','lens_id','": ["', paste0('', paste(article_list, collapse = '", "'), '"'),']\n\t\t}\n\t},\n\t"include":["', paste(includes, collapse = '", "'), '"],\n\t"size":1000\n}')
  json = httr::POST(url = url, httr::add_headers(.headers=headers), body = request) |> httr::content("text") |> jsonlite::fromJSON()
  
  get_id = function(external_id, id_type = "doi") {
    external_id |>
      filter(type == id_type) |>
      pull(value)
  }
  
  get_first_author = function(authors) {
    authors %>%
      mutate(name = paste(first_name, last_name)) %>%
      filter(row_number()==1) %>% pull(name)
  }
  
  complete_articles = json$data |> as_tibble() |>
    #select(authors, title, abstract) |>
    select(lens_id, title, abstract) |>
    mutate(doi = sapply(json$data$external_ids, get_id, "doi")) 
  #mutate(first_author = sapply(json$data$authors, get_first_author)) %>% 
  #select(-authors) %>% 
  df_merged <- merge(df_final,complete_articles,by="lens_id")
  df_merged <- df_merged %>% arrange(desc(Freq))

  return(df_merged)

}




getLENSData <- function(token, query){
  url <- 'https://api.lens.org/scholarly/search'
  headers <- c('Authorization' = token, 'Content-Type' = 'application/json')
  httr::POST(url = url, httr::add_headers(.headers=headers), body = query)
}

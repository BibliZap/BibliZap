library(httr)
library(tidyr)
library(dplyr)
library(maditr)
library(tibble)

source("lens.R")

citation_network <- function(id_list, forward, backward, api_key='TdUUUOLUWn9HpA7zkZnu01NDYO1gVdVz71cDjFRQPeVDCrYGKWoY') {
  id_list<-na.omit(id_list)
  if(length(id_list) == 0 || is.na(id_list[1])) {
    return(NA)
  }
  
  print(sprintf('Fetching references and citations for %s articles', length(id_list)))
  
  dd = request_lens_df(id_list,
                       includes = c("lens_id", "references", "scholarly_citations"),
                       api_key=api_key)
  
  cit_by <- unlist(dd[["scholarly_citations"]])
  references <- unlist(dd[["references"]])
  if (forward==TRUE){
    if (backward==TRUE){
  corpus = c(cit_by,references)}
    else{corpus = c(references)}}
  else if (backward==TRUE){
    corpus = c(cit_by)}
  else{return(NA)}
  
  if(is.null(corpus)) {
    return(NA)
  }
  return(corpus)
}

pubmed_complete = function(data) {
  missing_abstracts_but_found_pmid = data |>
    filter(is.na(abstract)) |>
    drop_na(pmid) |>
    pull(pmid)
  
  pubmed_raw_features = paste("https://pubmed.ncbi.nlm.nih.gov/?term=", paste(missing_abstracts_but_found_pmid, collapse = "+"), "&show_snippets=off&format=pubmed&size=200", sep="") |> 
    rvest::read_html() |> 
    as.character()
  
  pmid_abstract = tibble(pmid = pubmed_raw_features |> # re-extraction of PMID, as a safety measure if order or count has changed
                           str_extract_all("(?<=PMID- ).+") |>
                           unlist(),
                         abstract = strsplit(pubmed_raw_features, "PMID- ")[[1]][-1] |>
                           str_extract("(?<=AB  - )([\\s\\S]*?)(?=[A-Z]{2,}\\s{1,}-)"),
  )
  
  pubmed_complete = data |>
    left_join(pmid_abstract, by="pmid") |> 
    mutate(abstract = coalesce(abstract.x, abstract.y))
  
  pubmed_complete
}

snowball_bibliography <- function(id_list, forward=TRUE, backward=TRUE, ndisp=50, depth=2, api_key='TdUUUOLUWn9HpA7zkZnu01NDYO1gVdVz71cDjFRQPeVDCrYGKWoY') {
  id_list<- gsub(" ", "", id_list)
  corpus_list = list()
  corpus_list[[1]] = id_list
  
corpus_list_asc1 = citation_network(id_list, backward, forward=FALSE, api_key=api_key)
corpus_list_desc1 = citation_network(id_list, forward, backward=FALSE, api_key=api_key)
corpus_list_asc2 <- c(corpus_list_asc1, citation_network(corpus_list_asc1, backward, forward=FALSE, api_key=api_key))
corpus_list_desc2 <- c(corpus_list_desc1, citation_network(corpus_list_asc1, forward, backward=FALSE, api_key=api_key))
corpus_list_desc2 <- c(corpus_list_desc2, citation_network(corpus_list_desc1, forward, backward=FALSE, api_key=api_key))
corpus_list_asc2 <- c(corpus_list_asc2, citation_network(corpus_list_desc1, backward, forward=FALSE, api_key=api_key))
chiffres_uniques <- union(corpus_list_asc2, corpus_list_desc2)

df <- data.frame(lens_id = chiffres_uniques)

df$freq_desc <- sapply(chiffres_uniques, function(x) sum(corpus_list_desc2 == x))

df$freq_asc <- sapply(chiffres_uniques, function(x) sum(corpus_list_asc2 == x))

df$freq <- df$freq_asc+df$freq_desc

df_freq <- df %>% arrange(desc(freq)) %>% slice(1:ndisp)

  
  includes = c("lens_id","title", "authors", "abstract", "external_ids", "scholarly_citations_count", "source", "year_published")
  
  complete_articles = df_freq$lens_id |>
    as.character() |> 
    request_lens_df(includes) |>
    as_tibble() |>
    mutate(doi = sapply(external_ids, get_id, "doi")) |> 
    mutate(journal = source$title) |>
    mutate(pmid = sapply(external_ids, get_id, "pmid")) |>
    left_join(df_freq, by="lens_id") |> 
    arrange(-freq) |> 
    pubmed_complete() |> 
    select(authors,year_published, journal, title, abstract, scholarly_citations_count, freq, freq_asc, freq_desc,doi)
  
  for (i in 1:ndisp){
    complete_articles$authors[i]<-paste0(complete_articles$authors[[i]][[3]][[1]], ". ",complete_articles$authors[[i]][[2]][[1]])
  }
  
  
  return(complete_articles)
}


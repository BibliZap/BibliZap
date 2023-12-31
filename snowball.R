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

snowball_bibliography <- function(id_list, unwanted_list=c("empty"), forward=TRUE, backward=TRUE, ndisp=50, depth=2, api_key='TdUUUOLUWn9HpA7zkZnu01NDYO1gVdVz71cDjFRQPeVDCrYGKWoY') {
  id_list<- gsub(" ", "", id_list)

  unwanted_list <- unlist(strsplit(unwanted_list, split = "[, ]+"))  
  print(is.numeric(ndisp))
  print(is.numeric(length(unwanted_list)))
  if (length(unwanted_list)>0){
    ndisp<-as.numeric(ndisp)+length(unwanted_list)
    }
  corpus_list = list()
  corpus_list[[1]] = id_list
  for(i in 1:depth) {
    print(sprintf('Depth %d', i))
    corpus_list[[i+1]] = citation_network(corpus_list[[i]], forward, backward, api_key=api_key)
  }
  
  corpus = corpus_list |>
    unlist() |> 
    na.omit() |> 
    c()
  
  df_freq = table(corpus) |> 
    as_tibble() |> 
    arrange(desc(n)) |>
    slice(1:ndisp) |>
    rename("lens_id" = "corpus", "Freq" = "n")
  
  includes = c("lens_id","title", "authors", "abstract", "external_ids", "scholarly_citations_count", "source", "year_published")
  
  complete_articles = df_freq$lens_id |>
    as.character() |> 
    request_lens_df(includes) |>
    as_tibble() |>
    mutate(doi = sapply(external_ids, get_id, "doi")) |> 
    mutate(journal = source$title) |>
    mutate(pmid = sapply(external_ids, get_id, "pmid")) |>
    left_join(df_freq, by="lens_id") |> 
    filter(!doi %in% unwanted_list & !pmid %in% unwanted_list) |>
    arrange(-Freq) |> 
    pubmed_complete() |> 
    select(authors,year_published, journal, title, abstract, scholarly_citations_count, Freq,doi, pmid)
  if (length(complete_articles$title)<ndisp){ndisp<-length(complete_articles$title)}
  for (i in 1:ndisp){
    complete_articles$authors[i]<-paste0(complete_articles$authors[[i]][[3]][[1]], ". ",complete_articles$authors[[i]][[2]][[1]])

  }
  
  
  return(complete_articles)
}


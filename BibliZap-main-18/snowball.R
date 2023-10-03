library(httr)
library(tidyr)
library(dplyr)
library(maditr)
library(tibble)
setwd("~/Downloads/BibliZap-main-18")
source("lens.R")

citation_network <- function(id_list, id_review="blank", date_review, api_key='TdUUUOLUWn9HpA7zkZnu01NDYO1gVdVz71cDjFRQPeVDCrYGKWoY') {
  std_date<-as.Date("2023-10-10")

  id_list<-na.omit(id_list)
  if(length(id_list) == 0 || is.na(id_list[1])) {
    return(NA)
  }
  taille_corpus<<-1
  print(sprintf('Fetching references and citations for %s articles', length(id_list)))
  if (length(id_list)>1){
    taille_corpus <<-length(id_list)
    std_date<-as.Date(date_review)
  }

  dd = request_lens_df(id_list,
                       includes = c("lens_id", "references", "scholarly_citations", "external_ids", "date_published"),
                       api_key=api_key)
  cit_by <- unlist(dd$scholarly_citations[as.Date(dd$date_published)<std_date|is.na(dd$date_published)])
  cit_by <- cit_by[cit_by != id_review]
  references <- unlist(dd$references[as.Date(dd$date_published)<std_date|is.na(dd$date_published)])
  references <- references[references != id_review]
  corpus = c(cit_by,references)

  
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

snowball_bibliography <- function(id_list, id_review="blank", date_review, ndisp=50, depth=2, api_key='TdUUUOLUWn9HpA7zkZnu01NDYO1gVdVz71cDjFRQPeVDCrYGKWoY') {
  id_list<- gsub(" ", "", id_list)
  naugm<-4*ndisp
  corpus_list = list()
  corpus_list[[1]] = id_list
  for(i in 1:depth) {
    print(sprintf('Depth %d', i))
    corpus_list[[i+1]] = citation_network(corpus_list[[i]], id_review,date_review,  api_key=api_key)
  }
  
  corpus = corpus_list |>
    unlist() |> 
    na.omit() |> 
    c()
  
  df_freq = table(corpus) |> 
    as_tibble() |> 
    arrange(desc(n)) |>
    slice(1:naugm) |>
    rename("lens_id" = "corpus", "Freq" = "n")
  includes = c("lens_id","title", "authors", "abstract", "external_ids", "scholarly_citations", "references", "source", "date_published")
  complete_articles = df_freq$lens_id |>
    as.character() |> 
    request_lens_df(includes) |>
    as_tibble() |>
    mutate(doi = sapply(external_ids, get_id, "doi")) |> 
    mutate(journal = source$title) |>
    mutate(pmid = sapply(external_ids, get_id, "pmid")) |>
    left_join(df_freq, by="lens_id") |> 
    filter(date_published < date_review|is.na(date_published))|>
    arrange(-Freq) |> 
    pubmed_complete() |> 
    slice(1:ndisp) |>
    select(date_published, journal, title, abstract, lens_id, pmid, Freq)
  


  return(complete_articles)
}


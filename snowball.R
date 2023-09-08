#v2.0 
#Now relying on Lens.org

library(httr)
library(tidyr)
library(dplyr)
library(maditr)
library(tibble)

source("lens.R")

citation_network <- function(article_list, api_key='TdUUUOLUWn9HpA7zkZnu01NDYO1gVdVz71cDjFRQPeVDCrYGKWoY') {
  if(length(article_list) == 0 || is.na(article_list[1])) {
    return(NA)
  }
  dd = request_lens_df(article_list, api_key=api_key)
  
  cit_by <- unlist(dd[["scholarly_citations"]])
  references <- unlist(dd[["references"]])
  corpus = c(cit_by,references)
  print(length(corpus))
  if(is.null(corpus)) {
    return(NA)
  }
  return(corpus)
}


SnowBall <- function(article_list, ndisp=50, depth=2, api_key='TdUUUOLUWn9HpA7zkZnu01NDYO1gVdVz71cDjFRQPeVDCrYGKWoY') {
  corpus_list = list()
  corpus_list[[1]] = article_list

  for(i in 1:depth) {
    print(i)
    corpus_list[[i+1]] = citation_network(corpus_list[[i]], api_key=api_key)
  }
  
  corpus = corpus_list |>
    unlist() |> 
    na.omit() |> 
    c()
  
  df_freq = table(corpus) |> 
    as_tibble() |> 
    arrange(desc(n)) |>
    filter(row_number() <= ndisp) |> 
    rename("lens_id" = "corpus", "Freq" = "n")
  
  includes = c("lens_id","title", "authors", "abstract", "external_ids", "scholarly_citations", "references")

  complete_articles = df_freq$lens_id |>
    as.character() |> 
    request_lens_df(includes) |>
    as_tibble() |>
    mutate(doi = sapply(external_ids, get_id, "doi")) |> 
    select(lens_id, title, abstract) |>
    left_join(df_freq, by="lens_id") |> 
    arrange(-Freq)

  return(complete_articles)
}


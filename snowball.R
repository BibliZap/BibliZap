require(tidyverse)

get_references_onestep = function(src_pmid, asc=F) {
  MAX_URL_SIZE_REFNUMBER=325
  splitter = function(vec, chunksize) {
    split(vec, ceiling(seq_along(vec)/chunksize))
  }
  
  get_references_onestep_unsafe = function(src_pmid, asc) {
    url = ifelse(asc,
                 "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin&id=",
                 "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_refs&id=")
    url |>
      paste(paste(src_pmid, collapse = "&id="), sep="") |> 
      xml2::read_html() |> 
      xml2::xml_find_all(".//id") |> 
      xml2::xml_text()
  }
  
  onestep = src_pmid |>
    splitter(MAX_URL_SIZE_REFNUMBER) |>
    lapply(get_references_onestep_unsafe, asc) |> 
    unlist(recursive = T) |> 
    unname()
  
  return(tail(onestep, n = length(onestep) - length(src_pmid)))
}

# Recursive function that gets pmid references for a source article
get_references = function(src_pmid, depth=1, asc=T, desc=T) {
  if(depth<=0 || (asc==F && desc==F)) {
    return(src_pmid)
  } else {
    ref_asc = c()
    ref_desc = c()
    if(asc) {
      ref_asc = get_references(src_pmid, depth-1, asc, desc) |> get_references_onestep(asc=T)
    }
    if(desc) {
      ref_desc = get_references(src_pmid, depth-1, asc, desc) |> get_references_onestep(asc=F)
    }
    
    return(c(ref_asc, ref_desc))
  }
}

get_authors = function(features) {
  # Extraction de AU
  # Diviser le texte en au_blocs correspondant à chaque article
  au_blocs <- strsplit(features, "PMID- ")[[1]]
  au_blocs <- au_blocs[-1]  # Supprimer le premier élément vide
  
  # Initialiser un vecteur pour stocker les noms d'auteurs
  auteurs <- character(length(au_blocs))
  
  # Parcourir chaque bloc et extraire le premier nom d'auteur
  for (i in 1:length(au_blocs)) {
    bloc <- au_blocs[i]
    match_start <- regexpr("AU  - (.+)", bloc, perl = TRUE)
    if (match_start != -1) {
      match <- regmatches(bloc, match_start)[[1]]
      auteurs[i] <- match
    } else {
      auteurs[i] <- ""
    }
  }
  
  auteurs
}

SnowBallBibliography <- function(src_pmid, depth=2) {
  pmid_score = tibble(PMID = get_references(src_pmid, depth=depth)) |> 
    group_by(PMID) |>
    summarise(Score=n()) |> # Establish score based for each article based on the number of occurrences
    arrange(-Score) |>
    head(200) # Limit the articles to the 200 best according to score
  
  ################################## Joindre revue et année

  #lire le html
  features = paste("https://pubmed.ncbi.nlm.nih.gov/?term=", paste(pmid_score$PMID, collapse = "+"), "&show_snippets=off&format=pubmed&size=200", sep="") |> 
    rvest::read_html() |> 
    as.character()
  
  df_features = tibble(PMID = features |> # re-extraction of PMID, as a safety measure if order or count has changed
               str_extract_all("(?<=PMID- ).+") |>
               unlist(),
             Year = features |> 
               str_extract_all("(?<=DP  - )\\d{4}") |> 
               unlist(),
             Journal = features |> 
               str_extract_all("(?<=JT  - ).+") |> 
               unlist(),
             Title = features |> 
               str_extract_all("(?<=TI  - )([\\s\\S]*?)(?=[A-Z]{2,}\\s{1,}-)") |> 
               unlist() |> 
               str_replace("\r\n", " ") |> 
               str_replace(" +", " "),
             Abstract = strsplit(features, "PMID- ")[[1]][-1] |>
               str_extract("(?<=AB  - )([\\s\\S]*?)(?=[A-Z]{2,}\\s{1,}-)"),
             `First author` = get_authors(features) |> 
               str_remove_all("^[A-Z]+\\s*-\\s+") |> 
               str_remove_all("\\r") |> 
               str_replace("\\s+", " ") |> 
               trimws(which="left"),
             )
  
  #FINAL
  pmid_score |> 
    left_join(df_features, by="PMID") |> 
    arrange(-Score) 
}

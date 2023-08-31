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



# Source the file containing the SnowBallFunction
SnowBall <- function(PMID_origine) {
  PMID_origine<-as.numeric(PMID_origin)
  total = tibble(PMID = get_references(PMID_origine, depth=2)) |> 
    group_by(PMID) |>
    summarise(Score=n()) |>
    arrange(-Score) |> 
    mutate(Score = Score |> as.character())
  
  ################################## Joindre revue et année
  
  # Vérifier le nombre de lignes dans total$PMID
  nb_lignes <- nrow(total)
  nb_lignes_max <- 200
  
  # Déterminer le nombre de lignes à extraire
  nb_lignes_extraites <- min(nb_lignes, nb_lignes_max)
  
  # Extraire les lignes de total$PMID
  subset_PMID <- head(total$PMID, nb_lignes_extraites)
  
  # Concaténer les valeurs avec le séparateur "+"
  liste_PMID <- paste(subset_PMID, collapse = "+")
  
  #lire le html
  html_features <- paste("https://pubmed.ncbi.nlm.nih.gov/?term=", liste_PMID, "&show_snippets=off&format=pubmed&size=200", sep="")
  features <- rvest::read_html(html_features)
  features <- as.character(features)
  
  # Extraction du PMID
  pmid_features <- str_extract_all(features, "(?<=PMID- ).+")
  pmid_features <- unlist(pmid_features)
  pmid_features <- data.frame(pmid_features)
  colnames(pmid_features) <- "PMID"
  # Extraction de DP
  dp_features <- str_extract_all(features, "(?<=DP  - )\\d{4}")
  dp_features <- unlist(dp_features)
  dp_features <- data.frame(dp_features)
  colnames(dp_features) <- "Year"
  # Extraction de JT
  jt_features <- str_extract_all(features, "(?<=JT  - ).+")
  jt_features <- unlist(jt_features)
  jt_features <- data.frame(jt_features)
  colnames(jt_features) <- "Journal"
  # Extraction de TI
  ti_features <- str_extract_all(features, "(?<=TI  - )([\\s\\S]*?)(?=[A-Z]{2,}\\s{1,}-)")
  ti_features <- unlist(ti_features)
  ti_features <- gsub("\r\n", " ", ti_features)
  ti_features <- gsub(" +", " ", ti_features)
  ti_features <- data.frame(ti_features)
  colnames(ti_features) <- "Title"
  
  # Extraction de AB
  # Diviser le texte en ab_blocs correspondant à chaque article
  ab_blocs <- strsplit(features, "PMID- ")[[1]]
  ab_blocs <- ab_blocs[-1]  # Supprimer le premier élément vide
  
  # Extraire les PMIDs et les résumés de chaque bloc
  ab_features <- tibble(abstract = ifelse(grepl("AB  -", ab_blocs), str_extract(ab_blocs, "(?<=AB  - )([\\s\\S]*?)(?=[A-Z]{2,}\\s{1,}-)"), NA))
  
  ab_features$abstract <- gsub("^[A-Z]+\\s*:", "", ab_features$abstract)
  ab_features$abstract <- gsub("\\s+", " ", ab_features$abstract)
  ab_features$abstract <- trimws(ab_features$abstract, "left")
  colnames(ab_features) <- "Abstract"
  
  
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
  
  # Créer un tibble avec les noms d'auteurs extraits
  au_features <- tibble(auteur = auteurs)
  
  au_features$auteur <- gsub("^[A-Z]+\\s*-\\s+", "", au_features$auteur)
  au_features$auteur <- gsub("\\s+", " ", au_features$auteur)
  au_features$auteur <- trimws(au_features$auteur, "left")
  colnames(au_features) <- "First author"
  
  
  # fusion de features
  df_features <- bind_cols(pmid_features, dp_features, jt_features, ti_features, ab_features, au_features)
  
  #FINAL
  Bibliographie <- left_join(total, df_features, by="PMID")
  Bibliographie <- Bibliographie %>% 
    select("PMID", "Score", "Title", "First author", "Year", "Journal", "Abstract") %>%
    slice(1:nb_lignes_extraites)
  
  # Identifier l'index de la ligne correspondant à PMID_origine
  index_ligne <- which(Bibliographie$PMID == PMID_origine)
  
  # Mettre à jour la valeur dans la colonne Score pour l'index identifié
  if (length(index_ligne) > 0) {
    Bibliographie[index_ligne, "Score"] <- "Source Article"
    
  }
  
  Bibliographie <- Bibliographie[order(Bibliographie$Score, decreasing = TRUE),]
  
  Bibliographie$Score <- as.character(Bibliographie$Score)
  
  Bibliographie$PMID <- sprintf(
    '<a href="https://pubmed.ncbi.nlm.nih.gov/%s/" target="_blank">%s</a>',
    Bibliographie$PMID, Bibliographie$PMID
  )
  
  DT::datatable(Bibliographie, escape = FALSE)
}

library(shiny)
library(dplyr)
library(shinycssloaders)
library(shinyjs)
library(DT) 
library(openxlsx)
library(tidyverse)
library(stringr)
library(rvest)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(tidyr)
library(quanteda)
library(quanteda.textstats)
library(htmltools)
library(lubridate)

# Source the file containing the SnowBallFunction
SnowBall <- function(PMID_origine) {
  
  
  #descendant sur PMID origine (les articles présents dans les références)
  PMID_origine<-extract_numeric(PMID_origine)
  html_ref <- paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_refs&id=", PMID_origine, sep="")
  ref <- read_html(html_ref)
  ref <- as.character(ref)
  pmid_ref <- str_extract_all(ref, "(?<=<id>)[^<]+(?=</id>)", simplify = TRUE)
  pmid_ref <- t(pmid_ref)
  pmid_ref <- data.frame(pmid_ref)
  
  #ascendant sur pmid_cite_ref
  asc_ref_concatenated <- paste(pmid_ref$pmid_ref, collapse = "&id=")
  html_asc_ref <- paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin&id=", asc_ref_concatenated, sep="")
  asc_ref <- read_html(html_asc_ref)
  asc_ref <- as.character(asc_ref)
  pmid_asc_ref <- str_extract_all(asc_ref, "(?<=<id>)[^<]+(?=</id>)", simplify = TRUE)
  pmid_asc_ref <- t(pmid_asc_ref)
  pmid_asc_ref <- data.frame(pmid_asc_ref)
  
  #descendant sur pmid_cite_ref
  desc_ref_concatenated <- paste(pmid_ref$pmid_ref, collapse = "&id=")
  html_desc_ref <- paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_refs&id=", desc_ref_concatenated, sep="")
  desc_ref <- read_html(html_desc_ref)
  desc_ref <- as.character(desc_ref)
  pmid_desc_ref <- str_extract_all(desc_ref, "(?<=<id>)[^<]+(?=</id>)", simplify = TRUE)
  pmid_desc_ref <- t(pmid_desc_ref)
  pmid_desc_ref <- data.frame(pmid_desc_ref)
  
  #############################################################
  
  #ascendant sur PMID origine (les articles qui citent cet article)
  html_citedby <- paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin&id=", PMID_origine, sep="")
  citedby <- read_html(html_citedby)
  citedby <- as.character(citedby)
  pmid_citedby <- str_extract_all(citedby, "(?<=<id>)[^<]+(?=</id>)", simplify = TRUE)
  pmid_citedby <- t(pmid_citedby)
  pmid_citedby <- data.frame(pmid_citedby)
  
  #ascendant sur pmid_cited_by
  asc_citedby_concatenated <- paste(pmid_citedby$pmid_citedby, collapse = "&id=")
  
  # Vérifier la longueur de asc_citedby_concatenated
  if (length(pmid_citedby[,1]) > 300) {
    # Découper en morceaux de taille maximale de 300
    citedby_chunks <- split(pmid_citedby$pmid_citedby, ceiling(seq_along(pmid_citedby$pmid_citedby)/300))
    pmid_asc_citedby <- character(0) # Initialiser le vecteur pour stocker les résultats
    
    # Boucle pour chaque morceau
    for (chunk in citedby_chunks) {
      chunk_concatenated <- paste(chunk, collapse = "&id=")
      html_asc_citedby <- paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin&id=", chunk_concatenated, sep="")
      asc_citedby <- read_html(html_asc_citedby)
      asc_citedby <- as.character(asc_citedby)
      pmid_asc_citedby_chunk <- str_extract_all(asc_citedby, "(?<=<id>)[^<]+(?=</id>)", simplify = TRUE)
      pmid_asc_citedby_chunk <- t(pmid_asc_citedby_chunk)
      pmid_asc_citedby_chunk <- data.frame(pmid_asc_citedby_chunk)
      pmid_asc_citedby <- rbind(pmid_asc_citedby, pmid_asc_citedby_chunk)
    }
  } else {
    html_asc_citedby <- paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin&id=", asc_citedby_concatenated, sep="")
    asc_citedby <- read_html(html_asc_citedby)
    asc_citedby <- as.character(asc_citedby)
    pmid_asc_citedby <- str_extract_all(asc_citedby, "(?<=<id>)[^<]+(?=</id>)", simplify = TRUE)
    pmid_asc_citedby <- t(pmid_asc_citedby)
    pmid_asc_citedby <- data.frame(pmid_asc_citedby)
  }
  
  
  #descendant sur pmid_cited_by
  desc_citedby_concatenated <- paste(pmid_citedby$pmid_citedby, collapse = "&id=")
  
  # Vérifier la longueur de desc_citedby_concatenated
  if (length(pmid_citedby[,1]) > 300) {
    # Découper en morceaux de taille maximale de 300
    citedby_chunks <- split(pmid_citedby$pmid_citedby, ceiling(seq_along(pmid_citedby$pmid_citedby)/300))
    pmid_desc_citedby <- character(0) # Initialiser le vecteur pour stocker les résultats
    
    # Boucle pour chaque morceau
    for (chunk in citedby_chunks) {
      chunk_concatenated <- paste(chunk, collapse = "&id=")
      html_desc_citedby <- paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin&id=", chunk_concatenated, sep="")
      desc_citedby <- read_html(html_desc_citedby)
      desc_citedby <- as.character(desc_citedby)
      pmid_desc_citedby_chunk <- str_extract_all(desc_citedby, "(?<=<id>)[^<]+(?=</id>)", simplify = TRUE)
      pmid_desc_citedby_chunk <- t(pmid_desc_citedby_chunk)
      pmid_desc_citedby_chunk <- data.frame(pmid_desc_citedby_chunk)
      pmid_desc_citedby <- rbind(pmid_desc_citedby, pmid_desc_citedby_chunk)
    }
  } else {
    html_desc_citedby <- paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin&id=", desc_citedby_concatenated, sep="")
    desc_citedby <- read_html(html_desc_citedby)
    desc_citedby <- as.character(desc_citedby)
    pmid_desc_citedby <- str_extract_all(desc_citedby, "(?<=<id>)[^<]+(?=</id>)", simplify = TRUE)
    pmid_desc_citedby <- t(pmid_desc_citedby)
    pmid_desc_citedby <- data.frame(pmid_desc_citedby)
  }
  
  
  #############################################################
  
  #on fusionne tous les data frame
  
  resume_pmid_ref <- data.frame(table(unlist(pmid_ref)))
  resume_pmid_desc_ref <- data.frame(table(unlist(pmid_desc_ref)))
  resume_pmid_asc_ref <- data.frame(table(unlist(pmid_asc_ref)))
  
  
  resume_pmid_citedby <- data.frame(table(unlist(pmid_citedby)))
  resume_pmid_desc_citedby <- data.frame(table(unlist(pmid_desc_citedby)))
  resume_pmid_asc_citedby <- data.frame(table(unlist(pmid_asc_citedby)))
  
  colnames(resume_pmid_ref)[c(1,2)] <- c("PMID", "freq_pmid_ref")
  colnames(resume_pmid_desc_ref)[c(1,2)] <- c("PMID", "freq_pmid_desc_ref")
  colnames(resume_pmid_asc_ref)[c(1,2)] <- c("PMID", "freq_pmid_asc_ref")
  colnames(resume_pmid_citedby)[c(1,2)] <- c("PMID", "freq_pmid_citedby")
  colnames(resume_pmid_desc_citedby)[c(1,2)] <- c("PMID", "freq_pmid_desc_citedby")
  colnames(resume_pmid_asc_citedby)[c(1,2)] <- c("PMID", "freq_pmid_asc_citedby")
  
  total <- full_join(resume_pmid_ref, resume_pmid_desc_ref, by="PMID")
  total <- full_join(total, resume_pmid_asc_ref, by="PMID")
  total <- full_join(total, resume_pmid_citedby, by="PMID")
  total <- full_join(total, resume_pmid_asc_citedby, by="PMID")
  total <- full_join(total, resume_pmid_desc_citedby, by="PMID")
  
  total <- replace(total,is.na(total),0)
  
  total$Score <- total$"freq_pmid_ref"+total$"freq_pmid_desc_ref"+total$"freq_pmid_asc_ref"+total$"freq_pmid_citedby"+total$"freq_pmid_desc_citedby"+total$"freq_pmid_asc_citedby"
  
  total <- total[order(total$Score, decreasing = TRUE),]
  
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
  
  datatable(Bibliographie, escape = FALSE)
  
}

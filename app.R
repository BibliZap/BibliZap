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
library(quanteda)
library(quanteda.textstats)
library(htmltools)
library(TheOpenAIR)
library(lubridate)

# Source the file containing the SnowBallFunction
SnowBall <- function(PMID_origine) {
  
  
  #descendant sur PMID origine (les articles présents dans les références)
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
  colnames(dp_features) <- "Annee"
  # Extraction de JT
  jt_features <- str_extract_all(features, "(?<=JT  - ).+")
  jt_features <- unlist(jt_features)
  jt_features <- data.frame(jt_features)
  colnames(jt_features) <- "Revue"
  # Extraction de TI
  ti_features <- str_extract_all(features, "(?<=TI  - )([\\s\\S]*?)(?=[A-Z]{2,}\\s{1,}-)")
  ti_features <- unlist(ti_features)
  ti_features <- gsub("\r\n", " ", ti_features)
  ti_features <- gsub(" +", " ", ti_features)
  ti_features <- data.frame(ti_features)
  colnames(ti_features) <- "Titre"
  
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
  colnames(au_features) <- "Premier auteur"
  
  
  # fusion de features
  df_features <- bind_cols(pmid_features, dp_features, jt_features, ti_features, ab_features, au_features)
  
  #FINAL
  Bibliographie <- left_join(total, df_features, by="PMID")
  Bibliographie <- Bibliographie %>% 
    select("PMID", "Score", "Titre", "Premier auteur", "Annee", "Revue", "Abstract") %>%
    slice(1:nb_lignes_extraites)
  Bibliographie[1, "Score"] <- "Article source"
  Bibliographie$Score <- as.character(Bibliographie$Score)
  
  Bibliographie$PMID <- sprintf(
    '<a href="https://pubmed.ncbi.nlm.nih.gov/%s/" target="_blank">%s</a>',
    Bibliographie$PMID, Bibliographie$PMID
  )
  
  datatable(Bibliographie, escape = FALSE)
  
}



ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(
      HTML('
        /* ... Autres styles ... */
        /* Nouvelle classe pour centrer les éléments verticalement */
        .center-vertically {
          display: flex;
          align-items: center;
          justify-content: center;
          height: 100%;
        }
        
        /* Ajouter de lespace entre les deux div */
             .input-container {
               margin-bottom: 15px; /* Adjust the margin as needed */
                 text-align: center; /* Center the content */
             }
           
           /* Réduire la largeur du bouton et le centrer */
             .custom-button {
               width: 100%;
               display: flex;
               justify-content: center;
               margin: 0 auto;
             }
           
           /* Ajuster la taille de la police */
             .custom-label {
               font-size: 16px;
             }
           .custom-button-label {
             font-size: 16px;
           }
           
           /* Réduire la largeur du champ de saisie et le centrer */
             .custom-textinput {
               width: 60%; /* Adjust the width as needed */
                 margin: 0 auto;
             }
           /* ... Autres styles ... */
             ')
    ),
    tags$script(
      # ...
    )
  ),
  # En-tête personnalisée
  div(class = "custom-header center-vertically",
      tags$div(class = "navbar navbar-center", 
               img(src = "logo_BZ.png", height = "100px", width = "300px", class= "center-vertically"),
               tags$div(class = "container-fluid", # Use Bootstrap's container-fluid class
                        tags$div(class = "row",
                                 tags$div(class = "col-md-12", # Use Bootstrap's col-md-12 class for full width
                                          tags$div(class = "input-container",
                                                   tags$label(class = "centered-label custom-label", "Entrez votre PMID"),
                                                   tags$input(id = "PMID_origine", type = "text", class = "form-control custom-textinput")
                                          )
                                 )
                        ),
                        tags$div(class = "row", # Separate row for the button
                                 tags$div(class = "col-md-12", # Use Bootstrap's col-md-12 class for full width
                                          tags$div(class = "button-container",
                                                   actionButton("submit_button", "Fouiller les références", class = "custom-button custom-button-label")
                                          )
                                 )
                        )
               )
      )
  ),
  # Contenu de lapplication
  div(class = "custom-content",
      tabsetPanel(id = "tabs",
                  tabPanel("Bibliographie",
                           # Spinner pour indiquer le chargement
                           withSpinner(DTOutput("bibliographie_table"), type = 2, color = "black", color.background = "white"),
                           downloadButton("download_bibliographie", "Télécharger la bibliographie")
                  ),
                  tabPanel("Mots spécifiques",
                           div(class = "center-vertically",
                               actionButton("mots_specifiques_reactive", "Détecter mes mots spécifiques", class = "custom-button custom-button-label")
                           ),
                           plotOutput("mots_specifiques_plot")  # Ajout de l'élément de sortie pour le plot
                  
                           
                  )
      ,
                  tabPanel("Obtenir mon PMID",
                           # Contenu de l'onglet "Obtenir mon PMID" ici
                           p("Pour obtenir votre le PMID (PubMed identifier) de votre article, rien de plus simple. Il suffit simplement d'écrire le titre de votre article dans la barre de recherche PubMed et vous verrez dans les résultats l'inscription \"PMID: 12345678\". Celui ci est composé en général de 8 chiffres, mais il peut être plus ou moins long en fonction de l'année de publication."),
                  ),
                  tabPanel("Comment ça marche ?",
                           # Contenu de l'onglet "Comment ça marche ?" ici
                           h2("Fonctionnement général"),
                           p("BiblioZap a pour but de recenser les articles similaires à l'article source à partir des citations ascendantes et descendantes. Les citations descendantes correspondent aux références des articles (leur bibliographie) et les citations ascendantes correspondent aux articles qui citent l'article source. Voici un schéma qui résume le fonctionnement :"),
                           tags$img(src = "fonctionnement.png", width = "600px"),
                           p("A chaque étage est compé le nombre de fois que chaque PMID apparaît. A la fin du processus, la somme des occurences donne le score. Par exemple, si un article est retrouvé 1 fois dans les références de l'article source, puis est retrouvé 6 fois dans les articles qui sont cités par les articles qui sont cités par l'article sources, puis n'est plus retrouvé ailleurs il aura le score de 7."),
                           h3("Pourquoi ça ne marche qu'avec PubMed ?"),
                           p("PubMed est le moteur de recherche est la base de données de référence dans le domaine de la recherche médicale. PubMed, qui est administré par la NLM (National Library of Medicine), effectue un contrôle des journaux présents sur la plateforme. Ceux-ci sont selectionnés de manière indépendante par un collège d'expert."),
                           p("Utiliser PubMed c'est bénéficier de l'expertise de la NLM."),
                           h2("N'y-a-t-il pas un risque que ça participe au biais de citation ?"),
                           p("Oui BiblioZap peut participer au biais de citation, il est donc extremement important de toujours effectuer en parallèle une recherche d'articles par mots clés. Surtout quand vous souhaitez publier un travail."),
                  )
      )
  )
)

server <- function(input, output, session) {
  # Créer une réactive pour stocker les résultats de la recherche bibliographique
  bibliographie_reactive <- eventReactive(input$submit_button, {
    req(input$PMID_origine)
    PMID_origine <- input$PMID_origine
    
    # Call the SnowBallFunction to generate the DataFrame Bibliographie
    Bibliographie <- SnowBall(PMID_origine)
    
    
    
    # Stocker la variable Bibliographie dans l'environnement global
    assign("Bibliographie", Bibliographie, envir = .GlobalEnv)
    
    return(Bibliographie)
  })
  
  # Rendre la table "bibliographie_table" réactive à la recherche
  output$bibliographie_table <- renderDT({
    bibliographie_reactive()
  })
  
  # Gérer le téléchargement de la bibliographie au format Excel (.xlsx)
  output$download_bibliographie <- downloadHandler(
    filename = function() {
      paste("BiblioZap-", format(Sys.time(), "%Y%m%d%H%M%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      # Get the bibliographie data from the reactive
      bibliographie <- bibliographie_reactive()
      
      # Save the data to an Excel file
      write.xlsx(bibliographie, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$mots_specifiques_reactive, {
    # Get the bibliographie data from the reactive
    Bibliographie <- bibliographie_reactive()
    
    # Call the function to get specific words
    mots_specifiques_resultat <- analyse_mots_specifiques(Bibliographie)
    output$mots_specifiques_plot <- renderPlot({
      # Code pour créer le plot basé sur mots_specifiques_resultat
      plot(mots_specifiques_resultat, main = "Mots spécifiques")
    })
  })
}


shinyApp(ui, server)
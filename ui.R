source("snowball.R")

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
library(lubridate)

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
               img(src = "BibliZap.png", height = "200px", width = "600px", class= "center-vertically"),
               tags$div(class = "container-fluid", # Use Bootstrap's container-fluid class
                        tags$div(class = "row",
                                 tags$div(class = "col-md-12", # Use Bootstrap's col-md-12 class for full width
                                          tags$div(class = "input-container",
                                                   tags$label(class = "centered-label custom-label", "Enter a PMID"),
                                                   tags$input(id = "PMID_origine", type = "text", class = "form-control custom-textinput")
                                          )
                                 )
                        ),
                        tags$div(class = "row", # Separate row for the button
                                 tags$div(class = "col-md-12", # Use Bootstrap's col-md-12 class for full width
                                          tags$div(class = "button-container",
                                                   actionButton("submit_button", "Search for related articles", class = "custom-button custom-button-label")
                                          )
                                 )
                        )
               )
      )
  ),
  # Contenu de lapplication
  div(class = "custom-content",
      tabsetPanel(id = "tabs",
                  tabPanel("Bibliography",
                           # Spinner pour indiquer le chargement
                           withSpinner(DTOutput("bibliographie_table"), type = 2, color = "black", color.background = "white"),
                           downloadButton("download_bibliographie", "Download the bibliography")
                  ),
                  tabPanel("Specific words",
                           # Contenu de l'onglet "Obtenir mon PMID" ici
                           p("Available soon"),
                           
                           
                           
                  )
                  ,
                  tabPanel("How to get a PMID ?",
                           # Contenu de l'onglet "Obtenir mon PMID" ici
                           p("To get a PMID (PubMed IDentifier), just search for your article of interest in PubMed, and you will see a number in the format: \"PMID: 12345678\". It generally comprises 8 digits, but may be shorter or longer depending on the year of publishing."),
                  ),
                  tabPanel("How does it work ?",
                           # Contenu de l'onglet "Comment ça marche ?" ici
                           h2("General principle"),
                           p("BibliZap aims to catalog articles similar to the source article based on both upward and downward citations. Downward citations correspond to the references of the articles (their bibliography). Upward citations correspond to the articles citing the source article. Here is a diagram summarizing the process:"),
                           tags$img(src = "Figure1.png", width = "800px"),
                           p("At each level, the number of times each PMID appears is recorded. At the end of the process, the sum of occurrences provides the score. For instance, if an article is found once in the references of the source article, then is discovered 6 times in the articles cited by the articles that are cited by the source article, and is not found elsewhere, its score will be 7."),
                           h3("Why does it work only with PubMed?"),
                           p("PubMed relies on MedLine, a database developed by the NLM (National Library of Medicine USA). MedLine is the premier database in the field of medical research. The NLM performs quality control on the journals available on the platform. These journals are independently selected by a panel of experts."),
                           p("BibliZap relies on the expertise of the NLM."),
                           h3("Is there a risk that BibliZap contributes to the citation bias?"),
                           p("Yes, there is a potential risk of BibliZap contributing to citation bias. Therefore, it is extremely important to always conduct keyword-based article searches in parallel. This is especially crucial when you intend to publish your work."),
                  )
      )
  )
)

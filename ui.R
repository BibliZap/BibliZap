source("/home/shiny/snowball.R")

fluidPage(
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
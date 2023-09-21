require(shinycssloaders)
require(shinyjs)
require(DT) 

source("snowball.R")

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(
      HTML('
        /* Nouvelle classe pour centrer les éléments verticalement */
        .center-vertically {
          display: flex;
          align-items: center;
          justify-content: center;
          height: 100%;
        }
        /* Nouvelle classe pour centrer les éléments verticalement et horizontalement */
        .center-vertically-horiz {
          margin-top: 80px;
          display: flex;
          align-items: center;
          justify-content: center;
          height: 100%;
        }
        
        /* Nouvelle classe pour les boites depth et nb to display */
        .boite-defilement {
          margin: 0 auto;
          align-items: middle;
          text-align: left;
          width: 70%;
        }
        
        
        /* Nouvelle classe pour les textes des boites depth et nb to display */
        .txt-defilement {
          margin: 0 auto;
          padding: 0px;
          text-align: left;
          width:50%;
        }
        
        /* Ajouter de lespace entre les deux div */
        .input-container {
          margin-top: 40px; /* Add margin at the top */
          margin-bottom: 15px;
          text-align: center; /* Center the content */
        }
           
       /* Réduire la largeur du bouton et le centrer */
         .custom-button {
           width: 50%;
           display: flex;
           justify-content: center;
           margin: 0 auto;
         }
       
       /* Ajuster la taille de la police */
         .custom-label {
           font-size: 18px;
         }
       .custom-button-label {
         font-size: 18px;
       }
       
       /* Réduire la largeur du champ de saisie et le centrer */
         .custom-textinput {
           width: 45%; /* Adjust the width as needed */
             margin: 0 auto;
         }
             ')
    ),
  ),
  # En-tête personnalisée
  div(class = "custom-header center-vertically",
      tags$div(class = "navbar navbar-center", 
               img(src = "logo.png", height = "400px", width = "800px", class= "center-vertically"),
               tags$div(class = "container-fluid", # Utilisez la classe container-fluid de Bootstrap
                        tags$div(class = "row", # Row principale
                                 tags$div(class = "col-md-12", # Colonne pleine largeur
                                          tags$div(class = "input-container",
                                                   tags$label(class = "centered-label custom-label", "Enter a PMID, DOI or Lens ID"),
                                                   tags$input(id = "id_list", type = "text", class = "form-control custom-textinput")
                                          )
                                 )
                        ),
                        tags$div(class = "row", # Nouvelle ligne pour les selectInputs
                                 tags$div(class = "col-md-6", 
                                          tags$div(class = "boite-defilement",
                                                   tags$label(class = "txt-defilement", "Select depth"),
                                                   selectInput("depth_slider", "", 
                                                               choices = c("1" = 1, "2" = 2, "3" = 3),
                                                               selected = 2
                                                   )
                                          )
                                 ),
                                 tags$div(class = "col-md-6", 
                                          tags$div(class = "boite-defilement",
                                                   tags$label(class = "txt-defilement", "Articles to display"),
                                                   selectInput("ndisp_slider", "", choices = c("10" = 10, "50" = 50, "100" = 100, "200" = 200, "300" = 300, "400" = 400, "500" = 500, "600" = 600, "700" = 700, "800" = 800, "900" = 200, "1000" = 1000),
                                                               selected = 50)
                                          )
                                 )
                        ),
                        div(class = "center-vertically",
                            checkboxInput("f_box", "Forward", TRUE),
                            checkboxInput("b_box", "Backward", TRUE)
                        ),
                        tags$div(class = "row", # Nouvelle ligne pour le bouton
                                 tags$div(class = "col-md-12", # Colonne pleine largeur
                                          tags$div(class = "button-container",
                                                   actionButton("submit_button", "Search for related articles", class = "custom-button custom-button-label")
                                          )
                                 )
                        )
               )
      )
  ),
  div(class = "custom-content",
      tabsetPanel(tags$style(HTML(".nav-tabs > li > a { color: black !important; }")),
                  tabPanel("Bibliography",
                           # Spinner to indicate loading
                           withSpinner(DTOutput("bibliography_table"), type = 2, color = "black", color.background = "white"),
                           downloadButton("download_bibliography", "Download bibliography")
                  ),
                  tabPanel("Specific words",
                           fluidRow(
                             
                             div(class = "center-vertically",
                                 actionButton("mots_specifiques_reactive", "Detect specifics words (click and wait 30 sec)", class = "custom-button custom-button-label")
                                 
                             )
                             ,
                             
                             
                             div(class = "center-vertically"
                                 
                             ),DTOutput("mots_specifiques")
                             
                           )
                  ),
                  tabPanel("How does it work ?",
                           h2("General principle"),
                           p("BibliZap aims to catalog articles similar to the source article based on both upward and downward citations. Downward citations correspond to the references of the articles (their bibliography). Upward citations correspond to the articles citing the source article. Here is a diagram summarizing the process:"),
                           tags$img(src = "BibliZapFig1.svg", width = "800px"),
                           p("At each level, the number of times each PMID appears is recorded. At the end of the process, the sum of occurrences provides the score. For instance, if an article is found once in the references of the source article, then is discovered 6 times in the articles cited by the articles that are cited by the source article, and is not found elsewhere, its score will be 7."),
                           h3("Is there a risk that BibliZap contributes to the citation bias?"),
                           p("Yes, there is a potential risk of BibliZap contributing to citation bias. Therefore, it is extremely important to always conduct keyword-based article searches in parallel. This is especially crucial when you intend to publish your work.")),
                  tabPanel("Legal Information",
                           h2("Legal Information"),
                           h3("Disclaimer"),
                           p("The information provided on this website is for general informational purposes only and is extracted from the website Lens.org. While we strive to keep the information up to date and accurate, we make no representations or warranties of any kind, express or implied, about the completeness, accuracy, reliability, suitability, or availability with respect to the website or the information, products, services, or related graphics contained on the website for any purpose. Any reliance you place on such information is therefore strictly at your own risk."),
                           h3("Privacy Policy"),
                           p("No personal information is collected on this website."),
                           h3("Intellectual Property"),
                           p("All intellectual property rights in and to the content and materials on this website are owned by us or our licensors. You may not use, reproduce, distribute, or otherwise exploit any content from this website without our prior written consent."),
                           h3("Contact Information"),
                           p("If you have any questions or concerns regarding the legal information on this website, please contact us at contact@biblizap.org"),
                           h3("Web host"),
                           p("Website hosted by OVH (2 rue Kellermann – BP 80157 59053 ROUBAIX CEDEX 1)."
                           )),
                  tabPanel("Git",
                           div(class = "center-vertically-horiz",
                               a(href = "https://github.com/BibliZap",
                                 img(src = "github-mark.png", width = "50px", height = "50px"),
                                 target = "_blank")
                           )
                  )
      )
  ))




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
               img(src = "logo.png", height = "400px", width = "800px", class= "center-vertically"),
               tags$div(class = "container-fluid", # Use Bootstrap's container-fluid class
                        tags$div(class = "row",
                                 tags$div(class = "col-md-12", # Use Bootstrap's col-md-12 class for full width
                                          tags$div(class = "input-container",
                                                   tags$label(class = "centered-label custom-label", "Enter a PMID, DOI or Lens ID"),
                                                   tags$input(id = "PMID_origine", type = "text", class = "form-control custom-textinput")
                                          )
                                 )
                        ),
                        tags$div(class = "row",
                                 tags$div(class = "col-md-12", # Utilisez la classe col-md-12 de Bootstrap pour la largeur totale
                                          tags$div(class = "slider-container",
                                                   tags$label(class = "button-container", "Select Depth (1-3)"),
                                                   sliderInput("depth_slider", "Depth:", min = 1, max = 3, value = 2)
                                          )
                                 )
                        ),
                        tags$div(class = "row",
                                tags$div(class = "col-md-12", # Utilisez la classe col-md-12 de Bootstrap pour la largeur totale
                                         tags$div(class = "slider-container",
                                                  tags$label(class = "button-container", "Select number of articles to display (10-1000):"),
                                                  sliderInput("ndisp_slider", "", min = 10, max = 1000, value = 50)
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
  div(class = "custom-content",
      tabsetPanel(id = "tabs",tags$style(HTML(".nav-tabs > li > a { color: black !important; }")),
                  tabPanel("Bibliography",
                           # Spinner to indicate loading
                           withSpinner(DTOutput("bibliography_table"), type = 2, color = "black", color.background = "white"),
                           downloadButton("download_bibliography", "Download bibliography")
                  ),
                  tabPanel("Specific words",
                           p("Available soon")
                  ),
                  tabPanel("How to get a PMID ?",
                           p("To get a PMID (PubMed IDentifier), just search for your article of interest in PubMed, and you will see a number in the format: \"PMID: 12345678\". It generally comprises 8 digits, but may be shorter or longer depending on the year of publishing.")
                  ),
                  tabPanel("How does it work ?",
                           h2("General principle"),
                           p("BibliZap aims to catalog articles similar to the source article based on both upward and downward citations. Downward citations correspond to the references of the articles (their bibliography). Upward citations correspond to the articles citing the source article. Here is a diagram summarizing the process:"),
                           tags$img(src = "Figure1.png", width = "800px"),
                           p("At each level, the number of times each PMID appears is recorded. At the end of the process, the sum of occurrences provides the score. For instance, if an article is found once in the references of the source article, then is discovered 6 times in the articles cited by the articles that are cited by the source article, and is not found elsewhere, its score will be 7."),
                           h3("Why does it work only with PubMed?"),
                           p("PubMed relies on MedLine, a database developed by the NLM (National Library of Medicine USA). MedLine is the premier database in the field of medical research. The NLM performs quality control on the journals available on the platform. These journals are independently selected by a panel of experts."),
                           p("BibliZap relies on the expertise of the NLM."),
                           h3("Is there a risk that BibliZap contributes to the citation bias?"),
                           p("Yes, there is a potential risk of BibliZap contributing to citation bias. Therefore, it is extremely important to always conduct keyword-based article searches in parallel. This is especially crucial when you intend to publish your work.")
                  ),
                  tabPanel("Legal Information",
                           h2("Legal Information"),
                           h3("Disclaimer"),
                           p("The information provided on this website is for general informational purposes only and is extracted from the website PubMed (NLM USA). While we strive to keep the information up to date and accurate, we make no representations or warranties of any kind, express or implied, about the completeness, accuracy, reliability, suitability, or availability with respect to the website or the information, products, services, or related graphics contained on the website for any purpose. Any reliance you place on such information is therefore strictly at your own risk."),
                           h3("Privacy Policy"),
                           p("No personal information is collected on this website."),
                           h3("Intellectual Property"),
                           p("All intellectual property rights in and to the content and materials on this website are owned by us or our licensors. You may not use, reproduce, distribute, or otherwise exploit any content from this website without our prior written consent. Credit for the BibliZap logo depicting an avalanche on a mountain: Tommaso.sansone91, CC0, via Wikimedia Commons."),
                           h3("Contact Information"),
                           p("If you have any questions or concerns regarding the legal information on this website, please contact us at leblancvictor59@gmail.com."),
                           h3("Web host"),
                           p("Website hosted by OVH (2 rue Kellermann – BP 80157 59053 ROUBAIX CEDEX 1)."
                    )),
                    tabPanel("GitHub",
                             div(class = "center-vertically-horiz",
                                 a(href = "https://github.com/Rotrocs/BibliZap.git",
                                   img(src = "github-mark.png", width = "50px", height = "50px"),
                                   target = "_blank")
                                 )
                             )
                    )
                  ))




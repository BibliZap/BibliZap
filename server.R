server <- function(input, output, session) {
  # Créer une réactive pour stocker les résultats de la recherche bibliographique
  bibliographie_reactive <- eventReactive(input$submit_button, {
    req(input$src_pmid)
    src_pmid <- input$src_pmid
    
    # Call the SnowBallFunction to generate the DataFrame Bibliographie
    Bibliographie <- SnowBallBibliography(src_pmid) |>
      mutate(`Is source PMID` = (PMID %in% src_pmid)) |> 
      mutate(PMID = sprintf(
        '<a href="https://pubmed.ncbi.nlm.nih.gov/%s/" target="_blank">%s</a>',
        PMID, PMID)) |>
      DT::datatable(escape = FALSE,
                    options = list(
                      columnDefs = list(list(targets = 'Is source PMID', visible = FALSE)))) |> 
      formatStyle(
        'Is source PMID',
        target = 'row',
        backgroundColor = styleEqual(c(0, 1), c('#00000000', '#0055ff30'))
      )
    
    
    
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
      paste("BibliZap-", format(Sys.time(), "%Y%m%d%H%M%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      # Get the bibliographie data from the reactive
      Bibliographie_to_dl <- Bibliographie$x$data
      Bibliographie_to_dl$PMID <- gsub("<a.*?>(\\d+)</a>", "\\1", Bibliographie_to_dl$PMID)
      
      # Save the data to an Excel file
      writexl::write_xlsx(x = Bibliographie_to_dl, path = file)
    }
  )
  
  observeEvent(input$mots_specifiques_reactive, {
    # Get the bibliographie data from the reactive
    Bibliographie <- bibliographie_reactive()
    
    # Call the function to get specific words
    mots_specifiques_resultat <- analyse_mots_specifiques(Bibliographie)
    output$mots_specifiques_plot <- renderPlot({
      # Code pour créer le plot basé sur mots_specifiques_resultat
      plot(mots_specifiques_resultat, main = "Specific words")
    })
  })
}

function(input, output, session) {
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
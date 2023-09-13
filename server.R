require(dplyr)

source("snowball.R")

server <- function(input, output, session) {
  bibliography_reactive <- eventReactive(input$submit_button, {
    req(input$id_list)
    id_list <- input$id_list |>
      str_split_1(",") |>
      trimws()
    
    depth <- input$depth_slider
    ndisp <- input$ndisp_slider
    
    bibliography <- snowball_bibliography(id_list, ndisp=ndisp, depth=depth)
    
    # Store bibliography in global environment
    assign("bibliography", bibliography, envir = .GlobalEnv)
    
    return(bibliography)
  })

  observe({
    shinyjs::runjs(
      '$("#id_list").on("keydown", function(e) {
        if (e.keyCode === 13) { // 13 correspond à la touche "Entrée"
          $("#submit_button").click(); // Déclencher le clic sur le bouton de soumission
        }
      });'
    )
  })

  # Make bibliography_table reactive
  output$bibliography_table <- renderDT({
    bibliography_reactive() |> 
      mutate(Lens = sprintf('<a href="https://www.lens.org/lens/scholar/article/%s/main" target="_blank">%s</a>', lens_id, lens_id)) |> 
      mutate(PMID = sprintf('<a href="https://pubmed.ncbi.nlm.nih.gov/%s/" target="_blank">%s</a>',pmid, pmid))|>
      rename(Title = title, Abstract = abstract, Score = Freq, FirstAuthor = authors, Year = year_published, Journal = journal) |> 
      select(FirstAuthor, Year, Journal, Title, Abstract, Lens, PMID, Score) |> 
      DT::datatable(escape=F)
  })
  
  # Download bibliography as Excel (.xlsx)
  output$download_bibliography <- downloadHandler(
    filename = function() {
      paste("BibliZap-", format(Sys.time(), "%Y%m%d%H%M%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      # Save the data to an Excel file
      openxlsx::write.xlsx(bibliography, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$mots_specifiques_reactive, {
    # Get the bibliography data from the reactive
    bibliography <- bibliography_reactive()
    
    # Call the function to get specific words
    mots_specifiques_resultat <- analyse_mots_specifiques(bibliography)
    output$mots_specifiques_plot <- renderPlot({
      # Create plot for specific words
      plot(mots_specifiques_resultat, main = "Specific words")
    })
  })
}

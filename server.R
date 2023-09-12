require(dplyr)

source("snowball.R")

server <- function(input, output, session) {
  bibliography_reactive <- eventReactive(input$submit_button, {
    req(input$id_list)
    id_list <- input$id_list
    depth <- input$depth_slider
    ndisp <- input$ndisp_slider
    
    bibliography <- snowball_bibliography(id_list, ndisp=ndisp, depth=depth)
    
    # Store bibliography in global environment
    assign("bibliography", bibliography, envir = .GlobalEnv)
    
    return(bibliography)
  })

  # Make bibliography_table reactive
  output$bibliography_table <- renderDT({
    bibliography_reactive() |> 
      mutate(Lens = sprintf('<a href="https://www.lens.org/lens/scholar/article/%s/" target="_blank">%s</a>', lens_id, lens_id)) |> 
      rename(Title = title, Abstract = abstract) |> 
      select(Lens, Title, Abstract) |> 
      DT::datatable(escape=F)
  })
  
  # Download bibliography as Excel (.xlsx)
  output$download_bibliography <- downloadHandler(
    filename = function() {
      paste("BibliZap-", format(Sys.time(), "%Y%m%d%H%M%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      # Get the bibliography data from the reactive
      bibliography_to_dl <- bibliography$x$data
      bibliography_to_dl$PMID <- gsub("<a.*?>(\\d+)</a>", "\\1", bibliography_to_dl$PMID)
      
      # Save the data to an Excel file
      openxlsx::write.xlsx(bibliography_to_dl, file, row.names = FALSE)
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

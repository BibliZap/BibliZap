require(dplyr)

source("snowball.R")
source("bibliography_mining.R")

server <- function(input, output, session) {
  bibliography_reactive <- eventReactive(input$submit_button, {
    req(input$id_list)
    id_list <- input$id_list |>
      str_split_1(",") |>
      trimws()
    
    depth <- input$depth_slider
    ndisp <- input$ndisp_slider
    unwanted_id<-input$unwanted_id_list

    
    bibliography <- snowball_bibliography(id_list, ndisp=ndisp, depth=depth, unwanted_list=unwanted_id)
    
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
    dt <- bibliography_reactive() |> 
      #mutate(Lens = sprintf('<a href="https://www.lens.org/lens/scholar/article/%s/main" target="_blank">%s</a>', lens_id, lens_id)) |> 
      #mutate(PMID = sprintf('<a href="https://pubmed.ncbi.nlm.nih.gov/%s/" target="_blank">%s</a>', pmid, pmid))|>
      mutate(DOI = sprintf('<a href="https://www.doi.org/%s" target="_blank">%s</a>', doi, doi)) |>
      rename(Title = title, Abstract = abstract, Score = Freq, FirstAuthor = authors, Year = year_published, Journal = journal, Citations = scholarly_citations_count) |> 
      select(FirstAuthor, Year, Journal, Title, Abstract, DOI, Score, Citations)
    
    datatable(dt,
              escape = FALSE,
              filter = 'top',
              options = list(
                columnDefs = list(list(
                  targets = 0,
                  render = DT::JS(
                    "function(data, type, full, meta) {
                      return '<input type=\"checkbox\" class=\"row-checkbox\"/>';
                    }"
                  )
                ))
              )
    ) |> 
      DT::formatStyle(
        columns = "Abstract", 
        fontSize = "11px" 
      )
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
  observeEvent(input$bibliography_table_rows_selected, {
    pmids_selected <- bibliography$pmid[input$bibliography_table_rows_selected]
    updateTextInput(session, "selected_pmids", value = paste(pmids_selected, collapse = ", "))
  })
  
  observe({
    shinyjs::runjs(
      '
      $(document).on("change", ".row-checkbox", function() {
        var rowIndex = $(this).closest("tr").index();
        if(this.checked) {
          Shiny.setInputValue("bibliography_table_rows_selected", Shiny.inputValues.bibliography_table_rows_selected.concat([rowIndex]), {priority: "event"});
        } else {
          Shiny.setInputValue("bibliography_table_rows_selected", Shiny.inputValues.bibliography_table_rows_selected.filter(index => index !== rowIndex), {priority: "event"});
        }
      });
    '
    )
  })
  
  observeEvent(input$mots_specifiques_reactive, {
    
    # Call the function to get specific words
    output$mots_specifiques <-renderDT(bibli_mining(bibliography))
    
    
  })

        names <- c("Raphaël Bentegeac", "Bastien Le Guellec", "Victor Leblanc")
  
  # Mélanger la liste de noms de manière aléatoire
  shuffled_names <- sample(names)
  
  output$shuffled_names <- renderText({
    paste(shuffled_names, collapse = ", ")
  })
}

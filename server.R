library(shiny)
library(DT)
library(tidyverse)
library(data.table)

shinyServer(function(input, output, session) {
  values <- reactiveValues(
    dataset.original = NULL, 
    dataset.modified = NULL, 
    col.names = NULL)
  
  observeEvent(input$input.file, {
    # Pierwsze wczytanie danych
    if (!is.null(input$input.file)) {
      tryCatch({
        values$dataset.original <- read.csv(
          file = input$input.file$datapath,
          header = input$header,
          stringsAsFactors = input$stringAsFactors,
          sep = input$sep)
        
        values$dataset.modified <- data.table::copy(values$dataset.original)
        values$col.names <- colnames(values$dataset.original)
      },
      warning = function(warn){
        showNotification(paste0(warn), type = 'warning', duration = 8, closeButton = TRUE)
      },
      error = function(err){
        showNotification(paste0(err), type = 'err', duration = 8, closeButton = TRUE)
      }
      )
      
    }
  })
  
  observeEvent(input$load.again, {
    # Ponowne zaladowanie danych
    if (!is.null(input$input.file)) {
      tryCatch({
        values$dataset.original <- read.csv(
          file = input$input.file$datapath,
          header = input$header,
          stringsAsFactors = input$stringAsFactors,
          sep = input$sep
        )
        if (!is.null(values$dataset.modified))
          # Przywrocenie dataset.modified do stanu poczatkowego
          values$dataset.modified <- data.table::copy(values$dataset.original)
        
        values$col.names <- colnames(values$dataset.original)
        
      },
      warning = function(warn){
        showNotification(paste0(warn), type = 'warning', duration = 8, closeButton = TRUE)
      },
      error = function(err){
        showNotification(paste0(err), type = 'err', duration = 8, closeButton = TRUE)
      }
      )
      
      
    }
    
  })
  
  observe({
    # Aktualizuje nazwy kolumn w wejsciu
    shinyWidgets::updatePickerInput(
      session,
      inputId = 'select.cols',
      choices = values$col.names,
      selected = values$col.names
    )
  })
  
  observeEvent(input$modify.cols, {
    # Potwierdza modyfikacje kolumn
    if (!is.null(input$input.file) & !is.null(values$col.names)) {
      values$dataset.modified <- data.table::copy(values$dataset.original) # Przywraca modified -> original
      values$dataset.modified <- values$dataset.modified %>% 
        select(input$select.cols)
      
    }
    
  })
  
  output$render.table <- DT::renderDT({
    # Renderuje tabele
    if (is.null(input$input.file))
      return (NULL)
    values$dataset.modified
  })
  
  datasetDownload <- reactive(values$dataset.modified)
  
  output$downloadBtn1 <- downloadHandler(
    filename = function(){'download.csv'},
    content = function(fname) {
      write.csv(datasetDownload(), fname)
    }
  )
  
  
})
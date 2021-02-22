library(shiny)
library(DT)
library(tidyverse)
library(data.table)

shinyServer(function(input, output, session) {
  values <- reactiveValues(      # ZMIENNE
    dataset.original = NULL,     # Pierwotny wczytany dataset
    dataset.modified = NULL,     # Zmodyfikowany dataset
    col.names.original = NULL,   # Wszystkie (oryginalne) nazwy kolumn (cech)
    col.names.modified = NULL,   # Zmodyfikowane nazwy kolumn (cech)
    na.values = NULL)
  
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
        values$col.names.original <- colnames(values$dataset.original)
        values$col.names.modified <- values$col.names.original
        values$na.values <- sum(is.na(values$dataset.modified))
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
        
        values$col.names.original <- colnames(values$dataset.original)
        values$col.names.modified <- values$col.names.original
        values$na.values <- sum(is.na(values$dataset.modified))
        
        
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
    # Aktualizuje nazwy kolumn przy wyborze kolumn
    shinyWidgets::updatePickerInput(
      session,
      inputId = 'select.cols',
      choices = values$col.names.original,
      selected = values$col.names.original
    )
  })
  
  observe({
    # Aktualizuje nazwy kolumn do sortowania
    updateSelectInput(
      session,
      inputId = 'sort.cols',
      choices = values$col.names.modified
    )
  })
  
  
  observeEvent(input$select.cols.btn, {
    # Potwierdza modyfikacje kolumn
    if (!is.null(input$input.file) & !is.null(values$col.names.original)) {
      values$dataset.modified <- data.table::copy(values$dataset.original) # Przywraca modified -> original
      values$dataset.modified <- values$dataset.modified %>% 
        select(input$select.cols)
      
      values$col.names.modified <- colnames(values$dataset.modified)
      values$na.values <- sum(is.na(values$dataset.modified))
    }
  })
  
  observeEvent(input$sort.cols.btn, {
    # Potwierdza sortowanie kolumn po zmiennej
    if (!is.null(input$input.file) & !is.null(values$col.names.original)) {
      # values$dataset.modified <- data.table::copy(values$dataset.original) # Przywraca modified -> original
      if (input$is.sort.desc) {
        values$dataset.modified <- values$dataset.modified %>% 
          arrange(desc(values$dataset.modified[input$sort.cols]))
      }
      else {
        values$dataset.modified <- values$dataset.modified %>% 
          arrange(values$dataset.modified[input$sort.cols])
      }
      
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
    # Pobranie tabeli jako .csv
    filename = function(){'download.csv'},
    content = function(fname) {
      write.csv(datasetDownload(), fname)
    }
  )
  
  output$return.na.number <- renderText({
    # Zwraca liczbe obserwacji zawierajacych wartosc NULL
    as.character(values$na.values)
  })
  
})
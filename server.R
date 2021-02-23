library(shiny)
library(DT)
library(tidyverse)
library(data.table)
library(mice)

shinyServer(function(input, output, session) {
  values <- reactiveValues(        # ZMIENNE
    dataset.original = NULL,       # Pierwotny wczytany dataset
    dataset.modified = NULL,       # Zmodyfikowany dataset
    col.names.original = NULL,     # Wszystkie (oryginalne) nazwy kolumn (cech)
    col.names.modified = NULL,     # Zmodyfikowane nazwy kolumn (cech)
    col.names.modified.num = NULL, # Zmodyfikowane nazwy kolumn (tylko numeric)
    na.values = 0)
  
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
        values$col.names.modified.num <- colnames(values$dataset.modified %>% select_if(is.numeric))
        values$na.values <- sum(is.na(values$dataset.modified))
      },
      warning = function(warn){
        showNotification(paste0(warn), type = 'warning', duration = 10, closeButton = TRUE)
      },
      error = function(err){
        showNotification(paste0(err), type = 'err', duration = 10, closeButton = TRUE)
      }
      )
    }
  })
  
  updateValues <- reactive({
    return (NULL)
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
        values$col.names.modified.num <- colnames(values$dataset.modified %>% select_if(is.numeric))
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
  
  observe({
    # Aktualizuje nazwy kolumn, w ktorych mozna uzupelniac wartosci puste
    updateSelectInput(
      session,
      inputId = 'impute.col',
      choices = values$col.names.modified.num
      
    )
  })
  
  
  observeEvent(input$select.cols.btn, {
    # Potwierdza modyfikacje liczby kolumn
    if (!is.null(input$input.file) & !is.null(values$col.names.original)) {
      values$dataset.modified <- data.table::copy(values$dataset.original) # Przywraca modified -> original
      values$dataset.modified <- values$dataset.modified %>% 
        select(input$select.cols)
      
      values$col.names.modified <- colnames(values$dataset.modified)
      values$col.names.modified.num <- colnames(values$dataset.modified %>% select_if(is.numeric))
      values$na.values <- sum(is.na(values$dataset.modified))
    }
  })
  
  observeEvent(input$sort.cols.btn, {
    # Potwierdza posortowanie danych po wybranej zmiennej
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
  
  observeEvent(input$delete.nas, {
    # Potwierdza usuniecie obserwacji zawierajacych wartosc NULL
    if (!is.null(input$input.file)) {
      values$dataset.modified <- na.omit(values$dataset.modified)
      values$na.values <- sum(is.na(values$dataset.modified))
    }
  })
  
  
  observeEvent(input$apply.impute, {
    # Potwierdza uzupelnienie wartosci pustych wybranych kolumn przy uzyciu wybranej metody uzupelniania
    if (!is.null(input$input.file)) {
      tryCatch({
        if (input$impute.method == 'Srednia') {
          mean.val <- round(as.numeric(lapply(values$dataset.modified[input$impute.col], mean, na.rm = TRUE)), 2)
          values$dataset.modified[input$impute.col][is.na(values$dataset.modified[input$impute.col])] <- mean.val
        }
        
        else if (input$impute.method == 'Mediana') {
          median.val <- round(as.numeric(lapply(values$dataset.modified[input$impute.col], median, na.rm = TRUE)), 2)
          values$dataset.modified[input$impute.col][is.na(values$dataset.modified[input$impute.col])] <- median.val
        }
        
        else if (input$impute.method == 'Zero') {
          values$dataset.modified[input$impute.col][is.na(values$dataset.modified[input$impute.col])] <- 0
        }
        
        values$na.values <- sum(is.na(values$dataset.modified))
      },
      
      warning = function(warn){
        showNotification(paste0(warn), type = 'warning', duration = 10, closeButton = TRUE)
        
      },
      error = function(err){
        showNotification(paste0(err), type = 'err', duration = 10, closeButton = TRUE)
      }
      
      )
      
      
    }
  })
  
  observeEvent(input$apply.MICE, {
    # Uzupelnianie wielokrotne MICE
    numerical <- unlist(lapply(values$dataset.modified, is.numeric))
    if( ncol(values$dataset.modified[, numerical]) > 2) {
      imp <- mice::mice(values$dataset.modified[, numerical])
      values$dataset.modified[, numerical] <- mice::complete(imp)
    }
    
  })
  
  output$render.table <- DT::renderDT({
    # Renderuje tabele w zakladce 'przetwarzanie'
    if (is.null(input$input.file))
      return (NULL)
    values$dataset.modified
  })
  
  datasetDownload <- reactive(values$dataset.modified)
  output$downloadBtn1 <- downloadHandler(
    # Pobranie tabeli jako .csv
    filename = function(){'download.csv'},
    content = function(fname) {
      write.csv(datasetDownload(), fname, row.names = FALSE)
    }
  )
  
  output$return.na.number <- renderText({
    # Zwraca informacje o liczbie obserwacji zawierajacych wartosc NULL
    paste('Wartosci puste w zbiorze: ', values$na.values)
  })


  
})
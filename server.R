library(shiny)
library(DT)
library(tidyverse)
library(data.table)
library(mice)
library(lubridate)

shinyServer(function(input, output, session) {
  values <- reactiveValues(         # ZMIENNE
    dataset.original = NULL,        # Pierwotny, wczytany dataset
    dataset.modified = NULL,        # Zmodyfikowany dataset
    col.names.original = NULL,      # Wszystkie (oryginalne) nazwy kolumn (cech)
    col.names.modified = NULL,      # Zmodyfikowane nazwy kolumn (wszystkich cech)
    col.names.modified.num = NULL,  # Zmodyfikowane nazwy kolumn (tylko numeric)
    col.names.modified.fac = NULL,  # Zmodyfikowane nazwy kolumn (tylko factor)
    col.names.modified.cha = NULL,  # Zmodyfikowane nazwy kolumn (tylko character)
    col.names.modified.dat = NULL,  # Zmodyfikowane nazwy kolumn (tylko date)
    selected.num = NULL,            # Zmienna, ktora przechwytuje nazwe wybranej zmiennej typu numeric
    na.values = 0,
    min.num = 0,
    value.num = 0,
    max.num = 1,
    dataset.histogram = NULL,
    dataset.scatter = NULL,
    dataset.boxplot = NULL,
    show.menu.visualise = TRUE)
  
  # addCssClass(selector = "a[data-value='menuInfo']", class = "inactive-link")
  
  ######### PRZETWARZANIE ############
  
  observeEvent(input$input.file, {
    # Pierwsze wczytanie danych
    if (!is.null(input$input.file)) {
      tryCatch({
        values$dataset.original <- read.csv(
          file = input$input.file$datapath,
          header = input$header,
          stringsAsFactors = input$stringAsFactors,
          sep = input$sep,
          dec = input$dec)
        
        values$dataset.modified <- data.table::copy(values$dataset.original)
        values$col.names.original <- colnames(values$dataset.original)
        values$col.names.modified <- values$col.names.original
        values$col.names.modified.num <- colnames(values$dataset.modified %>% select_if(is.numeric))
        values$col.names.modified.fac <- colnames(values$dataset.modified %>% select_if(is.factor))
        values$col.names.modified.cha <- colnames(values$dataset.modified %>% select_if(is.character))
        values$col.names.modified.dat <- colnames(values$dataset.modified %>% select_if(lubridate::is.Date))
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
    # Aktualizuje wartosi zmiennych (TO DO)
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
          sep = input$sep,
          dec = input$dec
        )
        if (!is.null(values$dataset.modified))
          # Przywrocenie dataset.modified do stanu poczatkowego
          values$dataset.modified <- data.table::copy(values$dataset.original)
        
        values$col.names.original <- colnames(values$dataset.original)
        values$col.names.modified <- values$col.names.original
        values$col.names.modified.num <- colnames(values$dataset.modified %>% select_if(is.numeric))
        values$col.names.modified.fac <- colnames(values$dataset.modified %>% select_if(is.factor))
        values$col.names.modified.cha <- colnames(values$dataset.modified %>% select_if(is.character))
        values$col.names.modified.dat <- colnames(values$dataset.modified %>% select_if(lubridate::is.Date))
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
  
  observeEvent(input$approve.data, {
  # Obsluguje przycisk, ktory powoduje zatwierdzenie przetwarzanego zbioru danych (umozliwia rozpoczecie wizualizacji)
    if (!is.null(values$dataset.modified)) {
      values$show.menu.visualise <- TRUE
      
      values$dataset.histogram <- data.table::copy(values$dataset.modified)
      values$dataset.scatter <- data.table::copy(values$dataset.modified)
      values$dataset.boxplot <- data.table::copy(values$dataset.modified)
    }
    
  })
  
  output$menuVisualise <- renderMenu({
    # renderuje sidebar menu dla zakladki 'wizualizacja'
    if (values$show.menu.visualise == TRUE) {
      menuItem(
        text = 'Wizualizacja', tabName = 'menuVisualise', icon = icon('chart-area'),
        menuSubItem('Histogram', tabName = 'menuHist'),
        menuSubItem('Wykres rozrzutu', tabName = 'menuScatter'),
        menuSubItem('Wykres pudelkowy', tabName = 'menuBoxplot'))
    }
  })
  
  observe({
    # Aktualizuje liste nazw kolumn przy wyborze kolumn
    shinyWidgets::updatePickerInput(
      session,
      inputId = 'select.cols',
      choices = values$col.names.original,
      selected = values$col.names.original
    )
  })
  
  observe({
    # Aktualizuje liste nazw kolumn do sortowania
    updateSelectInput(
      session,
      inputId = 'sort.cols',
      choices = values$col.names.modified
    )
  })
  
  observe({
    # Aktualizuje liste nazw kolumn, w ktorych mozna uzupelniac wartosci puste
    updateSelectInput(
      session,
      inputId = 'impute.col',
      choices = values$col.names.modified.num
    )
  })
  
  observe({
    # Aktualizuje liste nazw kolumn do konwersji typu zmiennej
    updateSelectInput(
      session,
      inputId = 'var.to.convert',
      choices = values$col.names.modified
    )
  })
  
  observeEvent(input$apply.convert, {
    # Obsluguje i potwierdza konwersje typu zmiennej
    # input$var.to.convert
    # input$type.to.convert
    if (!is.null(input$var.to.convert)) {
      tryCatch({
        if (input$type.to.convert == 'character') {
          values$dataset.modified[input$var.to.convert] <- as.character(values$dataset.modified[input$var.to.convert])
        }
        
        if (input$type.to.convert == 'numeric') {
          values$dataset.modified[input$var.to.convert] <- as.numeric(values$dataset.modified[input$var.to.convert])
        }
        
        if (input$type.to.convert == 'factor') {
          values$dataset.modified[input$var.to.convert] <- as.factor(values$dataset.modified[input$var.to.convert])
        }
        
        # TO DO
        if (input$type.to.convert == 'date') {
          values$dataset.modified[input$var.to.convert] <- as.Date(values$dataset.modified$input$var.to.convert,
                                                                   tryFormats = c("%Y-%m-%d", "%Y/%m/%d", '%d/%m/%Y'))
        }
        
        values$col.names.modified.num <- colnames(values$dataset.modified %>% select_if(is.numeric))
        values$col.names.modified.fac <- colnames(values$dataset.modified %>% select_if(is.factor))
        values$col.names.modified.cha <- colnames(values$dataset.modified %>% select_if(is.character))
        values$col.names.modified.dat <- colnames(values$dataset.modified %>% select_if(lubridate::is.Date))
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
  
  observe({
    # Aktualizuje liste nazw kolumn typu numeric
    updateSelectInput(
      session,
      inputId = 'select.num',
      choices = values$col.names.modified.num
    )
  })
  
  observe({
    # Aktualizuje liste nazw kolumn typu date
    updateSelectInput(
      session,
      inputId = 'select.date',
      choices = values$col.names.modified.dat
    )
  })
  
  observe({
    # Aktualizuje liste nazw kolumn typu factor
    updateSelectInput(
      session,
      inputId = 'select.fac',
      choices = values$col.names.modified.fac
    )
  })
  
  observe({
    # Aktualizuje liste nazw kolumn typu character
    updateSelectInput(
      session,
      inputId = 'select.char',
      choices = values$col.names.modified.cha
    )
  })
  
  observe({
    # Aktualizuje wlasciwosci suwaka
    updateSliderInput(
      session,
      inputId = 'filter.num',
      min = values$min.num,
      max = values$max.num,
      value = c(values$min.num, values$max.num)
    )
  })
  
  observeEvent(input$select.var.to.filter.num, {
    # Potwierdza, ze wybrana zmienna ma zostac filtrem
    values$selected.num <- input$select.num
    
    values$min.num <- min(values$dataset.modified[values$selected.num])
    values$value.num <- c(
      min(values$dataset.modified[values$selected.num]),
      min(values$dataset.modified[values$selected.num])
      )
    values$max.num <- max(values$dataset.modified[values$selected.num])

  })

  observeEvent(input$apply.filter.num, {
    # Potwierdza i dokonuje filtrowanie po zmiennej typu numeric
    if (!is.null(input$input.file)) {
      values$dataset.modified <- values$dataset.modified %>% 
        filter(.data[[values$selected.num]] >= input$filter.num[1],
               .data[[values$selected.num]] <= input$filter.num[2])
      
      values$min.num <- min(values$dataset.modified[values$selected.num])
      values$value.num <- c(
        min(values$dataset.modified[values$selected.num]),
        min(values$dataset.modified[values$selected.num])
      )
      values$max.num <- max(values$dataset.modified[values$selected.num])
      
      
    }
  })
  
  observeEvent(input$select.cols.btn, {
    # Potwierdza modyfikacje liczby kolumn
    if (!is.null(input$input.file) & !is.null(values$col.names.original)) {
      values$dataset.modified <- data.table::copy(values$dataset.original) # Przywraca modified -> original
      values$dataset.modified <- values$dataset.modified %>% 
        select(input$select.cols)
      
      values$col.names.modified <- colnames(values$dataset.modified)
      values$col.names.modified.num <- colnames(values$dataset.modified %>% select_if(is.numeric))
      values$col.names.modified.fac <- colnames(values$dataset.modified %>% select_if(is.factor))
      values$col.names.modified.cha <- colnames(values$dataset.modified %>% select_if(is.character))
      values$col.names.modified.dat <- colnames(values$dataset.modified %>% select_if(lubridate::is.Date))
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
  
  ############ EKSPLORACJA #############
  
  
  output$render.table <- DT::renderDT({
    # Renderuje tabele w zakladce 'eksploracja'
    if (is.null(input$input.file))
      return (NULL)
    values$dataset.modified
  })
  
  
  ########### WIZUALIZACJA ############
  
  output$render.table.vis <- DT::renderDT({
    # Renderuje tabele w zakladce 'eksploracja'
    if (is.null(input$input.file))
      return (NULL)
    values$dataset.histogram
  })
  
  
})
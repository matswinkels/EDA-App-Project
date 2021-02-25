library(shiny)
library(shinydashboard)
library(shinyWidgets)

box1.h <- 250

shinyUI(
    shinydashboard::dashboardPage(
        skin = 'blue',
        title = 'EksploratorDanych',
        
        ########### HEADER ########### 
        
        shinydashboard::dashboardHeader(
            title = 'Eksplorator Danych',
            titleWidth = 300,
            
            dropdownMenu(
                type = 'notifications',
                headerText = 'Opis dzialania aplikacji',
                icon = icon('question-circle', 'fa-2x'),
                badgeStatus = NULL
            ),
            tags$li(
                a(
                    icon('github', 'fa-2x'),
                    href = 'https://github.com/matswinkels/EDA-App-Project',
                    target = '_blank'
                ),
                class = 'dropdown'
            )
        ),
        
        ########### SIDEBAR ########### 
        
        shinydashboard::dashboardSidebar(width = 300,
            tags$head(
                tags$link(
                    rel = 'stylesheet',
                    type = 'text/css',
                    href = 'style.css'
                )
            ),
            
            fileInput(
                inputId = 'input.file',
                label = 'Wczytaj plik .csv',
                accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"
                ),
                multiple = FALSE,
                placeholder = 'Nie wybrano pliku',
                buttonLabel = 'Przegladaj'
            ),
            
            checkboxInput(
                inputId = 'header', 
                label = 'Header', 
                value = TRUE),
            checkboxInput(
                inputId = 'stringAsFactors',
                label = 'String as factors',
                value = FALSE
            ),
            radioButtons(
                inputId = 'sep',
                label = 'Separator',
                choices = c(
                    przecinek = ',',
                    srednik = ';',
                    tab = '\t',
                    spacja = ' '
                ),
                selected = ','
            ),
            radioButtons(
                inputId = 'dec',
                label = 'Separator dziesietny',
                choices = c(
                    kropka = '.',
                    przecinek = ','
                ),
                selected = '.'
            ),
            
            actionButton('load.again', 'Zaladuj ponownie', width = 180),
            
            shinydashboard::sidebarMenu(
                menuItem(text = 'Home', tabName = 'menuHome', icon = icon('home')),
                menuItem(text = 'Przetwarzanie', tabName = 'menuPrepare', icon = icon('cog')),
                menuItem(text = 'Eksploracja', tabName = 'menuExplore', icon = icon('map')),
                menuItem(text = 'Wizualizacja', tabName = 'menuVisualise', icon = icon('chart-area')),
                menuItem(text = 'Informacje o aplikacji', tabName = 'menuInfo', icon = icon('info-circle'))
            )
        ),
        
        ########### BODY ########### 
        
        shinydashboard::dashboardBody(
            tabItems(
                
                ########### HOME ###########
                
                tabItem(
                    tabName = 'menuHome',
                    tags$h1(class = 'tabTitle', 'Home')
                ),
                
                ########### PRZETWARZANIE ###########
                
                tabItem(
                    tabName = 'menuPrepare',
                    tags$h1(class = 'tabTitle', 'Przetwarzanie  zbioru danych'),
                    
                    fluidRow(
                        column(12,
                            tabBox(width = 4, height = box1.h,
                                
                                tabPanel(
                                    title = 'Wybierz cechy',
                                    shinyWidgets::pickerInput(
                                        inputId = 'select.cols',
                                        label = NULL,
                                        choices = c(),
                                        options = list('actions-box' = TRUE),
                                        multiple = TRUE,
                                        width = '100%'
                                    ),
                                    
                                    div(style = "margin-top: 20px; text-align:center",
                                        actionButton('select.cols.btn', 'Potwierdz', width = 180)
                                    ) 
                                )
                            ),
                            
                            tabBox(width = 4, height = box1.h,
                                
                                tabPanel(
                                    title = 'Sortuj obserwacje wedlug cechy',
                                    selectInput(
                                        inputId = 'sort.cols',
                                        label = NULL,
                                        choices = c(),
                                        multiple = FALSE,
                                        width = '100%'
                                    ),
                                    column(6,
                                           div(style = "margin-top: 5px; text-align:center",
                                               actionButton('sort.cols.btn', 'Potwierdz', width = 180)
                                           ),   
                                    ),
                                    column(6,
                                           div(style = "text-align:center",
                                               checkboxInput('is.sort.desc', 'Sortuj malejaco', value = FALSE)
                                           )   
                                    )
                                )
                            ),
                            
                            tabBox(width = 4, height = box1.h,
                                
                                tabPanel(
                                    title = 'Usun wartosci puste',
                                    textOutput('return.na.number'),
                                    div(style = "margin-top: 35px; text-align:center",
                                        actionButton('delete.nas', 'Usun', width = 180)
                                    )
                                ),
                                
                                tabPanel(
                                    title = 'Uzupelnij wartosci puste',
                                    div(
                                        style = "text-align:center",
                                        selectInput('impute.col', label = 'Wybierz ceche', choices = c(), width = '100%'),
                                        selectInput('impute.method', label = 'Wybierz metode', width = '100%', choices = c(
                                            'Srednia', 'Mediana', 'Zero')),
                                        actionButton('apply.impute', 'Uzupelnij', width = 180)
                                    )
                                ),
                                
                                tabPanel(
                                    title = 'MICE',
                                    div(
                                        style = "text-align:center",
                                        p('Uzupelnianie wielokrotne MICE (Multiple Imputation by Chained Equation)'),
                                        actionButton('apply.MICE', 'Zastosuj', width = 200)
                                    )
                                )
                            ),
                        )
                    ),
                    
                    fluidRow(
                        column(12, align = 'center',
                            tabBox(width = 6, height = box1.h,
                                
                                tabPanel(
                                    title = "Konwertowanie zmiennych",
                                    selectInput(
                                        'var.to.convert',
                                        label = 'Wybierz ceche',
                                        choices = c()
                                    ),
                                    selectInput(
                                        'type.to.convert',
                                        label = 'Wybierz docelowy typ danych',
                                        choices = c('numeric', 'character', 'factor', 'date')
                                    ),
                                    actionButton('apply.convert', 'Konwertuj')
                                )
                            ),
                            
                            tabBox(width = 6, height = box1.h,
                                title = ""
                            )
                        )
                    ),
                    
                    fluidRow(
                        column(12, align = 'center',
                               tabBox(width = 6, height = box1.h * 1.5, 
                                      
                                      tabPanel(
                                          title = 'Filtrowanie zmiennych numerycznych',
                                          selectInput(
                                              'select.num',
                                              label = 'Wybierz ceche',
                                              choices = c()
                                          ),
                                          
                                          actionButton('select.var.to.filter.num', 'Zatwierdz'),
                                          
                                          sliderInput(
                                              'filter.num',
                                              min = 0,
                                              max = 0,
                                              value = c(0, 0),
                                              label = NULL,
                                              dragRange = TRUE
                                          ),
                                          
                                          actionButton('apply.filter.num', 'Filtruj')
                                      ),
                                      
                                      tabPanel(
                                          title = 'Filtrowanie dat',
                                          selectInput(
                                              'select.date',
                                              label = 'Wybierz ceche',
                                              choices = c()
                                          )
                                      )
                               ),
                               
                               tabBox(width = 6, height = box1.h * 1.5,
                                      
                                      tabPanel(
                                          title = 'Filtrowanie zmiennych kategorycznych',
                                          selectInput(
                                              'select.fac',
                                              label = 'Wybierz ceche',
                                              choices = c()
                                          )
                                          
                                      ),
                                      
                                      tabPanel(
                                          title = 'Filtrowanie zmiennych znakowych',
                                          selectInput(
                                              'select.char',
                                              label = 'Wybierz ceche',
                                              choices = c()
                                          )
                                      )  
                               )
                        )
                    )
                ),
                
                ########### EKSPLORACJA ###########
                
                tabItem(
                    tabName = 'menuExplore',
                    tags$h1(class = 'tabTitle', 'Eksploracja zbioru danych'),
                    
                    fluidRow(
                        column(12,
                               box(width = 12,
                                   DT::DTOutput('render.table'),
                                   shinyWidgets::dropdown(
                                       downloadButton(
                                           outputId = 'downloadBtn1',
                                           label = 'Pobierz .csv'
                                       ),
                                       up = TRUE,
                                       icon = icon('download')
                                   ))
                        )
                    )
                    
                ),
                
                ########### WIZUALIZACJA ###########
                
                tabItem(
                    tabName = 'menuVisualise',
                    tags$h1(class = 'tabTitle', 'Wizualizacja zbioru danych'),
                ),
                
                ########### INFO ###########
                
                tabItem(
                    tabName = 'menuInfo',
                    tags$h1(class = 'tabTitle', 'Informacje o aplikacji')
                )
            )
        )
    )
)

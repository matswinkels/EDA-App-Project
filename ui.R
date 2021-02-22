library(shiny)
library(shinydashboard)
library(shinyWidgets)

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
                icon = icon('question-circle'),
                badgeStatus = NULL
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

                tabItem(
                    tabName = 'menuHome',
                    tags$h1(class = 'tabTitle', 'Home')
                ),
                
                tabItem(
                    tabName = 'menuPrepare',
                    tags$h1(class = 'tabTitle', 'Przetwarzanie  zbioru danych'),
                    
                    fluidRow(
                        column(12,
                            tabBox(width = 4,
                                height = 150,
                                
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
                            
                            tabBox(
                                width = 4, 
                                height = 150,
                                
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
                            
                            tabBox(
                                width = 4, 
                                height = 150,
                                
                                tabPanel(
                                    title = 'Usun wartosci puste',
                                    textOutput('return.na.number')
                                ),
                                
                                tabPanel(
                                    title = 'Zamien wartosci puste'
                                )
                            )
                        )
                    ),
                    
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
                
                tabItem(
                    tabName = 'menuExplore',
                    tags$h1(class = 'tabTitle', 'Eksploracja zbioru danych'),
                    
                    fluidRow(
                        column(12,
                               box(width = 12,
                                   collapsible = TRUE
                               )
                        )
                    )
                    
                ),
                
                tabItem(
                    tabName = 'menuVisualise',
                    tags$h1(class = 'tabTitle', 'Wizualizacja zbioru danych'),
                ),
                
                tabItem(
                    tabName = 'menuInfo',
                    tags$h1(class = 'tabTitle', 'Informacje o zbiorze danych')
                )
            )
        )
    )
)

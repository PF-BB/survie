library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Analyse de données de survie",  
    # Favicon
    tags$head(tags$link(rel="shortcut icon", href="survival.ico"))
  ),

  # Sidebar
  sidebarPanel(
    fileInput('file1', 'Choisir un fichier csv',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    tags$hr(),
    uiOutput("chir"),
    uiOutput("dn"),
    uiOutput("dc"),
    uiOutput("facteur")   
  ),
  # Main panel
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("mkplot")), 
      tabPanel("Summary", verbatimTextOutput("summary"))
    )
  )
))


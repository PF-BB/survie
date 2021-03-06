library(shiny)
library(survival)

# We tweak the "am" field to have nicer factor labels. Since this doesn't
# rely on any user inputs we can do this once at startup and then use the
# value throughout the lifetime of the application
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {

  fichier <- reactive({ 
    inFile <- input$file1
    if (is.null(inFile))
      fichier <- NULL
    else
      fichier <- read.csv2(inFile$datapath, as.is=TRUE, na.strings=c("","N/C"), fileEncoding="iso-8859-1")
      #fichier <- read.csv(inFile$datapath,na.strings=c(" ","N/C"))
  })



  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  output$mkplot <- renderPlot({
    tab <- fichier()
    format_date <- "%d/%m/%y"
    date_de_fin <- as.Date( gsub("NA","",sprintf("%s%s",tab[, input$dn] , tab[, input$dc])) ,format=format_date)
    date_de_chir <- as.Date(tab[, input$chir], format=format_date)
    date_de_dn <- as.Date(tab[, input$dn], format=format_date)
    date_de_dc <- as.Date(tab[, input$dc], format=format_date)

    facteur <- factor(tab[, input$facteur])
    tobs <- as.numeric( date_de_fin - date_de_chir )
    evnt <- ((is.na(date_de_dn))) + 0

    print(facteur)
    select <- (tobs > 0) & (tobs < 20000)
    
    y <- Surv(tobs, evnt)[select]
    fac <- facteur[select]

    plot(survfit(y ~ fac,), col= 1:nlevels(fac), main=sprintf("Kaplan-Meier, %s",input$facteur))
    legend("topright",levels(fac),col=1:nlevels(fac),pch=3)

  })
  
  output$summary <- renderPrint({
    tab <- fichier()
    format_date <- "%d/%m/%y"
    date_de_fin <- as.Date( gsub("NA","",sprintf("%s%s",tab[, input$dn] , tab[, input$dc])) ,format=format_date)
    date_de_chir <- as.Date(tab[, input$chir], format=format_date)
    date_de_dn <- as.Date(tab[, input$dn], format=format_date)
    date_de_dc <- as.Date(tab[, input$dc], format=format_date)
    facteur <- tab[, input$facteur]
    tobs <- as.numeric( date_de_fin - date_de_chir )
    evnt <- ((is.na(date_de_dn))) + 0
    y <- Surv(tobs, evnt)[(tobs > 0) & (tobs < 20000)]
    fac <- facteur[(tobs > 0) & (tobs < 20000)]
    res <- survdiff(formula = y ~ fac)
    print(res)
    p.val <- 1 - pchisq(res$chisq, length(res$n) - 1)
    if (p.val < .Machine$double.eps) {
      cat("\b\b\b\b p-value:\n")
      print(integrate(function(x) dchisq(x, length(res$n) - 1),res$chisq,+Inf))
    }

  })
  
  ### Date de chirurgie
  output$chir <- renderUI({
    selectInput("chir", 
                label="Date de chirurgie :",
                c(Default="PRV_DATE_CHIR",names(fichier())) )
  })
  ### Date de dernière nouvelle
  output$dn <- renderUI({
    selectInput("dn", 
                label="Date de dernière nouvelle :",
                c(Default="DN_DATE",names(fichier())) )
  })
  ### Date de décés
  output$dc <- renderUI({
    selectInput("dc", 
                label="Date de décés :",
                c(Default="DATE_DECES",names(fichier())) )
  })
  ### Facteur d'intérêt
  output$facteur <- renderUI({
    selectInput("facteur", 
                label="Facteur d'intérêt :",
                c(Default="IDH",names(fichier())) )
  })
  
})


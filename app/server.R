library(shiny)  # Se incluye la biblioteca de Shiny
library(arules) # Se incluye la biblioteca de Reglas de Asociacion
library(arulesViz) # Se incluye la biblioteca de Reglas de Asociacion
library(stringr) # Se incluye la biblioteca para manejar cadenas de caracteres

global <- reactiveValues()
global$movies <- NULL
global$ratings <- NULL
global$rulesByName <- NULL
global$rulesByYear <- NULL

shinyServer(function(input, output, session) {

  session$onSessionEnded(function() {

      stopApp()

  })

  #Funcion que retorna los ultimos caracteres de un string
  ultimosCaracteres <- function(x, n){

    substr(x, nchar(x)-n+1, nchar(x))

  }

  observeEvent(input$sugerir, {

    if (input$selFilter == "Título") {

      subreglas = subset(global$rulesByName, subset = lhs %pin% as.character(input$nameMovie))

      if (length(subreglas) > 0) {

        output$summary <- renderPrint({
        
            inspect(head(subreglas))      

            #Para obtener el consecuente
            rule <- inspect(subreglas[1]);
            rule <- rule[3]    

        })

      } else {

        output$summary <- renderPrint({
        
          cat("No se han encontrado reglas para esta película.") 

        })
               
      }

    } else {

      desde <- as.integer(input$sel3)
      hasta <- as.integer(input$sel4)

      years <- NULL
      for (i in desde:hasta) {

        years <- c(years, i)

      }

      if (desde == 1902) {

        years <- NULL
        years <- c(years, 1902)

      }

      years <- as.character(years)

      subreglas = subset(global$rulesByYear, subset = lhs %in% c(years))

      if (length(subreglas) > 0) {

        output$summary <- renderPrint({
        
            inspect(head(subreglas))          

        })

      } else {

        output$summary <- renderPrint({

          cat("No se han encontrado reglas para este rango de años.") 

        })

      }

    }    

  })



  observeEvent(input$preProcess, {

    inFile <- input$fileMovies
    #if (is.null(inFile)) return(NULL) 

    #Se lee el dataset de peliculas
    movies_aux <- read.csv(inFile$datapath, header=TRUE, sep=",")

    #Se identifican las columnas del dataset leido
    colnames(movies_aux) <- c("ID_pelicula", "titulo_pelicula", "generos" )

    #El titulo de cada pelicula va a ser tratado como un tipo de dato String (cadena de caracteres)
    movies_aux$titulo_pelicula <- as.character(movies_aux$titulo_pelicula)

    #Eliminando espacio al final del titulo de cada pelicula
    movies_aux$titulo_pelicula <- gsub(") ", ")", movies_aux$titulo_pelicula)

    #Se asigna a un nuevo vector "annos" los ultimos 6 caracteres de todas las celdas del titulo de cada pelicula
    annos <- ultimosCaracteres(as.character(movies_aux$titulo_pelicula), 6)
    #Se elimina el ultimo caracter a cada año para eliminar el parentesis
    annos <- substring(annos, 2)
    #Se elimina el primer caracter a cada año para eliminar el parentesis
    annos <- substring(annos, 1, nchar(annos)-1)

    #Se agrega una nueva columna al dataset correspondiente al año de cada pelicula
    movies_aux[ , "anno_pelicula"] <- annos

    #Se eliminara el año del titulo de cada pelicula
    movies_aux$titulo_pelicula <- substring(movies_aux$titulo_pelicula, 1, nchar(movies_aux$titulo_pelicula)-6)

    #Los generos de cada pelicula van a ser tratados como un tipo de dato String (cadena de caracteres)
    movies_aux$generos <- as.character(movies_aux$generos)

    #Se reemplaza el separador | por una coma en los generos de cada pelicula
    movies_aux$generos <- gsub("\\|", ",", movies_aux$generos)

    #Transformacion de los generos a variables dummies
    t <- strsplit(movies_aux$generos, split = ",")
    tags <- unique(str_trim(unlist(t)))
    df2 <- as.data.frame(Reduce(cbind, lapply(tags, function(i) sapply(t, function(j) +(any(grepl(i, j), na.rm = TRUE))))))
    names(df2) <- tags

    #Añadir variables dummies al dataframe movies
    movies_aux <- cbind(movies_aux, df2)

    # Eliminar variable generos
    movies_aux$generos <- NULL

    global$movies <- movies_aux

    inFile <- input$fileRatings
    #if (is.null(inFile)) return(NULL)  

    #Se lee el dataset de ratings
    ratings_aux <- read.csv(inFile$datapath, header=TRUE, sep=",")

    #Se elimina la columna userId
    #ratings_aux$userId <- NULL
    #Se elimina la columna timestamp
    #ratings_aux$timestamp <- NULL

    #Se identifican las columnas del dataset leido
    colnames(ratings_aux) <- c("userId", "ID_pelicula", "puntuacion", "timestamp" )

    global$ratings <- ratings_aux
    session$sendCustomMessage(type = 'data', message = movies_aux$titulo_pelicula)
    rules()
    graph()

  })

  actual.value <- reactive({

    global$ratings

  })

  rules <- function() {

    #Reglas de asociacion

    # Se transforman las columnas a Factor para poder cambiar el dataframe a transactions
    userrates <- merge(global$ratings,global$movies,by="ID_pelicula")
    userrates2 = as( split(as.vector(userrates$titulo_pelicula), as.vector(userrates$userId)) , "transactions" )
    rules = apriori( userrates2 , parameter=list(supp=0.005, conf=0.5, target="rules", minlen=2, maxlen=2,maxtime=20) )

    rules = sort(rules, by ="lift")
    subreglas = subset(rules, subset = lhs %pin% as.character("Angels & Demons"))

    global$rulesByName <- rules


    userrates <- merge(global$ratings,global$movies,by="ID_pelicula")
    userrates2 = as( split(as.vector(userrates$anno_pelicula), as.vector(userrates$userId)) , "transactions" )
    rules = apriori( userrates2 , parameter=list(supp=0.005, conf=0.5, target="rules", minlen=2, maxlen=2,maxtime=20) )

    rules = sort(rules, by ="lift")
    subreglas = subset(rules, subset = lhs %pin% as.character("1980"))

    global$rulesByYear <- rules

  }
  
  graph <- function() {

    output$plot1 <- renderPlot({

      #Colocar un boxplot o inspect aqui
      plot(head(sort(global$rulesByName, by="lift"), 30), main="30 reglas más importantes.");

    })

    output$plot2 <- renderPlot({
    
      plot(head(sort(global$rulesByName, by="lift"), 30), main="30 reglas más importantes.");

    })

    output$plot3 <- renderPlot({

      #Colocar un boxplot o inspect aqui    
      plot(head(sort(global$rulesByYear, by="lift"), 30), main="30 reglas más importantes.");

    })

    output$plot4 <- renderPlot({
    
      plot(head(sort(global$rulesByName, by="lift"), 30), main="30 reglas más importantes.");

    })

  }

})
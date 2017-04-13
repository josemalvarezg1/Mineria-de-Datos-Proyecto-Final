library(shiny)  # Se incluye la biblioteca de Shiny
library(arules) # Se incluye la biblioteca de Reglas de Asociacion
library(arulesViz) # Se incluye la biblioteca de Reglas de Asociacion
library(stringr) # Se incluye la biblioteca para manejar cadenas de caracteres

# Define server logic for random distribution application
shinyServer(function(input, output) {

  #Funcion que retorna los ultimos caracteres de un string
  ultimosCaracteres <- function(x, n){

    substr(x, nchar(x)-n+1, nchar(x))

  }

  #Funcion que realiza el pre-procesamiento del dataset de movies
  movies <- function() {

    inFile <- input$fileMovies
    if (is.null(inFile)) return(NULL)    
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

    movies <- movies_aux

  }

  #Funcion que realiza el pre-procesamiento del dataset de ratings
  ratings <- function() {

    inFile <- input$fileRatings
    if (is.null(inFile)) return(NULL)  

    #Se lee el dataset de ratings
    ratings_aux <- read.csv(inFile$datapath, header=TRUE, sep=",")

    #Se elimina la columna userId
    ratings_aux$userId <- NULL
    #Se elimina la columna timestamp
    ratings_aux$timestamp <- NULL

    #Se identifican las columnas del dataset leido
    colnames(ratings_aux) <- c("ID_pelicula", "puntuacion" )

    ratings <- ratings_aux

  }

  rules <- function() {

    #Reglas de asociacion
    if (is.null(ratings())) return(NULL)  
    # Se transforman las columnas a Factor para poder cambiar el dataframe a transactions
    ratings <- ratings()
    ratings$ID_pelicula <- factor(ratings$ID_pelicula)
    ratings$puntuacion <- factor(ratings$puntuacion)

    #Transformacion de dataframe a transactions
    votacion <- as(ratings,"transactions")

    # Ejecucion de Apriori
    # 0.0001 y 0.01 son buenos valores 1693+166
    reglas <- apriori(votacion,parameter=list(minlen=2,sup = 0.0001, conf = 0.01))

    sorted <- sort(reglas, by="support")

    rules <- sorted

  }

  output$plot <- renderPlot({

    #Imprimir Reglas de asociacion
    if (is.null(rules())) return(NULL)  
    plot(rules())

  })

  output$summary <- renderPrint({
    
    #Imprimir Reglas de asociacion
    if (!is.null(rules())) {
      options(max.print=30)
      inspect(rules())
      options(max.print=999999)
    }

  })

})
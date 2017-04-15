library(shiny)  # Se incluye la biblioteca de Shiny
library(arules) # Se incluye la biblioteca de Reglas de Asociacion
library(arulesViz) # Se incluye la biblioteca de Reglas de Asociacion
library(stringr) # Se incluye la biblioteca para manejar cadenas de caracteres
library(jsonlite) # Se incluye la biblioteca para manejar JSON

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

      subreglas = subset(global$rulesByName, subset = lhs %pin% paste("^",  as.character(input$nameMovie), "$", sep=""))

      if (length(subreglas) > 0) {

        #Validar cantidad        
        
        inspect(head(subreglas))  

        movies <- global$movies

        dfRules <- as(subreglas, "data.frame")
        dfRules <- dfRules$rules
        # Para mostrar el consecuente de la primera subregla
        splitted1 <- as.list(strsplit(as.character(dfRules[1]), "=>")[[1]])
        consequent1 <- as.character(splitted1[2])
        print(consequent1)
        consequent1 <- substring(consequent1, 3)
        consequent1 <- substr(consequent1, 1, nchar(consequent1)-1) 
        consequent1 <- paste("^", consequent1, "$", sep="")
        moviesSubset1 <- movies[grep(consequent1, movies$titulo_pelicula), ]
        themoviedbId1 <- as.character(moviesSubset1[2])
        print(consequent1)
        print(themoviedbId1)


        output$summary1 <- renderPrint({
          
          consequent1 <- substring(consequent1, 2)
          consequent1 <- substr(consequent1, 1, nchar(consequent1)-1) 
          cat(consequent1)   

        })

        peliTemp1 <- fromJSON(paste("https://api.themoviedb.org/3/movie/", themoviedbId1, "?api_key=3c9a39604750b33b3e006c0d54a11e55", sep=""))
        src1 <- paste0("https://image.tmdb.org/t/p/w500",peliTemp1$poster_path)
        
        output$poster1<-renderText({paste0('<img src="',src1,'" width="100%">')})

        # Para mostrar el consecuente de la segunda subregla
        splitted2 <- as.list(strsplit(as.character(dfRules[2]), "=>")[[1]])
        consequent2 <- as.character(splitted2[2])
        consequent2 <- substring(consequent2, 3)
        consequent2 <- substr(consequent2, 1, nchar(consequent2)-1) 
        consequent2 <- paste("^", consequent2, "$", sep="")
        moviesSubset2 <- movies[grep(consequent2, movies$titulo_pelicula), ]
        themoviedbId2 <- as.character(moviesSubset2[2])
        print(consequent2)
        print(themoviedbId2)

        output$summary2 <- renderPrint({
        
          consequent2 <- substring(consequent2, 2)
          consequent2 <- substr(consequent2, 1, nchar(consequent2)-1) 
          cat(consequent2)   

        })

        peliTemp2 <- fromJSON(paste("https://api.themoviedb.org/3/movie/", themoviedbId2, "?api_key=3c9a39604750b33b3e006c0d54a11e55", sep=""))
        src2 <- paste0("https://image.tmdb.org/t/p/w500",peliTemp2$poster_path)
        
        output$poster2<-renderText({paste0('<img src="',src2,'" width="100%">')})
        
        # Para mostrar el consecuente de la tercera subregla
        splitted3 <- as.list(strsplit(as.character(dfRules[3]), "=>")[[1]])
        consequent3 <- as.character(splitted3[2])
        consequent3 <- substring(consequent3, 3)
        consequent3 <- substr(consequent3, 1, nchar(consequent3)-1) 
        consequent3 <- paste("^", consequent3, "$", sep="")
        moviesSubset3 <- movies[grep(consequent3, movies$titulo_pelicula), ]
        themoviedbId3 <- as.character(moviesSubset3[2])
        print(consequent3)
        print(themoviedbId3)

        output$summary3 <- renderPrint({
        
          consequent3 <- substring(consequent3, 2)
          consequent3 <- substr(consequent3, 1, nchar(consequent3)-1) 
          cat(consequent3)   

        })

        peliTemp3 <- fromJSON(paste("https://api.themoviedb.org/3/movie/", themoviedbId3, "?api_key=3c9a39604750b33b3e006c0d54a11e55", sep=""))
        src3 <- paste0("https://image.tmdb.org/t/p/w500",peliTemp3$poster_path)
        
        output$poster3<-renderText({paste0('<img src="',src3,'" width="100%">')})
        
        # Para mostrar el consecuente de la cuarta subregla
        splitted4 <- as.list(strsplit(as.character(dfRules[4]), "=>")[[1]])
        consequent4 <- as.character(splitted4[2])
        consequent4 <- substring(consequent4, 3)
        consequent4 <- substr(consequent4, 1, nchar(consequent4)-1) 
        consequent4 <- paste("^", consequent4, "$", sep="")
        moviesSubset4 <- movies[grep(consequent4, movies$titulo_pelicula), ]
        themoviedbId4 <- as.character(moviesSubset4[2])
        print(consequent4)
        print(themoviedbId4)

        output$summary4 <- renderPrint({
        
          consequent4 <- substring(consequent4, 2)
          consequent4 <- substr(consequent4, 1, nchar(consequent4)-1) 
          cat(consequent4)   

        })                        

        peliTemp4 <- fromJSON(paste("https://api.themoviedb.org/3/movie/", themoviedbId4, "?api_key=3c9a39604750b33b3e006c0d54a11e55", sep=""))
        src4 <- paste0("https://image.tmdb.org/t/p/w500",peliTemp4$poster_path)
        
        output$poster4<-renderText({paste0('<img src="',src4,'" width="100%">')})

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

    ######################################################################################################

    inFile <- input$fileMovies
    if (is.null(inFile)) return(NULL) 

    if (input$includeheadermovies) {
        h <- TRUE
    } else {
        h<-FALSE
    }
  
    if (input$test=="Coma") {
        separador <- ","
    } else if(input$test=="Puntoycoma") {
        separador <- ";"
    } else if(input$test=="Tabular") {
        separador <- "\t"
    }

    if(input$radioquotmovies=="doublemovies"){
        comilla <- "\""
    } else if(input$radioquotmovies=="simplemovies"){
        comilla <- "\'"
    } else if(input$radioquotmovies=="nonemovies"){
        comilla <- ""
    }

    #Se lee el dataset de peliculas
    movies_aux <- read.csv(inFile$datapath, header=h, sep=separador,quote = comilla)

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

    ######################################################################################################

    inFile <- input$fileLinks
    if (is.null(inFile)) return(NULL) 

    if (input$includeheaderlinks) {
        h <- TRUE
    } else {
        h <- FALSE
    }
   
    if (input$radioseparatorlinks=="commalinks") {
        separador <- ","
    } else if(input$radioseparatorlinks=="semicolonlinks") {
        separador <- ";"
    } else if(input$radioseparatorlinks=="tablinks") {
        separador <- "\t"
    }

    #print(separador)

    if(input$radioquotlinks=="doublelinks"){
        comilla <- "\""
    }else if(input$radioquotlinks=="simplelinks"){
        comilla <- "\'"
    }else if(input$radioquotlinks=="nonelinks"){
        comilla <- ""
    }


    #Se lee el dataset de peliculas
    links <- read.csv(inFile$datapath, header=h, sep=separador,quote = comilla)

    #Se elimina el id de imdb
    links$imdbId <- NULL

    #Se identifican las columnas del dataset leido
    colnames(links) <- c("ID_pelicula", "tmdbId")

    #Se hace el merge de links y movies segun el id de cada pelicula
    movies_aux <- merge(links,movies_aux,by="ID_pelicula")

    global$movies <- movies_aux

    ######################################################################################################

    inFile <- input$fileRatings
    if (is.null(inFile)) return(NULL)  

    if (input$includeheaderratings) {
        h <- TRUE
    } else {
        h <- FALSE
    }
   
    if (input$radioseparatorratings=="commaratings") {
        separador <- ","
    } else if(input$radioseparatorratings=="semicolonratings") {
        separador <- ";"
    } else if(input$radioseparatorratings=="tabratings") {
        separador <- "\t"
    }

    #print(separador)

    if(input$radioquotratings=="doubleratings"){
        comilla <- "\""
    }else if(input$radioquotratings=="simpleratings"){
        comilla <- "\'"
    }else if(input$radioquotratings=="noneratings"){
        comilla <- ""
    }

    #Se lee el dataset de ratings
    ratings_aux <- read.csv(inFile$datapath, header=h, sep=separador,quote = comilla)

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

    #Reglas por titulo de pelicula
    userrates <- merge(global$ratings,global$movies,by="ID_pelicula")
    userrates2 = as( split(as.vector(userrates$titulo_pelicula), as.vector(userrates$userId)) , "transactions" )
    rules = apriori( userrates2 , parameter=list(supp=0.005, conf=0.5, target="rules", minlen=2, maxlen=2,maxtime=20) )
    rules = sort(rules, by ="lift")

    global$rulesByName <- rules

    #Reglas por anno de cada pelicula
    userrates <- merge(global$ratings,global$movies,by="ID_pelicula")
    userrates2 = as( split(as.vector(userrates$anno_pelicula), as.vector(userrates$userId)) , "transactions" )
    rules = apriori( userrates2 , parameter=list(supp=0.005, conf=0.5, target="rules", minlen=2, maxlen=2,maxtime=20) )
    rules = sort(rules, by ="lift")

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
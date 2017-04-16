library(shiny)  # Se incluye la biblioteca de Shiny
library(arules) # Se incluye la biblioteca de Reglas de Asociacion
library(arulesViz) # Se incluye la biblioteca de Reglas de Asociacion
library(stringr) # Se incluye la biblioteca para manejar cadenas de caracteres
library(jsonlite) # Se incluye la biblioteca para manejar JSON

global <- reactiveValues()
global$movies <- NULL
global$ratings <- NULL
global$rulesByName <- NULL

shinyServer(function(input, output, session) {

  session$onSessionEnded(function() {

      stopApp()

  })

  #Funcion que retorna los ultimos caracteres de un string
  ultimosCaracteres <- function(x, n){

    substr(x, nchar(x)-n+1, nchar(x))

  }

  observeEvent(input$sugerir, {

    subreglas = subset(global$rulesByName, subset = lhs %pin% paste("^",  as.character(input$nameMovie), "$", sep=""))

    session$sendCustomMessage(type = 'dataSubreglas', message = as.integer(length(subreglas)))

    if (length(subreglas) > 0) {

      #Validar cantidad        

      output$summary <- renderText({          
      
        "<h3 style='padding-left:25px;'>Películas que pueden gustarte:</h3>"   

      })

      output$inspectSubRules <- renderPrint({

        options(width = 1500);
        options(max.print = 200);
        inspect(head(subreglas));
        options(max.print = 99999999);

      })

      movies <- global$movies

      dfRules <- as(subreglas, "data.frame")
      dfRules <- dfRules$rules
      # Para mostrar el consecuente de la primera subregla
      splitted1 <- as.list(strsplit(as.character(dfRules[1]), "=>")[[1]])
      consequent1 <- as.character(splitted1[2])
      consequent1 <- substring(consequent1, 3)
      consequent1 <- substr(consequent1, 1, nchar(consequent1)-1) 
      consequent1 <- paste("^", consequent1, "$", sep="")
      moviesSubset1 <- movies[grep(consequent1, movies$titulo_pelicula), ]
      themoviedbId1 <- as.character(moviesSubset1[2])

      output$summary1 <- renderText({
        
        consequent1 <- substring(consequent1, 2)
        consequent1 <- substr(consequent1, 1, nchar(consequent1)-1) 
        #cat(consequent1)   
        paste0("<h4>",consequent1,"</h4>")

      })

      peliTemp1 <- fromJSON(paste("https://api.themoviedb.org/3/movie/", themoviedbId1, "?api_key=3c9a39604750b33b3e006c0d54a11e55", sep=""))
      src1 <- paste0("https://image.tmdb.org/t/p/w500",peliTemp1$poster_path)
      
      output$poster1<-renderText({paste0('<img src="',src1,'" width="100%">')})

      if (length(subreglas) > 1) {

        # Para mostrar el consecuente de la segunda subregla
        splitted2 <- as.list(strsplit(as.character(dfRules[2]), "=>")[[1]])
        consequent2 <- as.character(splitted2[2])
        consequent2 <- substring(consequent2, 3)
        consequent2 <- substr(consequent2, 1, nchar(consequent2)-1) 
        consequent2 <- paste("^", consequent2, "$", sep="")
        moviesSubset2 <- movies[grep(consequent2, movies$titulo_pelicula), ]
        themoviedbId2 <- as.character(moviesSubset2[2])

        output$summary2 <- renderText({
      
          consequent2 <- substring(consequent2, 2)
          consequent2 <- substr(consequent2, 1, nchar(consequent2)-1) 
         # cat(consequent2)   
          paste0("<h4>",consequent2,"</h4>")

        })

        peliTemp2 <- fromJSON(paste("https://api.themoviedb.org/3/movie/", themoviedbId2, "?api_key=3c9a39604750b33b3e006c0d54a11e55", sep=""))
        src2 <- paste0("https://image.tmdb.org/t/p/w500",peliTemp2$poster_path)
        
        output$poster2<-renderText({paste0('<img src="',src2,'" width="100%">')})

      }

      if (length(subreglas) > 2) {
      
        # Para mostrar el consecuente de la tercera subregla
        splitted3 <- as.list(strsplit(as.character(dfRules[3]), "=>")[[1]])
        consequent3 <- as.character(splitted3[2])
        consequent3 <- substring(consequent3, 3)
        consequent3 <- substr(consequent3, 1, nchar(consequent3)-1) 
        consequent3 <- paste("^", consequent3, "$", sep="")
        moviesSubset3 <- movies[grep(consequent3, movies$titulo_pelicula), ]
        themoviedbId3 <- as.character(moviesSubset3[2])

        output$summary3 <- renderText({
      
          consequent3 <- substring(consequent3, 2)
          consequent3 <- substr(consequent3, 1, nchar(consequent3)-1) 
         # cat(consequent3)   
          paste0("<h4>",consequent3,"</h4>")

        })

        peliTemp3 <- fromJSON(paste("https://api.themoviedb.org/3/movie/", themoviedbId3, "?api_key=3c9a39604750b33b3e006c0d54a11e55", sep=""))
        src3 <- paste0("https://image.tmdb.org/t/p/w500",peliTemp3$poster_path)
        
        output$poster3<-renderText({paste0('<img src="',src3,'" width="100%">')})

      }

      if (length(subreglas) > 3) {
      
        # Para mostrar el consecuente de la cuarta subregla
        splitted4 <- as.list(strsplit(as.character(dfRules[4]), "=>")[[1]])
        consequent4 <- as.character(splitted4[2])
        consequent4 <- substring(consequent4, 3)
        consequent4 <- substr(consequent4, 1, nchar(consequent4)-1) 
        consequent4 <- paste("^", consequent4, "$", sep="")
        moviesSubset4 <- movies[grep(consequent4, movies$titulo_pelicula), ]
        themoviedbId4 <- as.character(moviesSubset4[2])

        output$summary4 <- renderText({
      
          consequent4 <- substring(consequent4, 2)
          consequent4 <- substr(consequent4, 1, nchar(consequent4)-1) 
          #cat(consequent4)   
          paste0("<h4>",consequent4,"</h4>")

        })                         

        peliTemp4 <- fromJSON(paste("https://api.themoviedb.org/3/movie/", themoviedbId4, "?api_key=3c9a39604750b33b3e006c0d54a11e55", sep=""))
        src4 <- paste0("https://image.tmdb.org/t/p/w500",peliTemp4$poster_path)
        
        output$poster4<-renderText({paste0('<img src="',src4,'" width="100%">')})

      }

      if (length(subreglas) > 4) {

        # Para mostrar el consecuente de la quinta subregla
        splitted5 <- as.list(strsplit(as.character(dfRules[5]), "=>")[[1]])
        consequent5 <- as.character(splitted5[2])
        consequent5 <- substring(consequent5, 3)
        consequent5 <- substr(consequent5, 1, nchar(consequent5)-1) 
        consequent5 <- paste("^", consequent5, "$", sep="")
        moviesSubset5 <- movies[grep(consequent5, movies$titulo_pelicula), ]
        themoviedbId5 <- as.character(moviesSubset5[2])

        output$summary5 <- renderText({
      
          consequent5 <- substring(consequent5, 2)
          consequent5 <- substr(consequent5, 1, nchar(consequent5)-1) 
          # cat(consequent5)   
          paste0("<h4>",consequent5,"</h4>")

        })                          

        peliTemp5 <- fromJSON(paste("https://api.themoviedb.org/3/movie/", themoviedbId5, "?api_key=3c9a39604750b33b3e006c0d54a11e55", sep=""))
        src5 <- paste0("https://image.tmdb.org/t/p/w500",peliTemp5$poster_path)
        
        output$poster5<-renderText({paste0('<img src="',src5,'" width="100%">')})

      }

      if (length(subreglas) > 5) {

        # Para mostrar el consecuente de la cuarta subregla
        splitted6 <- as.list(strsplit(as.character(dfRules[6]), "=>")[[1]])
        consequent6 <- as.character(splitted6[2])
        consequent6 <- substring(consequent6, 3)
        consequent6 <- substr(consequent6, 1, nchar(consequent6)-1) 
        consequent6 <- paste("^", consequent6, "$", sep="")
        moviesSubset6 <- movies[grep(consequent6, movies$titulo_pelicula), ]
        themoviedbId6 <- as.character(moviesSubset6[2])

        output$summary6 <- renderText({
      
          consequent6 <- substring(consequent6, 2)
          consequent6 <- substr(consequent6, 1, nchar(consequent6)-1) 
          #cat(consequent6)   
          paste0("<h4>",consequent6,"</h4>")

        })                         

        peliTemp6 <- fromJSON(paste("https://api.themoviedb.org/3/movie/", themoviedbId6, "?api_key=3c9a39604750b33b3e006c0d54a11e55", sep=""))
        src6 <- paste0("https://image.tmdb.org/t/p/w500",peliTemp6$poster_path)
        
        output$poster6<-renderText({paste0('<img src="',src6,'" width="100%">')})

      }

      if (length(subreglas) > 6) {

        # Para mostrar el consecuente de la cuarta subregla
        splitted7 <- as.list(strsplit(as.character(dfRules[7]), "=>")[[1]])
        consequent7 <- as.character(splitted7[2])
        consequent7 <- substring(consequent7, 3)
        consequent7 <- substr(consequent7, 1, nchar(consequent7)-1) 
        consequent7 <- paste("^", consequent7, "$", sep="")
        moviesSubset7 <- movies[grep(consequent7, movies$titulo_pelicula), ]
        themoviedbId7 <- as.character(moviesSubset7[2])

        output$summary7 <- renderText({
      
          consequent7 <- substring(consequent7, 2)
          consequent7 <- substr(consequent7, 1, nchar(consequent7)-1) 
          #cat(consequent7)   
          paste0("<h4>",consequent7,"</h4>")

        })                       

        peliTemp7 <- fromJSON(paste("https://api.themoviedb.org/3/movie/", themoviedbId7, "?api_key=3c9a39604750b33b3e006c0d54a11e55", sep=""))
        src7 <- paste0("https://image.tmdb.org/t/p/w500",peliTemp7$poster_path)
        
        output$poster7<-renderText({paste0('<img src="',src7,'" width="100%">')})

      }

      if (length(subreglas) > 7) {

        # Para mostrar el consecuente de la cuarta subregla
        splitted8 <- as.list(strsplit(as.character(dfRules[8]), "=>")[[1]])
        consequent8 <- as.character(splitted8[2])
        consequent8 <- substring(consequent8, 3)
        consequent8 <- substr(consequent8, 1, nchar(consequent8)-1) 
        consequent8 <- paste("^", consequent8, "$", sep="")
        moviesSubset8 <- movies[grep(consequent8, movies$titulo_pelicula), ]
        themoviedbId8 <- as.character(moviesSubset8[2])

        output$summary8 <- renderText({
      
          consequent8 <- substring(consequent8, 2)
          consequent8 <- substr(consequent8, 1, nchar(consequent8)-1) 
          #cat(consequent8)   
          paste0("<h4>",consequent8,"</h4>")

        })                          

        peliTemp8 <- fromJSON(paste("https://api.themoviedb.org/3/movie/", themoviedbId8, "?api_key=3c9a39604750b33b3e006c0d54a11e55", sep=""))
        src8 <- paste0("https://image.tmdb.org/t/p/w500",peliTemp8$poster_path)
        
        output$poster8<-renderText({paste0('<img src="',src8,'" width="100%">')})

      }

    } else {

      output$summary <- renderText({          
      
        "<h3 style='padding-left:25px;'>No se han encontrado películas para ti.</h3>"   

      })
             
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

    #Eliminando parentesis en el titulo de cada pelicula
    movies_aux$titulo_pelicula <- gsub("\\(", "", movies_aux$titulo_pelicula)
    movies_aux$titulo_pelicula <- gsub(")", "", movies_aux$titulo_pelicula)

    #Colocando annos como titulo - anno
    movies_aux$titulo_pelicula <- sapply(movies_aux$titulo_pelicula, function(string) {
      yearMovie <- NULL
      yearMovie <- ultimosCaracteres(string,4)
      string <- substr(string, 1, nchar(string)-4)
      string <- paste(string,"- ",yearMovie, sep="")
    }  ) 

    #Eliminando signos en el titulo de cada pelicula
    movies_aux$titulo_pelicula <- gsub("\\?", "", movies_aux$titulo_pelicula)
    movies_aux$titulo_pelicula <- gsub("\\!", "", movies_aux$titulo_pelicula)

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

    if(input$radioquotratings=="doubleratings"){
        comilla <- "\""
    }else if(input$radioquotratings=="simpleratings"){
        comilla <- "\'"
    }else if(input$radioquotratings=="noneratings"){
        comilla <- ""
    }

    #Se lee el dataset de ratings
    ratings_aux <- read.csv(inFile$datapath, header=h, sep=separador,quote = comilla)

    #Se elimina la columna timestamp
    ratings_aux$timestamp <- NULL

    #Se identifican las columnas del dataset leido
    colnames(ratings_aux) <- c("userId", "ID_pelicula", "puntuacion")

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

  }
  
  graph <- function() {

    output$inspectRules <- renderPrint({

      options(width = 1500);
      options(max.print = 200);
      inspect((global$rulesByName));
      options(max.print = 99999999);

    })

  }

})
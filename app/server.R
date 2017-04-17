library(shiny)  # Se incluye la biblioteca de Shiny
library(arules) # Se incluye la biblioteca de Reglas de Asociacion
library(arulesViz) # Se incluye la biblioteca de Reglas de Asociacion
library(stringr) # Se incluye la biblioteca para manejar cadenas de caracteres
library(jsonlite) # Se incluye la biblioteca para manejar JSON

global <- reactiveValues() # Se utilizara una variable global en donde se guarden:
global$movies <- NULL # Peliculas
global$ratings <- NULL # Ratings
global$rulesByName <- NULL # Reglas de asociacion

shinyServer(function(input, output, session) {

  # Funcion que detiene la ejecucion de Shiny cuando se cierra la aplicacion
  session$onSessionEnded(function() {

      stopApp()

  })

  # Funcion que retorna los ultimos caracteres de un string
  ultimosCaracteres <- function(x, n) {

    substr(x, nchar(x)-n+1, nchar(x))

  }

  # Funcion que se ejecuta cuando se presiona el boton de "Sugerir Peliculas"
  observeEvent(input$sugerir, {

    # Se encuentran todas las subreglas que contengan exactamente el titulo de la pelicula que el usuario selecciono en el menu
    subreglas <- subset(global$rulesByName, subset = lhs %pin% paste("^",  as.character(input$nameMovie), "$", sep=""))

    # Se envia un mensaje al javascript indicando el numero de subreglas encontradas
    session$sendCustomMessage(type = 'dataSubreglas', message = as.integer(length(subreglas)))

    if (length(subreglas) > 0) {

      # Se escribe un texto por pantalla
      output$summary <- renderText({          
      
        "<h3 style='padding-left:25px;'>Películas que pueden gustarte:</h3>"   

      })

      output$inspectSubRules <- renderPrint({

        # Se coloca un ancho bastante alto para que quepa toda la impresion de las subreglas en pantalla 
        options(width = 1500);

        # Se imprimen las 6 primeras subreglas
        inspect(head(subreglas));

      })

      movies <- global$movies

      # Se trabajan todas las subreglas como un dataframe (matriz)
      dfRules <- as(subreglas, "data.frame")
      dfRules <- dfRules$rules

      # Para mostrar el consecuente de la primera subregla
      splitted1 <- as.list(strsplit(as.character(dfRules[1]), "=>")[[1]])
      consequent1 <- as.character(splitted1[2])
      consequent1 <- substring(consequent1, 3)
      consequent1 <- substr(consequent1, 1, nchar(consequent1)-1) 
      consequent1 <- paste("^", consequent1, "$", sep="")

      # Se encuentra el id de la pelicula en themoviedb
      moviesSubset1 <- movies[grep(consequent1, movies$titulo_pelicula), ]
      themoviedbId1 <- as.character(moviesSubset1[2])

      # Se imprime el titulo de la pelicula en pantalla
      output$summary1 <- renderText({
        
        consequent1 <- substring(consequent1, 2)
        consequent1 <- substr(consequent1, 1, nchar(consequent1)-1) 
        paste0("<h4>",consequent1,"</h4>")

      })

      tryCatch({

          # Se busca la pelicula por id en el API de themoviedb
          peliTemp1 <- fromJSON(paste("https://api.themoviedb.org/3/movie/", themoviedbId1, "?api_key=3c9a39604750b33b3e006c0d54a11e55", sep=""))
          src1 <- paste0("https://image.tmdb.org/t/p/w500",peliTemp1$poster_path)
          
          # Se coloca la imagen obtenida en pantalla
          output$poster1<-renderText({paste0('<img src="',src1,'" width="100%">')})

        },

        # Si ocurre un error o warning al tratar de buscar la imagen, no se muestra
        error = output$poster1<-renderText({paste0('<img src="" width="100%">')}),
        warning = output$poster1<-renderText({paste0('<img src="" width="100%">')})

      )

      if (length(subreglas) > 1) {

        # Para mostrar el consecuente de la segunda subregla
        splitted2 <- as.list(strsplit(as.character(dfRules[2]), "=>")[[1]])
        consequent2 <- as.character(splitted2[2])
        consequent2 <- substring(consequent2, 3)
        consequent2 <- substr(consequent2, 1, nchar(consequent2)-1) 
        consequent2 <- paste("^", consequent2, "$", sep="")

        # Se encuentra el id de la pelicula en themoviedb
        moviesSubset2 <- movies[grep(consequent2, movies$titulo_pelicula), ]
        themoviedbId2 <- as.character(moviesSubset2[2])

        # Se imprime el titulo de la pelicula en pantalla
        output$summary2 <- renderText({
      
          consequent2 <- substring(consequent2, 2)
          consequent2 <- substr(consequent2, 1, nchar(consequent2)-1) 
          paste0("<h4>",consequent2,"</h4>")

        })

        tryCatch({

          # Se busca la pelicula por id en el API de themoviedb
          peliTemp2 <- fromJSON(paste("https://api.themoviedb.org/3/movie/", themoviedbId2, "?api_key=3c9a39604750b33b3e006c0d54a11e55", sep=""))
          src2 <- paste0("https://image.tmdb.org/t/p/w500",peliTemp2$poster_path)
          
          # Se coloca la imagen obtenida en pantalla
          output$poster2<-renderText({paste0('<img src="',src2,'" width="100%">')})

          },

          # Si ocurre un error o warning al tratar de buscar la imagen, no se muestra
          error = output$poster2<-renderText({paste0('<img src="" width="100%">')}),
          warning = output$poster2<-renderText({paste0('<img src="" width="100%">')})

        )

      }

      if (length(subreglas) > 2) {
      
        # Para mostrar el consecuente de la tercera subregla
        splitted3 <- as.list(strsplit(as.character(dfRules[3]), "=>")[[1]])
        consequent3 <- as.character(splitted3[2])
        consequent3 <- substring(consequent3, 3)
        consequent3 <- substr(consequent3, 1, nchar(consequent3)-1) 
        consequent3 <- paste("^", consequent3, "$", sep="")

        # Se encuentra el id de la pelicula en themoviedb
        moviesSubset3 <- movies[grep(consequent3, movies$titulo_pelicula), ]
        themoviedbId3 <- as.character(moviesSubset3[2])

        # Se imprime el titulo de la pelicula en pantalla
        output$summary3 <- renderText({
      
          consequent3 <- substring(consequent3, 2)
          consequent3 <- substr(consequent3, 1, nchar(consequent3)-1) 
          paste0("<h4>",consequent3,"</h4>")

        })

        tryCatch({

          # Se busca la pelicula por id en el API de themoviedb
          peliTemp3 <- fromJSON(paste("https://api.themoviedb.org/3/movie/", themoviedbId3, "?api_key=3c9a39604750b33b3e006c0d54a11e55", sep=""))
          src3 <- paste0("https://image.tmdb.org/t/p/w500",peliTemp3$poster_path)
          
          # Se coloca la imagen obtenida en pantalla
          output$poster3<-renderText({paste0('<img src="',src3,'" width="100%">')})

          },

          # Si ocurre un error o warning al tratar de buscar la imagen, no se muestra
          error = output$poster3<-renderText({paste0('<img src="" width="">')}),
          warning = output$poster3<-renderText({paste0('<img src="" width="">')})

        )

      }

      if (length(subreglas) > 3) {
      
        # Para mostrar el consecuente de la cuarta subregla
        splitted4 <- as.list(strsplit(as.character(dfRules[4]), "=>")[[1]])
        consequent4 <- as.character(splitted4[2])
        consequent4 <- substring(consequent4, 3)
        consequent4 <- substr(consequent4, 1, nchar(consequent4)-1) 
        consequent4 <- paste("^", consequent4, "$", sep="")

        # Se encuentra el id de la pelicula en themoviedb
        moviesSubset4 <- movies[grep(consequent4, movies$titulo_pelicula), ]
        themoviedbId4 <- as.character(moviesSubset4[2])

        # Se imprime el titulo de la pelicula en pantalla
        output$summary4 <- renderText({
      
          consequent4 <- substring(consequent4, 2)
          consequent4 <- substr(consequent4, 1, nchar(consequent4)-1) 
          paste0("<h4>",consequent4,"</h4>")

        })                         

        tryCatch({

          # Se busca la pelicula por id en el API de themoviedb
          peliTemp4 <- fromJSON(paste("https://api.themoviedb.org/3/movie/", themoviedbId4, "?api_key=3c9a39604750b33b3e006c0d54a11e55", sep=""))
          src4 <- paste0("https://image.tmdb.org/t/p/w500",peliTemp4$poster_path)
          
          # Se coloca la imagen obtenida en pantalla
          output$poster4<-renderText({paste0('<img src="',src4,'" width="100%">')})

        },

          # Si ocurre un error o warning al tratar de buscar la imagen, no se muestra
          error = output$poster4<-renderText({paste0('<img src="" width="">')}),
          warning = output$poster4<-renderText({paste0('<img src="" width="">')})

        )

      }

      if (length(subreglas) > 4) {

        # Para mostrar el consecuente de la quinta subregla
        splitted5 <- as.list(strsplit(as.character(dfRules[5]), "=>")[[1]])
        consequent5 <- as.character(splitted5[2])
        consequent5 <- substring(consequent5, 3)
        consequent5 <- substr(consequent5, 1, nchar(consequent5)-1) 
        consequent5 <- paste("^", consequent5, "$", sep="")

        # Se encuentra el id de la pelicula en themoviedb
        moviesSubset5 <- movies[grep(consequent5, movies$titulo_pelicula), ]
        themoviedbId5 <- as.character(moviesSubset5[2])

        # Se imprime el titulo de la pelicula en pantalla
        output$summary5 <- renderText({
      
          consequent5 <- substring(consequent5, 2)
          consequent5 <- substr(consequent5, 1, nchar(consequent5)-1) 
          paste0("<h4>",consequent5,"</h4>")

        })                          

        tryCatch({

          # Se busca la pelicula por id en el API de themoviedb
          peliTemp5 <- fromJSON(paste("https://api.themoviedb.org/3/movie/", themoviedbId5, "?api_key=3c9a39604750b33b3e006c0d54a11e55", sep=""))
          src5 <- paste0("https://image.tmdb.org/t/p/w500",peliTemp5$poster_path)
          
          # Se coloca la imagen obtenida en pantalla
          output$poster5<-renderText({paste0('<img src="',src5,'" width="100%">')})

        },

          # Si ocurre un error o warning al tratar de buscar la imagen, no se muestra
          error = output$poster5<-renderText({paste0('<img src="" width="">')}),
          warning = output$poster5<-renderText({paste0('<img src="" width="">')})

        )

      }

      if (length(subreglas) > 5) {

        # Para mostrar el consecuente de la cuarta subregla
        splitted6 <- as.list(strsplit(as.character(dfRules[6]), "=>")[[1]])
        consequent6 <- as.character(splitted6[2])
        consequent6 <- substring(consequent6, 3)
        consequent6 <- substr(consequent6, 1, nchar(consequent6)-1) 
        consequent6 <- paste("^", consequent6, "$", sep="")

        # Se encuentra el id de la pelicula en themoviedb
        moviesSubset6 <- movies[grep(consequent6, movies$titulo_pelicula), ]
        themoviedbId6 <- as.character(moviesSubset6[2])

        # Se imprime el titulo de la pelicula en pantalla
        output$summary6 <- renderText({
      
          consequent6 <- substring(consequent6, 2)
          consequent6 <- substr(consequent6, 1, nchar(consequent6)-1) 
          paste0("<h4>",consequent6,"</h4>")

        })           

        tryCatch({

          # Se busca la pelicula por id en el API de themoviedb
          peliTemp6 <- fromJSON(paste("https://api.themoviedb.org/3/movie/", themoviedbId6, "?api_key=3c9a39604750b33b3e006c0d54a11e55", sep=""))
          src6 <- paste0("https://image.tmdb.org/t/p/w500",peliTemp6$poster_path)
          
          # Se coloca la imagen obtenida en pantalla
          output$poster6<-renderText({paste0('<img src="',src6,'" width="100%">')})

        },

          # Si ocurre un error o warning al tratar de buscar la imagen, no se muestra
          error = output$poster6<-renderText({paste0('<img src="" width="">')}),
          warning = output$poster6<-renderText({paste0('<img src="" width="">')})

        )

      }

      if (length(subreglas) > 6) {

        # Para mostrar el consecuente de la cuarta subregla
        splitted7 <- as.list(strsplit(as.character(dfRules[7]), "=>")[[1]])
        consequent7 <- as.character(splitted7[2])
        consequent7 <- substring(consequent7, 3)
        consequent7 <- substr(consequent7, 1, nchar(consequent7)-1) 
        consequent7 <- paste("^", consequent7, "$", sep="")

        # Se encuentra el id de la pelicula en themoviedb
        moviesSubset7 <- movies[grep(consequent7, movies$titulo_pelicula), ]
        themoviedbId7 <- as.character(moviesSubset7[2])

        # Se imprime el titulo de la pelicula en pantalla
        output$summary7 <- renderText({
      
          consequent7 <- substring(consequent7, 2)
          consequent7 <- substr(consequent7, 1, nchar(consequent7)-1) 
          paste0("<h4>",consequent7,"</h4>")

        })       

        tryCatch({                

          # Se busca la pelicula por id en el API de themoviedb
          peliTemp7 <- fromJSON(paste("https://api.themoviedb.org/3/movie/", themoviedbId7, "?api_key=3c9a39604750b33b3e006c0d54a11e55", sep=""))
          src7 <- paste0("https://image.tmdb.org/t/p/w500",peliTemp7$poster_path)
          
          # Se coloca la imagen obtenida en pantalla
          output$poster7<-renderText({paste0('<img src="',src7,'" width="100%">')})

        },

          # Si ocurre un error o warning al tratar de buscar la imagen, no se muestra
          error = output$poster7<-renderText({paste0('<img src="" width="">')}),
          warning = output$poster7<-renderText({paste0('<img src="" width="">')})

        )

      }

      if (length(subreglas) > 7) {

        # Para mostrar el consecuente de la cuarta subregla
        splitted8 <- as.list(strsplit(as.character(dfRules[8]), "=>")[[1]])
        consequent8 <- as.character(splitted8[2])
        consequent8 <- substring(consequent8, 3)
        consequent8 <- substr(consequent8, 1, nchar(consequent8)-1) 
        consequent8 <- paste("^", consequent8, "$", sep="")

        # Se encuentra el id de la pelicula en themoviedb
        moviesSubset8 <- movies[grep(consequent8, movies$titulo_pelicula), ]
        themoviedbId8 <- as.character(moviesSubset8[2])

        # Se imprime el titulo de la pelicula en pantalla
        output$summary8 <- renderText({
      
          consequent8 <- substring(consequent8, 2)
          consequent8 <- substr(consequent8, 1, nchar(consequent8)-1) 
          paste0("<h4>",consequent8,"</h4>")

        })                          

        tryCatch({      

          # Se busca la pelicula por id en el API de themoviedb
          peliTemp8 <- fromJSON(paste("https://api.themoviedb.org/3/movie/", themoviedbId8, "?api_key=3c9a39604750b33b3e006c0d54a11e55", sep=""))
          src8 <- paste0("https://image.tmdb.org/t/p/w500",peliTemp8$poster_path)
          
          # Se coloca la imagen obtenida en pantalla
          output$poster8<-renderText({paste0('<img src="',src8,'" width="100%">')})

        },

          # Si ocurre un error o warning al tratar de buscar la imagen, no se muestra
          error = output$poster8<-renderText({paste0('<img src="" width="">')}),
          warning = output$poster8<-renderText({paste0('<img src="" width="">')})

        )

      }

    } else {

      # Se escribe un texto por pantalla
      output$summary <- renderText({          
      
        "<h3 style='padding-left:25px;'>No se han encontrado películas para ti.</h3>"   

      })
             
    }

  })

  # Funcion que se ejecuta cuando se presiona el boton de "Pre-procesar y generar reglas"
  observeEvent(input$preProcess, {

    tryCatch({

      ####################################### Pre-procesamiento de movies.csv #########################################################

      # Se obtiene el archivo 
      inFile <- input$fileMovies
      if (is.null(inFile)) return(NULL) 

      # Se valida si el archivo contiene un header
      if (input$includeheadermovies) {

          h <- TRUE

      } else {

          h<-FALSE

      }
    
      # Se valida el caracter separador del archivo
      if (input$test=="Coma") {

          separador <- ","

      } else if(input$test=="Puntoycoma") {

          separador <- ";"

      } else if(input$test=="Tabular") {

          separador <- "\t"

      }

      # Se valida como se expresan las cadenas de caracteres del archivo
      if(input$radioquotmovies=="doublemovies"){

          comilla <- "\""

      } else if(input$radioquotmovies=="simplemovies"){

          comilla <- "\'"

      } else if(input$radioquotmovies=="nonemovies"){

          comilla <- ""

      }

      # Se lee el dataset de peliculas segun las validaciones anteriores
      movies_aux <- read.csv(inFile$datapath, header=h, sep=separador,quote = comilla)

      # Se identifican las columnas del dataset leido
      colnames(movies_aux) <- c("ID_pelicula", "titulo_pelicula", "generos" )

      # El titulo de cada pelicula va a ser tratado como un tipo de dato String (cadena de caracteres)
      movies_aux$titulo_pelicula <- as.character(movies_aux$titulo_pelicula)

      # Eliminando espacio al final del titulo de cada pelicula
      movies_aux$titulo_pelicula <- gsub(") ", ")", movies_aux$titulo_pelicula)

      # Se asigna a un nuevo vector "annos" los ultimos 6 caracteres de todas las celdas del titulo de cada pelicula
      annos <- ultimosCaracteres(as.character(movies_aux$titulo_pelicula), 6)
      # Se elimina el ultimo caracter a cada año para eliminar el parentesis
      annos <- substring(annos, 2)
      # Se elimina el primer caracter a cada año para eliminar el parentesis
      annos <- substring(annos, 1, nchar(annos)-1)

      # Se agrega una nueva columna al dataset correspondiente al año de cada pelicula
      movies_aux[ , "anno_pelicula"] <- annos

      # Eliminando parentesis en el titulo de cada pelicula
      movies_aux$titulo_pelicula <- gsub("\\(", "", movies_aux$titulo_pelicula)
      movies_aux$titulo_pelicula <- gsub(")", "", movies_aux$titulo_pelicula)

      # Colocando annos como titulo - anno
      movies_aux$titulo_pelicula <- sapply(movies_aux$titulo_pelicula, function(string) {

        yearMovie <- NULL
        yearMovie <- ultimosCaracteres(string,4)
        string <- substr(string, 1, nchar(string)-4)
        string <- paste(string,"- ",yearMovie, sep="")

      }  ) 

      # Eliminando signos en el titulo de cada pelicula
      movies_aux$titulo_pelicula <- gsub("\\?", "", movies_aux$titulo_pelicula)
      movies_aux$titulo_pelicula <- gsub("\\!", "", movies_aux$titulo_pelicula)

      # Eliminar variable generos
      movies_aux$generos <- NULL

      ####################################### Pre-procesamiento de links.csv #########################################################

      # Se obtiene el archivo 
      inFile <- input$fileLinks
      if (is.null(inFile)) return(NULL) 

      # Se valida si el archivo contiene un header
      if (input$includeheaderlinks) {

          h <- TRUE

      } else {

          h <- FALSE

      }

      # Se valida el caracter separador del archivo   
      if (input$radioseparatorlinks=="commalinks") {

          separador <- ","

      } else if(input$radioseparatorlinks=="semicolonlinks") {

          separador <- ";"

      } else if(input$radioseparatorlinks=="tablinks") {

          separador <- "\t"

      }

      # Se valida como se expresan las cadenas de caracteres del archivo
      if (input$radioquotlinks=="doublelinks") {

          comilla <- "\""

      } else if (input$radioquotlinks=="simplelinks") {

          comilla <- "\'"

      } else if (input$radioquotlinks=="nonelinks") {

          comilla <- ""

      }

      # Se lee el dataset de links segun las validaciones anteriores
      links <- read.csv(inFile$datapath, header=h, sep=separador,quote = comilla)

      # Se elimina el id de imdb
      links$imdbId <- NULL

      # Se identifican las columnas del dataset leido
      colnames(links) <- c("ID_pelicula", "tmdbId")

      # Se corresponden los links y movies segun el id de cada pelicula
      movies_aux <- merge(links,movies_aux,by="ID_pelicula")

      global$movies <- movies_aux

      ####################################### Pre-procesamiento de ratings.csv #########################################################

      # Se obtiene el archivo 
      inFile <- input$fileRatings
      if (is.null(inFile)) return(NULL)  

      # Se valida si el archivo contiene un header
      if (input$includeheaderratings) {

          h <- TRUE

      } else {

          h <- FALSE

      }
     
      # Se valida el caracter separador del archivo 
      if (input$radioseparatorratings=="commaratings") {

          separador <- ","

      } else if (input$radioseparatorratings=="semicolonratings") {

          separador <- ";"

      } else if (input$radioseparatorratings=="tabratings") {

          separador <- "\t"

      }

      # Se valida como se expresan las cadenas de caracteres del archivo
      if (input$radioquotratings=="doubleratings") {

          comilla <- "\""

      } else if (input$radioquotratings=="simpleratings") {

          comilla <- "\'"

      } else if (input$radioquotratings=="noneratings") {

          comilla <- ""

      }

      #Se lee el dataset de ratings segun las validaciones anteriores
      ratings_aux <- read.csv(inFile$datapath, header=h, sep=separador,quote = comilla)

      #Se elimina la columna timestamp
      ratings_aux$timestamp <- NULL

      #Se identifican las columnas del dataset leido
      colnames(ratings_aux) <- c("userId", "ID_pelicula", "puntuacion")

      global$ratings <- ratings_aux

      # Se envia un mensaje al javascript indicando todas las peliculas que se encuentran en el dataset
      session$sendCustomMessage(type = 'data', message = movies_aux$titulo_pelicula)

      # Se llama a la funcion rules()
      rules()

      # Se llama a la funcion graph()
      graph()

    })

  })

  # Funcion que se encarga de generar las reglas de asociacion
  rules <- function() {

    # Se corresponden los ratings y movies segun el id de cada pelicula
    merged <- merge(global$ratings,global$movies,by="ID_pelicula")

    # Se convierte el dataframe anterior a un conjunto de transacciones
    merged2 <- as( split(as.vector(merged$titulo_pelicula), as.vector(merged$userId)) , "transactions" )

    # Se llama a la funcion apriori con un soporte de 0.005 y confianza de 0.5
    rules <- apriori( merged2 , parameter=list(supp=0.005, conf=0.5, target="rules", minlen=2, maxlen=2, maxtime=60))

    # Se ordenan las reglas por su lift de mayor a menor
    rules <- sort(rules, by ="lift")

    global$rulesByName <- rules

  }
  
  # Funcion que muestra por pantalla las reglas generadas anteriormente
  graph <- function() {

    output$inspectRules <- renderPrint({

      # Se coloca un ancho bastante alto para que quepa toda la impresion de las subreglas en pantalla 
      options(width = 1500);

      # Se imprimen las 6 primeras reglas
      inspect(head(global$rulesByName));

    })

  }

})
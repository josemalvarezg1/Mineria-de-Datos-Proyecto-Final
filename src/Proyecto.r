#setwd("Documents/Proyecto-Mineria-de-Datos/")	# Cambiar directorio

library(shiny)	# Se incluye la biblioteca de Shiny
library(arules) # Se incluye la biblioteca de Reglas de Asociacion
library(arulesViz) # Se incluye la biblioteca de Reglas de Asociacion
library(stringr) # Se incluye la biblioteca para manejar cadenas de caracteres

runApp("app", display.mode = "showcase")	# Se ejecuta la aplicacion en Shiny

### Pre-procesamiento

#Se lee el dataset de peliculas
movies <- read.csv(file = "data/movies.csv", header = T)

#Se identifican las columnas del dataset leido
colnames(movies) <- c("ID_pelicula", "titulo_pelicula", "generos" )

#Funcion que retorna los ultimos caracteres de un string
ultimosCaracteres <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#El titulo de cada pelicula va a ser tratado como un tipo de dato String (cadena de caracteres)
movies$titulo_pelicula <- as.character(movies$titulo_pelicula)

#Eliminando espacio al final del titulo de cada pelicula
movies$titulo_pelicula <- gsub(") ", ")", movies$titulo_pelicula)

#Se asigna a un nuevo vector "annos" los ultimos 6 caracteres de todas las celdas del titulo de cada pelicula
annos <- ultimosCaracteres(as.character(movies$titulo_pelicula), 6)
#Se elimina el ultimo caracter a cada año para eliminar el parentesis
annos <- substring(annos, 2)
#Se elimina el primer caracter a cada año para eliminar el parentesis
annos <- substring(annos, 1, nchar(annos)-1)

#Se agrega una nueva columna al dataset correspondiente al año de cada pelicula
movies[ , "anno_pelicula"] <- annos

#Se eliminara el año del titulo de cada pelicula
movies$titulo_pelicula <- substring(movies$titulo_pelicula, 1, nchar(movies$titulo_pelicula)-6)

#Los generos de cada pelicula van a ser tratados como un tipo de dato String (cadena de caracteres)
movies$generos <- as.character(movies$generos)

#Se reemplaza el separador | por una coma en los generos de cada pelicula
movies$generos <- gsub("\\|", ",", movies$generos)

#Transformacion de los generos a variables dummies
t <- strsplit(movies$generos, split = ",")
tags <- unique(str_trim(unlist(t)))
df2 <- as.data.frame(Reduce(cbind, lapply(tags, function(i) sapply(t, function(j) +(any(grepl(i, j), na.rm = TRUE))))))
names(df2) <- tags

#Añadir variables dummies al dataframe movies
movies <- cbind(movies, df2)

# Eliminar variable generos
movies$generos <- NULL

#Se lee el dataset de ratings
ratings <- read.csv(file = "data/ratings.csv", header = T)

#Se elimina la columna userId
ratings$userId <- NULL
#Se elimina la columna timestamp
ratings$timestamp <- NULL

#Se identifican las columnas del dataset leido
colnames(ratings) <- c("ID_pelicula", "puntuacion" )

# Hacer el join de los dataframes
joined = cbind(movies[match(ratings$ID_pelicula, movies$ID_pelicula),], ratings$puntuacion)
joined$titulo_pelicula<-NULL

#Renombrar la columna de puntuacion
colnames(joined)[23]<-"puntuacion"

#Reglas de asociacion

# Se transforman las columnas a Factor para poder cambiar el dataframe a transactions

ratings$ID_pelicula <- factor(ratings$ID_pelicula)
ratings$puntuacion <- factor(ratings$puntuacion)

#Transformacion de dataframe a transactions
votacion <- as(ratings,"transactions")

# Ejecucion de Apriori con parametros por defecto
reglas <- apriori(votacion,parameter=list(sup = 0.001, conf = 0.001))

#Imprimir Reglas de asociacion
inspect(reglas)
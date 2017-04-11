#setwd("Documents/Proyecto-Mineria-de-Datos/")	# Cambiar directorio

library(shiny)	# Se incluye la biblioteca de Shiny
library(arules) # Se incluye la biblioteca de Reglas de Asociacion
library(arulesViz)

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

#Se lee el dataset de ratings
ratings <- read.csv(file = "data/ratings.csv", header = T)

#Se elimina la columna userId
ratings$userId <- NULL
#Se elimina la columna timestamp
ratings$timestamp <- NULL

#Se identifican las columnas del dataset leido
colnames(ratings) <- c("ID_pelicula", "puntuacion" )


alimentacion <- read.transactions("data/ratings.csv")
reglas <- apriori(alimentacion)
summary(reglas)


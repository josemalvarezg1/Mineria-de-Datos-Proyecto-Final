#setwd("Documents/Proyecto-Mineria-de-Datos/")	# Cambiar directorio

library(shiny)	# Se incluye la biblioteca de Shiny
library(arules) # Se incluye la biblioteca de Reglas de Asociacion
library(arulesViz) # Se incluye la biblioteca de Reglas de Asociacion
library(stringr) # Se incluye la biblioteca para manejar cadenas de caracteres
library(jsonlite) # Se incluye la biblioteca para manejar JSON

runApp("app", display.mode = "showcase")	# Se ejecuta la aplicacion en Shiny
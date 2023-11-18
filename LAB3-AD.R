install.packages("ggpubr")
install.packages("cluster")  # Instalar el paquete cluster si no está instalado
install.packages("ggplot")  # Instalar el paquete cluster si no está instalado
install.packages("arules")  # Instalar el paquete cluster si no está instalado
install.packages("arulesViz")  # Instalar el paquete cluster si no está instalado


library(dplyr)
library(ggpubr)
library(ggplot2) 
library(pROC)
library(caret)
library(cluster)  # Cargar el paquete cluster  
library(arules)
library(arulesViz)
library(arules)
library(arulesViz)
library(datasets)
library(rCBA)
library(tidyverse)



temp <- tempfile()

download.file("https://archive.ics.uci.edu/static/public/15/breast+cancer+wisconsin+original.zip", temp)

name_origen <- "breast-cancer-wisconsin.data"

file <- unz(temp, name_origen)

data <- read.table(file, fileEncoding = "UTF-8", sep = "," )

# Cambia los nombres de las columnas a minúsculas
colnames(data) <- c(
  "numero_de_codigo_de_muestra", "grosor_del_monton", "uniformidad_del_tamano_de_la_celula",
  "uniformidad_de_la_forma_de_la_celula", "adhesion_marginal",
  "tamano_de_celula_epitelial_individual", "nucleos_desnudos",
  "cromatina_blanda", "nucleolos_normales", "mitosis", "clase"
)



#limpieza de datos 

data[data == "?"] <- NA

# Convertir todas las columnas a tipo entero
data[] <- lapply(data, as.integer)

# Cambiar la variable "clase" a factor
data$clase <- as.factor(data$clase)

# Contar cuántas filas teníamos originalmente
filas_iniciales <- nrow(data)

# Eliminar las filas que contienen valores NA
data <- data[complete.cases(data),]

# Contar cuántas filas fueron descartadas
filas_descartadas <- filas_iniciales - nrow(data)

# Imprimir el resultado
cat("Número de filas descartadas:", filas_descartadas, "\n")

cat("Se descartan estas filas debido a la cantidad\n")

cat("Número de filas posible a utilizar en el analiss: ", nrow(data) , "\n")


#Otra solucion es calcular la media para distribuirlos en los datos faltantes 
#en este caso se opta por solo descartar los datos 

data <- data[, -1]  # Elimina la primera columna (numero_de_codigo_de_muestra)


cat("Elimina la primera columna (numero_de_codigo_de_muestra)")



data


#-------------------------------------------------------------------------------------



# Convertir la matriz a un formato transaccional

transacciones <- as(data[, c(
 "grosor_del_monton", "uniformidad_del_tamano_de_la_celula",
  "uniformidad_de_la_forma_de_la_celula", "adhesion_marginal",
  "tamano_de_celula_epitelial_individual", "nucleos_desnudos",
  "cromatina_blanda", "nucleolos_normales", "mitosis", "clase"
)], "transactions")

 
 


# Verificar la estructura de las transacciones
inspect(transacciones)

 
 
   
# Calcular el soporte relativo de cada item en las transacciones
soporte_items <- itemFrequency(transacciones, type = "relative")

# Multiplicar por 100 para obtener el porcentaje
soporte_items_porcentaje <- soporte_items * 100

# Imprimir el resumen en términos de porcentaje
su <- summary(soporte_items_porcentaje)

su

suporte <- as.numeric(su["1st Qu."] / 100)



print(suporte)


#--------------------------------------------------------------------------------


reglas <- apriori(transacciones, parameter = list(support = suporte, confidence = 0.8))

reglas 

summary(reglas)




# Suponiendo que 'reglas' es tu conjunto de reglas de asociación
plot(reglas, method = "graph")

 


# Ordenar reglas por confianza descendente
reglas_ordenadas <- sort(reglas, by = "confidence", decreasing = TRUE)

reglas_ordenadas



# Suponiendo que 'reglas' es tu conjunto de reglas de asociación
plot(reglas_ordenadas, method = "graph")

 

# Instala el paquete gridExtra si aún no lo has hecho
# install.packages("gridExtra")

# Cargar la biblioteca
library(gridExtra)

# Inicializar una lista para almacenar los gráficos
lista_de_graficos <- list()
 

# Iterar a través de diferentes valores de N
for (N in seq(200, 1000, by = 200)) {
  
  # Seleccionar las primeras N reglas más interesantes
  top_reglas <- head(reglas_ordenadas, n = N)
  
  lista_de_graficos[[as.character(N)]] <- plot(top_reglas, method = "graph")
   
}

# Mostrar todos los gráficos en una sola imagen
grid.arrange(grobs = lista_de_graficos, ncol = 3)
 


colnames(transacciones)






 ##-----------------------------------------------------------------------------------------

  

# Aplicar fpgrowth para encontrar reglas asociadas con la clase 
FPGrowth <- rCBA::fpgrowth(
  transacciones, 
  support = suporte, 
  confidence = 0.5, 
  maxLength = 2, 
  consequent = "clase"
)


summary(FPGrowth)

inspect(FPGrowth) 

plot(FPGrowth, method = "graph")

plot(FPGrowth)
 


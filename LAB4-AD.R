# Install the package
install.packages("rpart.plot")
install.packages("tree")
install.packages('rattle')
install.packages('C50')


# Import necessary libraries 
library(rpart)
library(rattle)
library(rpart.plot)
library(rattle)

library(rpart)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(C50)
library(partykit)


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

# Convert the column "Clase" to factor
data$clase <- as.factor(data$clase)

#cambiar a benigna a no y maligno yes
data$clase <- factor(data$clase, levels = c(2,4), labels = c("no", "yes"))


# Vector de etiquetas personalizadas
etiquetas_clases <- c("2 benigna", "4 maligna")


head(data)

#-----------------------------------------


#primer modelo 

#definir los set datos a usar para cada entrenamiento y prueba de un 70% para el entrenamiento 

set.seed(1649)

data_entrenamiento <- sample_frac(data, .9)
 
 
data_prueba <- setdiff(data, data_entrenamiento)

 
#entrenando el modelo  
 
modeloc50 <- C5.0( clase ~ . , data = data_entrenamiento )


# Imprimir un resumen del modelo
summary(modeloc50)
  

plot(modeloc50)

#evaluacion del modelo
data_predicted <- predict(modeloc50, data_prueba)

confusionMatrix(data = data_predicted, reference = data_prueba$clase, positive = "yes")

 


 #---------------------------------------------------
#segundo modelo 



#definir los set datos a usar para cada entrenamiento y prueba de un 70% para el entrenamiento 

set.seed(7439)

data_entrenamiento_2 <- sample_frac(data, .9)


data_prueba_2 <- setdiff(data, data_entrenamiento_2)


#entrenando el modelo  

modeloc50_2 <- C5.0( clase ~ . , data = data_entrenamiento_2 )


# Imprimir un resumen del modelo
summary(modeloc50_2)


plot(modeloc50_2)

#evaluacion del modelo
data_predicted_2 <- predict(modeloc50_2, data_prueba_2)

confusionMatrix(data = data_predicted_2, reference = data_prueba_2$clase, positive = "yes")




#---------------------------------------------------
#tercer modelo 



#definir los set datos a usar para cada entrenamiento y prueba de un 70% para el entrenamiento 

set.seed(8476)

data_entrenamiento_3 <- sample_frac(data, .9)


data_prueba_3 <- setdiff(data, data_entrenamiento_3)


#entrenando el modelo  

modeloc50_3 <- C5.0( clase ~ . , data = data_entrenamiento_3 )


# Imprimir un resumen del modelo
summary(modeloc50_3)


plot(modeloc50_3)

#evaluacion del modelo
data_predicted_3 <- predict(modeloc50_3, data_prueba_3)

confusionMatrix(data = data_predicted_3, reference = data_prueba_3$clase, positive = "yes")





#mejorar el modelo usando Boosting


modeloc50_4 <- C5.0( clase ~ . , data = data_entrenamiento_3, trials = 10 )


data_predicted_4 <- predict(modeloc50_4, data_prueba_3)

confusionMatrix(data = data_predicted_4, reference = data_prueba_3$clase, positive = "yes")


#--Matriz de coste


matrix_dimensions <- list(c("yes", "no"), c("yes", "no"))
names(matrix_dimensions) <- c("prediction", "reference")

error_cost <- matrix(c(0, 4, 1, 0), nrow = 2, dimnames = matrix_dimensions)
error_cost

 
modeloc50_costs <- C5.0( clase ~ . , data = data_entrenamiento_3,  costs = error_cost )

# Imprimir un resumen del modelo
summary(modeloc50_costs)


plot(modeloc50_costs)


data_predicted_costs <- predict(modeloc50_costs, data_prueba_3)


confusionMatrix(data = data_predicted_costs, reference = data_prueba_3$clase, positive = "yes")











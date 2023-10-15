install.packages("ggpubr")

library(dplyr)
library(ggpubr)
library(ggplot2) 
library(pROC)
library(caret)


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

# 1. Número de código de muestra: Es un número de identificación único para cada muestra.
# 2. Grosor del montón: Representa el grosor del tejido de la masa en una escala del 1 al 10.
# 3. Uniformidad del tamaño de la célula: Mide la uniformidad en el tamaño de las células y varía de 1 a 10.
# 4. Uniformidad de la forma de la célula: Mide la uniformidad en la forma de las células y varía de 1 a 10.
# 5. Adhesión marginal: Indica el nivel de adhesión de las células en los márgenes y varía de 1 a 10.
# 6. Tamaño de célula epitelial individual: Representa el tamaño de las células epiteliales individuales en una escala del 1 al 10.
# 7. Núcleos desnudos: Mide la cantidad de núcleos desnudos presentes y varía de 1 a 10.
# 8. Cromatina blanda: Indica la calidad de la cromatina, con una puntuación que va de 1 a 10.
# 9. Nucléolos normales: Mide la cantidad de nucléolos normales en las células y varía de 1 a 10.
# 10. Mitosis: Representa la tasa de mitosis celular y varía de 1 a 10.
# 11. Clase: Es la variable objetivo que indica la clase de la muestra. Tiene dos valores posibles: 2 para benigno y 4 para maligno. Esta variable se utiliza comúnmente para la clasificación de tumores como benignos o malignos en un contexto médico.

#https://archive.ics.uci.edu/static/public/15/breast+cancer+wisconsin+original.zip

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




# Función para calcular estadísticas solo en columnas numéricas
calculate_column_statistics <- function(data) {
  result <- data.frame()
  for (col_name in names(data)) {
    if (is.numeric(data[, col_name])) {
      # Resumen básico con summary()
      basic_summary <- summary(data[, col_name])
      
      # Media de la columna
      mean_value <- mean(data[, col_name])
      
      # Cantidad de valores en la columna
      count_values <- length(data[, col_name])
      
      # Desviación estándar de la columna
      sd_value <- sd(data[, col_name])
      
      # Mínimo y máximo de la columna
      min_value <- min(data[, col_name])
      max_value <- max(data[, col_name])
      
      # Mediana de la columna
      median_value <- median(data[, col_name])
      
      # Crear un resumen de estadísticas para la columna actual
      col_summary <- data.frame(
        Variable = col_name, 
        Media = mean_value,
        Cantidad_Valores = count_values,
        Desviacion_Estandar = sd_value,
        Minimo = min_value,
        Maximo = max_value,
        Mediana = median_value
      )
      
      # Agregar el resumen al resultado
      result <- rbind(result, col_summary)
    }
  }
  return(result)
}

# Llamar a la función para calcular estadísticas en columnas numéricas
column_statistics <- calculate_column_statistics(data)

# Imprimir el resumen de estadísticas
print(column_statistics)






##HISTOGRAMAS 
 
# Crear histogramas para todas las variables excepto "numero_de_codigo_de_muestra" y "clase"

# Variable 1: Grosor del Montón
ggplot(data, aes(x = grosor_del_monton)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histograma del Grosor del Montón", x = "Grosor del Montón", y = "Frecuencia") +
  scale_y_continuous(breaks = seq(0, 200, by = 5))
 

#  según la "Clase"
his <- ggplot(data, aes(x = grosor_del_monton, fill = factor(clase))) +
  geom_histogram(binwidth = 0.5, color = "black") +
  labs(title = "Histograma del Grosor del Montón por Clase", x = "Grosor del Montón", y = "Frecuencia") +
  scale_fill_manual(values = c("2" = "#00AFBB", "4" = "#FC4E07"), name = "Clase", labels = c("Benigno", "Maligno"))

print(his)
 

hist_data <- data %>%
  mutate(bin = cut(grosor_del_monton, breaks = seq(0, max(grosor_del_monton) + 0.5, by = 0.5))) %>%
  group_by(bin) %>%
  summarise(frequency = n())

# Imprime la tabla resultante
print(hist_data)


##------------------------------------------------------------------------------
 

# Variable 2: Uniformidad del Tamaño de la Célula
ggplot(data, aes(x = uniformidad_del_tamano_de_la_celula)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histograma de Uniformidad del Tamaño de la Célula", x = "Uniformidad del Tamaño de la Célula", y = "Frecuencia") +
  scale_y_continuous(breaks = seq(0, 400, by = 20))


#  según la "Clase"
ggplot(data, aes(x = uniformidad_del_tamano_de_la_celula, fill = factor(clase))) +
  geom_histogram(binwidth = 0.5, color = "black") +
  labs(title = "Histograma de Uniformidad del Tamaño de la Célula", x = "Uniformidad del Tamaño de la Célula", y = "Frecuencia") +
  scale_fill_manual(values = c("2" = "#00AFBB", "4" = "#FC4E07"), name = "Clase", labels = c("Benigno", "Maligno"))


hist_data <- data %>%
  mutate(bin = cut(uniformidad_del_tamano_de_la_celula, breaks = seq(0, max(uniformidad_del_tamano_de_la_celula) + 0.5, by = 0.5))) %>%
  group_by(bin) %>%
  summarise(frequency = n())

# Imprime la tabla resultante
print(hist_data)


##------------------------------------------------------------------------------




# Variable 3: Uniformidad de la Forma de la Célula
ggplot(data, aes(x = uniformidad_de_la_forma_de_la_celula)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histograma de Uniformidad de la Forma de la Célula", x = "Uniformidad de la Forma de la Célula", y = "Frecuencia") +
  scale_y_continuous(breaks = seq(0, 400, by = 20))


#  según la "Clase"
ggplot(data, aes(x = uniformidad_de_la_forma_de_la_celula, fill = factor(clase))) +
  geom_histogram(binwidth = 0.5, color = "black") +
  labs(title = "Histograma de Uniformidad de la Forma de la Célula", x = "Uniformidad de la Forma de la Célula", y = "Frecuencia") +
  scale_fill_manual(values = c("2" = "#00AFBB", "4" = "#FC4E07"), name = "Clase", labels = c("Benigno", "Maligno"))




hist_data <- data %>%
  mutate(bin = cut(uniformidad_de_la_forma_de_la_celula, breaks = seq(0, max(uniformidad_de_la_forma_de_la_celula) + 0.5, by = 0.5))) %>%
  group_by(bin) %>%
  summarise(frequency = n())

# Imprime la tabla resultante
print(hist_data)




##------------------------------------------------------------------------------




# Variable 4: Adhesión Marginal
ggplot(data, aes(x = adhesion_marginal)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histograma de Adhesión Marginal", x = "Adhesión Marginal", y = "Frecuencia") +
  scale_y_continuous(breaks = seq(0, 400, by = 20))

#  según la "Clase"
ggplot(data, aes(x = adhesion_marginal, fill = factor(clase))) +
  geom_histogram(binwidth = 0.5, color = "black") +
  labs(title = "Histograma de Adhesión Marginal", x = "Adhesión Marginal", y = "Frecuencia") +
  scale_fill_manual(values = c("2" = "#00AFBB", "4" = "#FC4E07"), name = "Clase", labels = c("Benigno", "Maligno"))



hist_data <- data %>%
  mutate(bin = cut(adhesion_marginal, breaks = seq(0, max(adhesion_marginal) + 0.5, by = 0.5))) %>%
  group_by(bin) %>%
  summarise(frequency = n())

# Imprime la tabla resultante
print(hist_data)



##------------------------------------------------------------------------------






# Variable 5: Tamaño de Célula Epitelial Individual
ggplot(data, aes(x = tamano_de_celula_epitelial_individual)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histograma de Tamaño de Célula Epitelial Individual", x = "Tamaño de Célula Epitelial Individual", y = "Frecuencia") +
  scale_y_continuous(breaks = seq(0, 400, by = 20))

#  según la "Clase"
ggplot(data, aes(x = tamano_de_celula_epitelial_individual, fill = factor(clase))) +
  geom_histogram(binwidth = 0.5, color = "black") +
  labs(title = "Histograma de Tamaño de Célula Epitelial Individual", x = "Tamaño de Célula Epitelial Individual", y = "Frecuencia") +
  scale_fill_manual(values = c("2" = "#00AFBB", "4" = "#FC4E07"), name = "Clase", labels = c("Benigno", "Maligno"))



hist_data <- data %>%
  mutate(bin = cut(tamano_de_celula_epitelial_individual, breaks = seq(0, max(tamano_de_celula_epitelial_individual) + 0.5, by = 0.5))) %>%
  group_by(bin) %>%
  summarise(frequency = n())

# Imprime la tabla resultante
print(hist_data)



##------------------------------------------------------------------------------







# Variable 6: Núcleos Desnudos
ggplot(data, aes(x = nucleos_desnudos)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histograma de Núcleos Desnudos", x = "Núcleos Desnudos", y = "Frecuencia") +
  scale_y_continuous(breaks = seq(0, 400, by = 20))

#  según la "Clase"
ggplot(data, aes(x = nucleos_desnudos, fill = factor(clase))) +
  geom_histogram(binwidth = 0.5, color = "black") +
  labs(title = "Histograma de Núcleos Desnudos", x = "Núcleos Desnudos", y = "Frecuencia") +
  scale_fill_manual(values = c("2" = "#00AFBB", "4" = "#FC4E07"), name = "Clase", labels = c("Benigno", "Maligno"))



hist_data <- data %>%
  mutate(bin = cut(nucleos_desnudos, breaks = seq(0, max(nucleos_desnudos) + 0.5, by = 0.5))) %>%
  group_by(bin) %>%
  summarise(frequency = n())

# Imprime la tabla resultante
print(hist_data)



##------------------------------------------------------------------------------








# Variable 7: Cromatina Blanda
ggplot(data, aes(x = cromatina_blanda)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histograma de Cromatina Blanda", x = "Cromatina Blanda", y = "Frecuencia") +
  scale_y_continuous(breaks = seq(0, 200, by = 5))

ggplot(data, aes(x = cromatina_blanda, fill = factor(clase))) +
  geom_histogram(binwidth = 0.5, color = "black") +
  labs(title = "Histograma de Cromatina Blanda", x = "Cromatina Blanda", y = "Frecuencia") +
  scale_fill_manual(values = c("2" = "#00AFBB", "4" = "#FC4E07"), name = "Clase", labels = c("Benigno", "Maligno"))



hist_data <- data %>%
  mutate(bin = cut(cromatina_blanda, breaks = seq(0, max(cromatina_blanda) + 0.5, by = 0.5))) %>%
  group_by(bin) %>%
  summarise(frequency = n())

# Imprime la tabla resultante
print(hist_data)



##------------------------------------------------------------------------------











# Variable 8: Nucléolos Normales
ggplot(data, aes(x = nucleolos_normales)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histograma de Nucléolos Normales", x = "Nucléolos Normales", y = "Frecuencia") +
  scale_y_continuous(breaks = seq(0, 500, by = 20))


ggplot(data, aes(x = nucleolos_normales, fill = factor(clase))) +
  geom_histogram(binwidth = 0.5, color = "black") +
  labs(title = "Histograma de Nucléolos Normales", x = "Nucléolos Normales", y = "Frecuencia") +
  scale_fill_manual(values = c("2" = "#00AFBB", "4" = "#FC4E07"), name = "Clase", labels = c("Benigno", "Maligno"))




hist_data <- data %>%
  mutate(bin = cut(nucleolos_normales, breaks = seq(0, max(nucleolos_normales) + 0.5, by = 0.5))) %>%
  group_by(bin) %>%
  summarise(frequency = n())

# Imprime la tabla resultante
print(hist_data)



##------------------------------------------------------------------------------











# Variable 9: Mitosis
ggplot(data, aes(x = mitosis)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histograma de Mitosis", x = "Mitosis", y = "Frecuencia") +
  scale_y_continuous(breaks = seq(0, 600, by = 20))


ggplot(data, aes(x = mitosis, fill = factor(clase))) +
  geom_histogram(binwidth = 0.5, color = "black") +
  labs(title = "Histograma de Mitosis", x = "Mitosis", y = "Frecuencia") +
  scale_fill_manual(values = c("2" = "#00AFBB", "4" = "#FC4E07"), name = "Clase", labels = c("Benigno", "Maligno"))



hist_data <- data %>%
  mutate(bin = cut(mitosis, breaks = seq(0, max(mitosis) + 0.5, by = 0.5))) %>%
  group_by(bin) %>%
  summarise(frequency = n())

# Imprime la tabla resultante
print(hist_data)



##------------------------------------------------------------------------------







 
 

# Crear un gráfico de barras apiladas para la variable "Clase"
ggplot(data, aes(x = factor(clase), fill = factor(clase))) +
  geom_bar() +
  labs(title = "Distribución de Clases", x = "Clase", y = "Frecuencia") +
  scale_fill_manual(values = c("2" = "#00AFBB", "4" = "#FC4E07")) +
  scale_x_discrete(labels = c("2" = "Benigno", "4" = "Maligno")) +
  geom_text(stat = "count", aes(label = after_stat(count), vjust = -0.5))


 


#dispercion grosor_del_monton 

 
g1 <- ggscatter(data,
                x = "grosor_del_monton",
                y = "uniformidad_del_tamano_de_la_celula",
                color = "clase",
                xlab = "grosor_del_monton",
                ylab = "uniformidad_del_tamano_de_la_celula") +
  scale_color_manual(values = c("2" = "#00AFBB", "4" = "#FC4E07"), name = "Clase", labels = c("Benigno", "Maligno"))


g2 <- ggscatter(data,
                x = "grosor_del_monton",
                y = "uniformidad_de_la_forma_de_la_celula",
                color = "clase",
                xlab = "grosor_del_monton",
                ylab = "uniformidad_de_la_forma_de_la_celula") +
  scale_color_manual(values = c("2" = "#00AFBB", "4" = "#FC4E07"), name = "Clase", labels = c("Benigno", "Maligno"))


g3 <- ggscatter(data,
                x = "grosor_del_monton",
                y = "adhesion_marginal",
                color = "clase",
                xlab = "grosor_del_monton",
                ylab = "adhesion_marginal") +
  scale_color_manual(values = c("2" = "#00AFBB", "4" = "#FC4E07"), name = "Clase", labels = c("Benigno", "Maligno"))


g4 <- ggscatter(data,
                x = "grosor_del_monton",
                y = "tamano_de_celula_epitelial_individual",
                color = "clase",
                xlab = "grosor_del_monton",
                ylab = "tamano_de_celula_epitelial_individual") +
  scale_color_manual(values = c("2" = "#00AFBB", "4" = "#FC4E07"), name = "Clase", labels = c("Benigno", "Maligno"))


g5 <- ggscatter(data,
                x = "grosor_del_monton",
                y = "nucleos_desnudos",
                color = "clase",
                xlab = "grosor_del_monton",
                ylab = "nucleos_desnudos") +
  scale_color_manual(values = c("2" = "#00AFBB", "4" = "#FC4E07"), name = "Clase", labels = c("Benigno", "Maligno"))


g6 <- ggscatter(data,
                x = "grosor_del_monton",
                y = "cromatina_blanda",
                color = "clase",
                xlab = "grosor_del_monton",
                ylab = "cromatina_blanda") +
  scale_color_manual(values = c("2" = "#00AFBB", "4" = "#FC4E07"), name = "Clase", labels = c("Benigno", "Maligno"))

g7 <- ggscatter(data,
                x = "grosor_del_monton",
                y = "nucleolos_normales",
                color = "clase",
                xlab = "grosor_del_monton",
                ylab = "nucleolos_normales") +
  scale_color_manual(values = c("2" = "#00AFBB", "4" = "#FC4E07"), name = "Clase", labels = c("Benigno", "Maligno"))

g8 <- ggscatter(data,
                x = "grosor_del_monton",
                y = "mitosis",
                color = "clase",
                xlab = "grosor_del_monton",
                ylab = "mitosis") +
  scale_color_manual(values = c("2" = "#00AFBB", "4" = "#FC4E07"), name = "Clase", labels = c("Benigno", "Maligno"))


g <- ggarrange(g1, g2, g3, g4, g5, g6, g7, g8,  ncol = 3, nrow = 3 , common.legend = TRUE)

print(g)



 
 

#matriz de dispersion de variables 

# Define las variables que deseas incluir 
variables <- c("grosor_del_monton", "uniformidad_del_tamano_de_la_celula",
               "uniformidad_de_la_forma_de_la_celula", "adhesion_marginal",
               "tamano_de_celula_epitelial_individual", "nucleos_desnudos",
               "cromatina_blanda", "nucleolos_normales", "mitosis")



# Crea una matriz de dispersión
pairs(data[variables], pch = 19, col = ifelse(data$clase == 2, "#00AFBB", "#FC4E07"),
      main = "Matriz de Dispersión de Variables",
      cex.main = 1.5)

# Agrega colores a los puntos
legend("topright", legend = c("Benigno (2)", "Maligno (4)"), col = c("#00AFBB", "#FC4E07"), pch = 19, title = "Clase")




# Crea una subconjunto de datos solo con las variables seleccionadas
data_subset <- data[variables]

# Calcula la matriz de correlación
correlation_matrix <- cor(data_subset)

correlation_matrix

# Imprime la matriz de correlación
# Cargar la librería corrplot
library(corrplot)

# Calcular la matriz de correlación
correlation_matrix <- cor(data_subset)

# Crear un gráfico de matriz de correlación
corrplot(correlation_matrix, method = "color")


correlation_matrix



 # Grá fico Q-Q para cada variable  . 
g1 <- ggqqplot(data, x = "grosor_del_monton", color = "#FC4E07") +
  labs(title = "Gráfico Q-Q - Grosor del Montón", x = "Cuantiles teóricos", y = "Cuantiles observados")

g2 <- ggqqplot(data, x = "uniformidad_del_tamano_de_la_celula", color = "#FC4E07") +
  labs(title = "Gráfico Q-Q - Uniformidad del Tamaño de la Célula", x = "Cuantiles teóricos", y = "Cuantiles observados")

g3 <- ggqqplot(data, x = "uniformidad_de_la_forma_de_la_celula", color = "#FC4E07") +
  labs(title = "Gráfico Q-Q - Uniformidad de la Forma de la Célula", x = "Cuantiles teóricos", y = "Cuantiles observados")

g4 <- ggqqplot(data, x = "adhesion_marginal", color = "#FC4E07") +
  labs(title = "Gráfico Q-Q - Adhesión Marginal", x = "Cuantiles teóricos", y = "Cuantiles observados")

g5 <- ggqqplot(data, x = "tamano_de_celula_epitelial_individual", color = "#FC4E07") +
  labs(title = "Gráfico Q-Q - Tamaño de Célula Epitelial Individual", x = "Cuantiles teóricos", y = "Cuantiles observados")

g6 <- ggqqplot(data, x = "nucleos_desnudos", color = "#FC4E07") +
  labs(title = "Gráfico Q-Q - Núcleos Desnudos", x = "Cuantiles teóricos", y = "Cuantiles observados")

g7 <- ggqqplot(data, x = "cromatina_blanda", color = "#FC4E07") +
  labs(title = "Gráfico Q-Q - Cromatina Blanda", x = "Cuantiles teóricos", y = "Cuantiles observados")

g8 <- ggqqplot(data, x = "nucleolos_normales", color = "#FC4E07") +
  labs(title = "Gráfico Q-Q - Nucleolos Normales", x = "Cuantiles teóricos", y = "Cuantiles observados")

g9 <- ggqqplot(data, x = "mitosis", color = "#FC4E07") +
  labs(title = "Gráfico Q-Q - Mitosis", x = "Cuantiles teóricos", y = "Cuantiles observados")

g <- ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, ncol = 3, nrow = 3, common.legend = TRUE)

print(g)


##tiene sentido hacer una prueba de T de student ???? Yo creo que no ya que ninguna se hacerla a una distribucion normal 





#debido a que los datos no siguen una forma normal no se puede aplicar una regresion lineal es por esto que se 
#procede a realizar una regresion logistica 
 


set.seed(600)


# Separar conjuntos de entrenamiento y prueba .
n <- nrow ( data )
n_entrenamiento <- floor (0.8 * n )
muestra <- sample.int( n = n , size = n_entrenamiento , replace = FALSE )
entrenamiento <- data[muestra,]
prueba <- data[-muestra,]

# Ajustar modelo .
modelo <- glm (clase ~ .  , family = binomial ( link = "logit") , data = entrenamiento )
print(summary(modelo))



# Evaluar el modelo con el conjunto de entrenamiento .
cat ("Evaluación del modelo a partir del conjunto de entrenamiento :\n")
probs_e <- predict( modelo , entrenamiento , type = "response")


umbral <- 0.5
preds_e <- sapply ( probs_e , function ( p ) ifelse ( p >= umbral , "4", "2") )
preds_e <- factor ( preds_e , levels = levels ( data[["clase"]]) )



ROC_e <- roc( entrenamiento[["clase"]] , probs_e )
plot(ROC_e )


matriz_e <- confusionMatrix( preds_e , entrenamiento[["clase"]])
print(matriz_e)


# Evaluar el modelo con el conjunto de prueba .
cat ("Evaluación del modelo a partir del conjunto de prueba :\n")
probs_p <- predict( modelo , prueba , type = "response")



preds_p <- sapply( probs_p , function ( p ) ifelse ( p >= umbral , "4", "2") )
preds_p <- factor( preds_p , levels = levels ( data[["clase"]]) )

ROC_p <- roc(prueba[["clase"]] , probs_p )
plot(ROC_p)

matriz_p <- confusionMatrix( preds_p , prueba[["clase"]])
print(matriz_p)

 

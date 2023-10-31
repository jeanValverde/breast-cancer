install.packages("ggpubr")
install.packages("cluster")  # Instalar el paquete cluster si no está instalado

library(dplyr)
library(ggpubr)
library(ggplot2) 
library(pROC)
library(caret)
library(cluster)  # Cargar el paquete cluster


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


# Selecciona las variables numéricas
datos_num <- data[, 0:10]

##metodo de codo 

# Realiza el Método del Codo para varios valores de K
wss <- numeric(10)
for (i in 1:10) {
  kmeans_model <- kmeans(datos_num, centers = i, nstart = 10)
  wss[i] <- kmeans_model$tot.withinss
}

wss

# Visualiza el gráfico del Método del Codo
plot(1:10, wss, type = "b", xlab = "Número de Clusters (K)", ylab = "Within-cluster Sum of Squares")




# Convierte las columnas a tipo numérico
datos_num <- as.data.frame(sapply(datos_num, as.numeric))

# Normaliza los datos
datos_estandarizados <- scale(datos_num)

datos_estandarizados



# Aplicar el algoritmo K-Means
num_clusters <- 3  # Define el número de clusters deseado

kmeans_model <- kmeans(datos_estandarizados, centers = num_clusters, nstart = 10)

# Obtener las etiquetas de cluster para cada observación
cluster_labels <- kmeans_model$cluster

# Obtener los centroides de los clusters
cluster_centers <- kmeans_model$centers

# Los resultados están en "cluster_labels" y "cluster_centers"

cluster_labels
cluster_centers


# los datos en un dataframe llamado "data" y los resultados de K-Means en "cluster_labels" y "cluster_centers"



# Crear un dataframe que incluya tus datos y las etiquetas de cluster
clustered_data <- data.frame(datos_estandarizados, Cluster = as.factor(cluster_labels))

# Crear un gráfico de dispersión con colores por cluster
ggplot(clustered_data, aes(x = grosor_del_monton, y = uniformidad_del_tamano_de_la_celula, color = Cluster)) +
  geom_point() +
  geom_point(data = data.frame(cluster_centers, Cluster = factor(1:num_clusters)), aes(x = grosor_del_monton, y = uniformidad_del_tamano_de_la_celula), color = "red", shape = 4, size = 5) +
  scale_color_manual(values = c("1" = "blue", "2" = "green", "3" = "purple")) +
  labs(title = "Resultado de K-Means", x = "grosor_del_monton", y = "uniformidad_del_tamano_de_la_celula") +
  theme_minimal()

clustered_data


#Laboratorio 2 Analisis de datos

#Bryan Salgado
#Nicolas Torreblanca

#----------------------------------------------------------------------------

#Primero que nada, importamos las librerias


library(readr)
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(klaR)
library(clustMixType)

#----------------------------------------------------------------------------

#Leemos el archivo del dataset

datos_historico <- read_csv("urinalysis_tests.csv")

#Comenzamos a procesar los datos que requieran ser procesados

datos_procesados <- datos_historico
str(datos_procesados)

#Se realiza el procesamiento por cada tipo de variable
#Variables dicotomicas
#Se pasan de positivos y negativos de 1 y 0.
#Genero, diagnostico, transparencia y proteina.

datos_procesados$Gender <- ifelse(datos_procesados$Gender == "MALE", 0, 1)
datos_procesados$Diagnosis <- ifelse(datos_procesados$Diagnosis == "NEGATIVE", 0, 1)
datos_procesados$Transparency <- ifelse(datos_procesados$Transparency == "CLEAR",1,0)
datos_procesados$Protein <- ifelse(datos_procesados$Protein == "NEGATIVE",0,1)

# ---------------------------------------------------------------------------------

# Variables cuantificadoras
# Se agrupan en intervalos
# Edad

intervalos_edad <- c(0.0,12.0,18.0,30.0,60.0,80.0,199.0)
grupos_edad <- c(1, 2, 3, 4,5,6)

datos_procesados$Age <-cut(datos_procesados$Age, breaks = intervalos_edad, labels = grupos_edad, right = FALSE)

#PH

intervalos_pH <- c(5.0,5.5,6.0,6.5,7.0,7.5,8.0, 8.5)
grupos_pH <- c(1, 2, 3, 4,5,6,7)

datos_procesados$pH <-cut(datos_procesados$pH, breaks = intervalos_pH, labels = grupos_pH, right = FALSE)

#Specific Gravity

intervalos_SG <- c(1.000,1.005,1.010,1.015,1.020,1.025,1.030,1.035,1.040)
grupos_SG <- c(1, 2, 3, 4,5,6,7,8)

datos_procesados$`Specific Gravity` <-cut(datos_procesados$`Specific Gravity`, breaks = intervalos_SG, labels = grupos_SG, right = FALSE)

# ---------------------------------------------------------------------------------

# Variables Categoricas
# Se pasan de un string a valor numerico

# Color

categorias_colores <- unique(datos_procesados$Color)
numeros_colores <- 1:length(categorias_colores)
mapeo_colores <- setNames(numeros_colores, categorias_colores)
datos_procesados$Color <- mapeo_colores[datos_procesados$Color]  

#Glucose

categorias_glucose <- c("NEGATIVE", "TRACE", "1+", "2+", "3+", "4+")
numeros_glucosa <- 0:length(categorias_glucose)
mapeo_glucosa <- setNames(numeros_glucosa, categorias_glucose)
datos_procesados$Glucose <- mapeo_glucosa[datos_procesados$Glucose]  

# WBC

# Función para asignar grupos
asignar_grupos_blood_cells <- function(valor) {
  if (valor == "LOADED" | valor == "TNTC") {
    return(valor)
  } else {
    primer_digito <- substr(valor, 1, 1)  # Extraer el primer dígito
    ultimo_digito <- substr(valor, nchar(valor), nchar(valor))  # Extraer el último dígito
    
    # Verificar si el primer y último dígito cumplen los criterios
    if (primer_digito %in% c("0", "1", "2", "3", "4") & ultimo_digito %in% c("0", "1", "2", "3", "4", "5")) {
      return("0-5")
    } else {
      return(">5")
    }
  }
}

datos_procesados$WBC <- sapply(datos_procesados$WBC,asignar_grupos_blood_cells)
datos_procesados$RBC <- sapply(datos_procesados$RBC,asignar_grupos_blood_cells)

categorias_BC <- unique(datos_procesados$WBC)
numeros_BC <- 1:length(categorias_BC)


#WBC
mapeo_wbc <- setNames(numeros_BC, categorias_BC)
datos_procesados$WBC <- mapeo_wbc[datos_procesados$WBC]  

# RBC

mapeo_RBC <- setNames(numeros_BC, categorias_BC)
datos_procesados$RBC <- mapeo_RBC[datos_procesados$RBC]  

# ---------------------------------------------------------------------------------

# Variables Ordinales
# Estas están ordenadas según la frecuencia en que aparecen dentro de un examen.
# Primero se especifica los ordenes de cada una
# Se asigna un valor numerico a cada frecuencia
# Se remplaza la etiqueta por ese valor numerico despues.

categorias_ordinales <- c("NONE SEEN","RARE","FEW","OCCASIONAL","MODERATE","PLENTY","LOADED")
numeros_ordinales <- 0:length(categorias_ordinales)

# Epitelial Cells

mapeo_EC <- setNames(numeros_ordinales, categorias_ordinales)
datos_procesados$`Epithelial Cells` <- mapeo_EC[datos_procesados$`Epithelial Cells`]  


# Mucous Threads

mapeo_MT <- setNames(numeros_ordinales, categorias_ordinales)
datos_procesados$`Mucous Threads` <- mapeo_MT[datos_procesados$`Mucous Threads`]  

# Amorphous Urates

mapeo_AU <- setNames(numeros_ordinales, categorias_ordinales)
datos_procesados$`Amorphous Urates` <- mapeo_AU[datos_procesados$`Amorphous Urates`]  

# Bacteria

mapeo_Bacteria <- setNames(numeros_ordinales, categorias_ordinales)
datos_procesados$Bacteria <- mapeo_Bacteria[datos_procesados$Bacteria]  


# -----------------------------------------------------------------------
# Se elimina única observacion con un dato NA

datos_procesados <- na.omit(datos_procesados)

# -------------------------------------------------

#Revisamos cuales son los valores nulos dentro del set de datos

#contar_na <-colSums(is.na(datos_procesados))
#contar_na

#Ignoramos las filas en donde se encuentren.

datos_procesados <- na.omit(datos_procesados)

# Se calcula el número de clusters a utilizar

datos_numericos <- sapply(datos_procesados[, -1] , as.numeric)

<<<<<<< HEAD
# # Utiliza fviz_nbclust para visualizar diferentes criterios
nclusters_sil <- fviz_nbclust(datos_numericos, pam, method = "silhouette")
# 
# # Visualiza el resultado
print(nclusters_sil)
=======
# Utiliza fviz_nbclust para visualizar diferentes criterios
nclusters <- fviz_nbclust(datos_numericos, pam, method = "silhouette")

# Visualiza el resultado
print(nclusters)
>>>>>>> 93f735acf903c8064a2f515293d38c6b736be6da

nclusters_wss <- fviz_nbclust(datos_numericos, pam, method = "wss")
# 
# # Visualiza el resultado
print(nclusters_wss)

# Aplicar PAM con 5 clusters
resultado_pam <- pam(datos_numericos , k = 5)

fviz_cluster(resultado_pam, data = datos_numericos)


# Obtener la asignación de clusters para cada observación
clusters <- resultado_pam$clustering

# Crear dataframes para cada cluster
datos_por_cluster <- lapply(1:max(clusters), function(i) datos_numericos[clusters == i, ])

# Realizar análisis adicional para cada cluster
for (i in 1:length(datos_por_cluster)) {
  cat("Cluster", i, ":\n")
  print(summary(datos_por_cluster[[i]]))
}


<<<<<<< HEAD
=======
# resultado_kmodes <- kmodes(datos_numericos, modes = 3)
# 
# # Ver los resultados
# print(resultado_kmodes)
# 
# pca_result <- prcomp(datos_numericos)
# 
# # Graficamos los clusters en el espacio de las dos primeras componentes principales
# fviz_cluster(list(data = pca_result$x, cluster = resultado_kmodes$cluster))
# 
# 
# 
# resultado_kproto <- kproto(datos_procesados, k = 5)
# 
# # Ver los resultados
# print(resultado_kproto)
# 
# pca_result <- prcomp(datos_numericos)
# 
# fviz_cluster(list(data = pca_result$x, cluster = resultado_kproto$cluster))
# 
# fviz_cluster(resultado_kproto, data = datos_procesados, geom = "point", stand = FALSE)
# 
# 
# pca_result <- prcomp(datos_numericos, scale. = TRUE)
# 
# # Paso 2: Proyectar los datos en un espacio de menor dimensionalidad
# datos_pca <- as.data.frame(predict(pca_result))
# 
# # Paso 3: Visualizar los clusters en el nuevo espacio
# # Supongamos que ya tienes los clusters obtenidos mediante algún algoritmo de clustering, como k-means o k-modes
# # Reemplaza 'resultado_clusters' con tus resultados reales
# fviz_cluster(resultado_pam, geom = "point", data = datos_pca)
# 
# # -------------------------------------------------

# Columnas de interes

interes <- datos_numericos[, c("Transparency", "Bacteria", "WBC", "Diagnosis")]

interes <- datos_numericos[, c("Color", "RBC", "WBC", "Diagnosis")]

interes <- datos_numericos[, c("Transparency", "Gender", "Protein", "Diagnosis")] ###


interes <- datos_numericos[, c("Amorphous Urates", "Epithelial Cells", "Bacteria", "Diagnosis")]

interes <- datos_numericos[, c("Bacteria", "WBC", "Epithelial Cells", "Glucose",
                               "Protein", "Diagnosis")]


# Utiliza fviz_nbclust para visualizar diferentes criterios
nclusters <- fviz_nbclust(interes, pam, method = "silhouette")

# Visualiza el resultado
print(nclusters)

# Aplicar PAM con 5 clusters
resultado_interes <- pam(interes , k = 3)

# resultado_kmodes <- kmodes(interes, modes = 3)

fviz_cluster(resultado_interes, data = interes, geom = "point")

# pca_result <- prcomp(interes)
# 
# # Graficamos los clusters en el espacio de las dos primeras componentes principales
# fviz_cluster(list(data = pca_result$x, cluster = resultado_kmodes$cluster))




# Obtener la asignación de clusters para cada observación
clusters <- resultado_interes$clustering

# Crear dataframes para cada cluster
datos_por_cluster <- lapply(1:max(clusters), function(i) datos_numericos[clusters == i, ])

# Realizar análisis adicional para cada cluster
for (i in 1:length(datos_por_cluster)) {
  cat("Cluster", i, ":\n")
  print(summary(datos_por_cluster[[i]]))
}
>>>>>>> 93f735acf903c8064a2f515293d38c6b736be6da

#Laboratorio 1 Analisis de datos

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
library(proxy)

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

# Funci�n para asignar grupos
asignar_grupos_blood_cells <- function(valor) {
  if (valor == "LOADED" | valor == "TNTC") {
    return(valor)
  } else {
    primer_digito <- substr(valor, 1, 1)  # Extraer el primer d�gito
    ultimo_digito <- substr(valor, nchar(valor), nchar(valor))  # Extraer el �ltimo d�gito
    
    # Verificar si el primer y �ltimo d�gito cumplen los criterios
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
# Estas est�n ordenadas seg�n la frecuencia en que aparecen dentro de un examen.
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
# Se elimina �nica observacion con un dato NA

datos_procesados <- na.omit(datos_procesados)

# -------------------------------------------------
#Primera generaci�n de clusters incluyendo todas lad dimensiones -ID

# Se elimina la columna ID
datos_numericos <- sapply(datos_procesados[, -1] , as.numeric)


# Se calcula el n�mero de clusters a utilizar
nclusters <- fviz_nbclust(datos_numericos, pam, method = "silhouette")

# Visualiza el resultado
print(nclusters)


# Aplicar PAM con 5 clusters
resultado_pam <- pam(datos_numericos , k = 5)

# Se visualizan los cl�ster
fviz_cluster(resultado_pam, data = datos_numericos)


# Obtener la asignaci�n de clusters para cada observaci�n
clusters <- resultado_pam$clustering

# Crear dataframes para cada cluster
datos_por_cluster <- lapply(1:max(clusters), function(i) datos_numericos[clusters == i, ])

# Realizar an�lisis adicional para cada cluster
for (i in 1:length(datos_por_cluster)) {
  cat("Cluster", i, ":\n")
  print(summary(datos_por_cluster[[i]]))
}

#---------------------------------------------------------------------------
# Segunda generaci�n de cl�sters con kmodes

resultado_kmodes <- kmodes(datos_numericos, modes = 5)



pca_result <- prcomp(datos_numericos)

# Graficamos los clusters en el espacio de las dos primeras componentes principales
fviz_cluster(list(data = pca_result$x, cluster = resultado_kmodes$cluster))



# Tercera generaci�n de cl�sters con kproto

resultado_kproto <- kproto(datos_procesados, k = 5)


pca_result <- prcomp(datos_numericos)

fviz_cluster(list(data = pca_result$x, cluster = resultado_kproto$cluster))



# -------------------------------------------------

# Generaci�n de cl�sters con columnas de inter�s


# Se aislan las dimensiones de inter�s

interes <- datos_numericos[, c("Transparency", "Bacteria","Color", "RBC", "WBC",
                               "Diagnosis", "Gender", "Protein", "Amorphous Urates",
                               "Epithelial Cells")]


interes <- datos_numericos[, c("Bacteria", "WBC", "Epithelial Cells", "Glucose",
                               "Protein", "Diagnosis")]


# Utiliza fviz_nbclust para visualizar diferentes criterios
nclusters <- fviz_nbclust(interes, pam, method = "silhouette")

# Visualiza el resultado
print(nclusters)


# Aplicar PAM con 5 clusters
resultado_interes <- pam(interes ,  k = 3)


fviz_cluster(resultado_interes, data = interes, geom = "point")



# Obtener la asignaci�n de clusters para cada observaci�n
clusters <- resultado_interes$clustering

# Crear dataframes para cada cluster
datos_por_cluster <- lapply(1:max(clusters), function(i) datos_numericos[clusters == i, ])

# Realizar an�lisis adicional para cada cluster
for (i in 1:length(datos_por_cluster)) {
  cat("Cluster", i, ":\n")
  print(summary(datos_por_cluster[[i]]))
}
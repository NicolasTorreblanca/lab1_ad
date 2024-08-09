#Laboratorio 4 Analisis de datos

#Bryan Salgado
#Nicolas Torreblanca

#----------------------------------------------------------------------------

#Primero que nada, importamos las librerias

# Importamos las librerías necesarias
library(readr)
library(dplyr)
library(ggplot2)
library(C50)
library(gmodels)
library(fastDummies)

# Leemos el archivo del dataset
datos_historico <- read_csv("urinalysis_tests.csv")

# Procesamos los datos
datos_procesados <- na.omit(datos_historico)

# Variables dicotómicas
datos_procesados$Gender <- ifelse(datos_procesados$Gender == "MALE", 0, 1)
datos_procesados$Diagnosis <- ifelse(datos_procesados$Diagnosis == "NEGATIVE", 0, 1)
datos_procesados$Transparency <- ifelse(datos_procesados$Transparency == "CLEAR",1,0)
datos_procesados$Protein <- ifelse(datos_procesados$Protein == "NEGATIVE",0,1)

# Variables cuantificadoras
datos_procesados$Age <- ifelse(datos_procesados$Age >= 14, 1, 0)

intervalos_pH <- c(5.0,5.5,6.0,6.5,7.0,7.5,8.0, 8.5)
grupos_pH <- c(1, 2, 3, 4, 5, 6, 7)
datos_procesados$pH <- cut(datos_procesados$pH, breaks = intervalos_pH, labels = grupos_pH, right = FALSE)

intervalos_SG <- c(1.000,1.005,1.010,1.015,1.020,1.025,1.030,1.035,1.040)
grupos_SG <- c(1, 2, 3, 4, 5, 6, 7, 8)
datos_procesados$`Specific Gravity` <- cut(datos_procesados$`Specific Gravity`, breaks = intervalos_SG, labels = grupos_SG, right = FALSE)

# Variables categóricas
categorias_colores <- unique(datos_procesados$Color)
numeros_colores <- 0:(length(categorias_colores) - 1)
mapeo_colores <- setNames(numeros_colores, categorias_colores)
datos_procesados$Color <- mapeo_colores[datos_procesados$Color]

categorias_glucose <- c("NEGATIVE", "TRACE", "1+", "2+", "3+", "4+")
numeros_glucosa <- 0:(length(categorias_glucose) - 1)
mapeo_glucosa <- setNames(numeros_glucosa, categorias_glucose)
datos_procesados$Glucose <- mapeo_glucosa[datos_procesados$Glucose]

# Variables ordinales
categorias_ordinales <- c("NONE SEEN","RARE","FEW","OCCASIONAL","MODERATE","PLENTY","LOADED")
numeros_ordinales <- 0:(length(categorias_ordinales) - 1)
mapeo_EC <- setNames(numeros_ordinales, categorias_ordinales)
datos_procesados$`Epithelial Cells` <- mapeo_EC[datos_procesados$`Epithelial Cells`]

# Se elimina la columna ID y se hacen conversiones adicionales
data <- datos_procesados[, -1]

# Agrupaciones de valores
data$Color <- ifelse(data$Color >= 6, 1, 0)
data$Glucose <- ifelse(data$Glucose >= 4, 1, 0)

data$pH <- as.character(data$pH)
data$pH <- as.integer(data$pH)
data$pH <- ifelse(data$pH >= 3, 1, 0)

data$`Specific Gravity` <- as.character(data$`Specific Gravity`)
data$`Specific Gravity` <- as.integer(data$`Specific Gravity`)
data$`Specific Gravity` <- ifelse(data$`Specific Gravity` >= 4, 1, 0)

data$`Epithelial Cells` <- ifelse(data$`Epithelial Cells` >= 3, 1, 0)

# Convertir todo a factores
data[] <- lapply(data, as.factor)

# Se hace el procesamiento one-hot para ciertas variables
dumy <- dummy_cols(data, select_columns = c("WBC", "RBC", "Amorphous Urates", "Bacteria"))
dumy <- lapply(dumy, as.factor)
dumy <- as.data.frame(dumy)

# Dividir los datos en conjunto de entrenamiento y prueba
set.seed(123)
train_sample <- sample(nrow(dumy), 0.8 * nrow(dumy))
df_train <- dumy[train_sample,]
df_test <- dumy[-train_sample,]

# Construcción del modelo C5.0
cmodel <- C5.0(df_train[,-which(names(df_train) == "Diagnosis")], df_train$Diagnosis)
summary(cmodel)

# Evaluación del modelo
cmodel_pred <- predict(cmodel, df_test)

# Validación con tabla cruzada
CrossTable(df_test$Diagnosis, cmodel_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual Diagnosis', 'predicted Diagnosis'))

# Visualización del árbol
plot(cmodel)

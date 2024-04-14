library(readr)
library(dplyr)

datos_historico <- read_csv("urinalysis_tests.csv")

#

datos_procesados <- datos_historico
# Verificar la estructura de los datos

str(datos_procesados)

# Convertir la variable de género a valores numéricos (0 y 1)

datos_procesados$Gender <- ifelse(datos_procesados$Gender == "MALE", 0, 1)
datos_procesados$Diagnosis <- ifelse(datos_procesados$Diagnosis == "NEGATIVE", 0, 1)

# Variables cuantificadoras

# ---------------------------------------------------------------------------------
#Edad

intervalos_edad <- c(0.0,12.0,18.0,30.0,60.0,80.0,199.0)
grupos_edad <- c(1, 2, 3, 4,5,6)

datos_procesados$Age <-cut(datos_procesados$Age, breaks = intervalos_edad, labels = grupos_edad, right = FALSE)

# ---------------------------------------------------------------------------------

#PH

intervalos_pH <- c(5.0,5.5,6.0,6.5,7.0,7.5,8.0)
grupos_pH <- c(1, 2, 3, 4,5,6)

datos_procesados$pH <-cut(datos_procesados$pH, breaks = intervalos_pH, labels = grupos_pH, right = FALSE)

# ---------------------------------------------------------------------------------

#Specific Gravity

intervalos_SG <- c(1.000,1.005,1.010,1.015,1.020,1.025,1.030,1.035,1.040)
grupos_SG <- c(1, 2, 3, 4,5,6,7,8)

datos_procesados$`Specific Gravity` <-cut(datos_procesados$`Specific Gravity`, breaks = intervalos_SG, labels = grupos_SG, right = FALSE)

# ---------------------------------------------------------------------------------

# Por variables Categoricas

# Color

#
#

# ---------------------------------------------------------------------------------
categorias_colores <- unique(datos_procesados$Color)
numeros_colores <- 1:length(categorias_colores)
mapeo_colores <- setNames(numeros_colores, categorias_colores)
datos_procesados$Color <- mapeo_colores[datos_procesados$Color]  

# ---------------------------------------------------------------------------------


#Transparency

#
#

# ---------------------------------------------------------------------------------
categorias_transparencia <- unique(datos_procesados$Transparency)
numeros_transparencia <- 1:length(categorias_transparencia)
mapeo_transparencia <- setNames(numeros_transparencia, categorias_transparencia)
datos_procesados$Transparency <- mapeo_transparencia[datos_procesados$Transparency]  

# ---------------------------------------------------------------------------------


# #Glucose
# 
# ---------------------------------------------------------------------------------
categorias_glucose <- unique(datos_procesados$Glucose)
numeros_glucosa <- 1:length(categorias_glucose)
mapeo_glucosa <- setNames(numeros_glucosa, categorias_glucose)
datos_procesados$Glucose <- mapeo_glucosa[datos_procesados$Glucose]  

# ---------------------------------------------------------------------------------
# # Protein
# 
# #
# #
# 
# # Los Respectivos Números
# 

# ---------------------------------------------------------------------------------
categorias_proteina <- unique(datos_procesados$Protein)
numeros_proteina <- 1:length(categorias_proteina)
mapeo_proteina <- setNames(numeros_proteina, categorias_proteina)
datos_procesados$Protein <- mapeo_glucosa[datos_procesados$Protein]  

# ---------------------------------------------------------------------------------



# ---------------------------------------------------------------------------------
# # WBC
# 
# #
# #
# 
# # Los Respectivos Números
# 

# ---------------------------------------------------------------------------------
categorias_WBC <- unique(datos_procesados$WBC)
numeros_wbc <- 1:length(categorias_WBC)
mapeo_wbc <- setNames(numeros_wbc, categorias_WBC)
datos_procesados$WBC <- mapeo_wbc[datos_procesados$WBC]  

# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
# # RBC
# 
# #
# #
# ---------------------------------------------------------------------------------
categorias_RBC <- unique(datos_procesados$RBC)
numeros_RBC <- 1:length(categorias_RBC)
mapeo_RBC <- setNames(numeros_RBC, categorias_RBC)
datos_procesados$RBC <- mapeo_RBC[datos_procesados$RBC]  
# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
# # Epitelial Cells
# 
# #
# #
# ---------------------------------------------------------------------------------
categorias_EC <- unique(datos_procesados$`Epithelial Cells`)
numeros_EC <- 1:length(categorias_EC)
mapeo_EC <- setNames(numeros_EC, categorias_EC)
datos_procesados$`Epithelial Cells` <- mapeo_EC[datos_procesados$`Epithelial Cells`]  

# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
# # Mucous Threads
# 
# #
# #
# ---------------------------------------------------------------------------------
categorias_MT <- unique(datos_procesados$`Mucous Threads`)
numeros_MT <- 1:length(categorias_MT)
mapeo_MT <- setNames(numeros_MT, categorias_MT)
datos_procesados$`Mucous Threads` <- mapeo_MT[datos_procesados$`Mucous Threads`]  

# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
# # Amorphous Urates
# 
# #
# #
# ---------------------------------------------------------------------------------
categorias_AU <- unique(datos_procesados$`Amorphous Urates`)
numeros_AU <- 1:length(categorias_AU)
mapeo_AU <- setNames(numeros_AU, categorias_AU)
datos_procesados$`Amorphous Urates` <- mapeo_AU[datos_procesados$`Amorphous Urates`]  

# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
# # Bacteria
# 
# #
# #
# ---------------------------------------------------------------------------------
categorias_Bacteria <- unique(datos_procesados$Bacteria)
numeros_Bacteria <- 1:length(categorias_Bacteria)
mapeo_Bacteria <- setNames(numeros_Bacteria, categorias_Bacteria)
datos_procesados$Bacteria <- mapeo_Bacteria[datos_procesados$Bacteria]  

# ---------------------------------------------------------------------------------
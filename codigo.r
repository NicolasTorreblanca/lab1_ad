library(readr)
library(dplyr)
library(ggplot2)

datos_historico <- read_csv("urinalysis_tests.csv")

#

datos_procesados <- datos_historico
# Verificar la estructura de los datos

str(datos_procesados)

datos_procesados$Gender <- ifelse(datos_procesados$Gender == "MALE", 0, 1)
datos_procesados$Diagnosis <- ifelse(datos_procesados$Diagnosis == "NEGATIVE", 0, 1)
datos_procesados$Transparency <- ifelse(datos_procesados$Transparency == "CLEAR",1,0)
datos_procesados$Protein <- ifelse(datos_procesados$Protein == "NEGATIVE",0,1)

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



# #Glucose
# 
# ---------------------------------------------------------------------------------
categorias_glucose <- unique(datos_procesados$Glucose)
numeros_glucosa <- 1:length(categorias_glucose)
mapeo_glucosa <- setNames(numeros_glucosa, categorias_glucose)
datos_procesados$Glucose <- mapeo_glucosa[datos_procesados$Glucose]  



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


#Ordinales

categorias_ordinales <- c("NONE SEEN","RARE","FEW","OCCASIONAL","MODERATE","PLENTY","LOADED")
numeros_ordinales <- 1:length(categorias_ordinales)

# ---------------------------------------------------------------------------------
# # Epitelial Cells

mapeo_EC <- setNames(numeros_ordinales, categorias_ordinales)
datos_procesados$`Epithelial Cells` <- mapeo_EC[datos_procesados$`Epithelial Cells`]  


# # Mucous Threads

mapeo_MT <- setNames(numeros_ordinales, categorias_ordinales)
datos_procesados$`Mucous Threads` <- mapeo_MT[datos_procesados$`Mucous Threads`]  

# # Amorphous Urates

mapeo_AU <- setNames(numeros_ordinales, categorias_ordinales)
datos_procesados$`Amorphous Urates` <- mapeo_AU[datos_procesados$`Amorphous Urates`]  

# # Bacteria

mapeo_Bacteria <- setNames(numeros_ordinales, categorias_ordinales)
datos_procesados$Bacteria <- mapeo_Bacteria[datos_procesados$Bacteria]  

# ---------------------------------------------------------------------------------
#Histogramas datos procesados

datos_procesados_dicotomicos <- select(datos_procesados,Gender,Transparency,Protein,Diagnosis)
  
datos_procesados_dicotomicos_positivos <- filter( datos_procesados_dicotomicos,Diagnosis== 1)
datos_procesados_dicotomicos_positivos <- subset(datos_procesados_dicotomicos_positivos,select = -Diagnosis)

datos_procesados_dicotomicos_negativos <- filter( datos_procesados_dicotomicos,Diagnosis== 0)
datos_procesados_dicotomicos_negativos <- subset(datos_procesados_dicotomicos_negativos,select = -Diagnosis)

datos_procesados_ordinales <- select(datos_procesados,`Epithelial Cells`,`Mucous Threads`,`Amorphous Urates`,Bacteria,Diagnosis)

datos_procesados_ordinales_positivos <-  filter( datos_procesados_ordinales,Diagnosis== 1)
datos_procesados_ordinales_positivos <-  subset(datos_procesados_ordinales_positivos, select = -Diagnosis)

datos_procesados_ordinales_negativos <-  filter( datos_procesados_ordinales,Diagnosis== 0)
datos_procesados_ordinales_negativos <-  subset(datos_procesados_ordinales_negativos, select = -Diagnosis)


datos_dic_pos <- tidyr::gather(datos_procesados_dicotomicos_positivos, key = "Variable", value = "Valor")
ggplot(datos_dic_pos, aes(x = Valor, fill = Variable)) +
  geom_bar(position = "dodge", color = "black") +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Gráficos de barras de las variables", x = "Valor", y = "Cantidad") +
  theme_minimal()


datos_dic_pos <- tidyr::gather(datos_procesados_dicotomicos_negativos, key = "Variable", value = "Valor")
ggplot(datos_dic_pos, aes(x = Valor, fill = Variable)) +
  geom_bar(position = "dodge", color = "black") +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Gráficos de barras de las variables", x = "Valor",y = "Cantidad") +
  theme_minimal()



datos_ord_pos <- tidyr::gather(datos_procesados_ordinales_positivos, key = "Variable", value = "Valor")
ggplot(datos_ord_pos, aes(x = Valor, fill = Variable)) +
  geom_bar(position = "dodge", color = "black") +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Gráficos de barras de las variables", x = "Valor",y = "Cantidad") +
  theme_minimal()


datos_ord_neg <- tidyr::gather(datos_procesados_ordinales_negativos, key = "Variable", value = "Valor")
ggplot(datos_ord_neg, aes(x = Valor, fill = Variable)) +
  geom_bar(position = "dodge", color = "black") +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Gráficos de barras de las variables", x = "Valor",y = "Cantidad") +
  theme_minimal()

# ---------------------------------------------------------------------------------

# Crear un nuevo dataframe para almacenar los valores modificados
datos_num <- datos_historico

# Modificar los valores de la columna en el nuevo dataframe
datos_num$Age[datos_num$Age < 1] <- (datos_num$Age[datos_num$Age < 1] * 100) / 12

datos_num <- subset(datos_num, select = -ID)
datos_num <- subset(datos_num, select = -Gender)
datos_num <- subset(datos_num, select = -Color)
datos_num <- subset(datos_num, select = -Transparency)
datos_num <- subset(datos_num, select = -Glucose)
datos_num <- subset(datos_num, select = -Protein)
datos_num <- subset(datos_num, select = -WBC)
datos_num <- subset(datos_num, select = -RBC)
datos_num <- datos_num[, !colnames(datos_num) %in% c("Epithelial Cells")]
datos_num <- datos_num[, !colnames(datos_num) %in% c("Mucous Threads")]
datos_num <- datos_num[, !colnames(datos_num) %in% c("Amorphous Urates")]
datos_num <- subset(datos_num, select = -Bacteria)

# Crear un dataframe con las filas donde Diagnosis es "NEGATIVE"
datos_negative <- subset(datos_num, Diagnosis == "NEGATIVE")
datos_negative <- subset(datos_negative, select = -Diagnosis)

# Crear un dataframe con las filas donde Diagnosis es "POSITIVE"
datos_positive <- subset(datos_num, Diagnosis == "POSITIVE")
datos_positive <- subset(datos_positive, select = -Diagnosis)


###Datos positivos

summary_pos <- summary(datos_positive)
summary_pos

# Calcular la desviación estándar de las columnas numéricas en el dataframe
desviacion_estandar <- apply(datos_positive, 2, sd)

# Calcular el rango de las columnas numéricas en el dataframe
rango <- apply(datos_positive, 2, function(x) diff(range(x)))

# Calcular el rango intercuartílico de las columnas numéricas en el dataframe
rango_intercuartilico <- apply(datos_positive, 2, IQR)

# Imprimir los resultados
print("Desviación Estándar:")
print(desviacion_estandar)
print("Rango:")
print(rango)
print("Rango Intercuartílico:")
print(rango_intercuartilico)

###Datos negativos

summary_neg <- summary(datos_negative)
summary_neg

# Calcular la desviación estándar de las columnas numéricas en el dataframe
desviacion_estandar <- apply(datos_negative, 2, sd)

# Calcular el rango de las columnas numéricas en el dataframe
rango <- apply(datos_negative, 2, function(x) diff(range(x)))

# Calcular el rango intercuartílico de las columnas numéricas en el dataframe
rango_intercuartilico <- apply(datos_negative, 2, IQR)

# Imprimir los resultados
print("Desviación Estándar:")
print(desviacion_estandar)
print("Rango:")
print(rango)
print("Rango Intercuartílico:")
print(rango_intercuartilico)

# Obtener todas las edades presentes en ambos dataframes
all_ages <- unique(c(datos_positive$Age, datos_negative$Age))

# Calcular las frecuencias relativas para cada dataframe
freq_rel_pos <- table(factor(datos_positive$Age, levels = all_ages)) / nrow(datos_positive)
freq_rel_neg <- table(factor(datos_negative$Age, levels = all_ages)) / nrow(datos_negative)

# Crear un dataframe con las frecuencias relativas de ambas datasets
frecuencias_rel <- data.frame(
  Age = as.numeric(as.character(names(freq_rel_pos))), 
  Positive = as.numeric(freq_rel_pos),
  Negative = as.numeric(freq_rel_neg)
)

# Graficar el diagrama comparativo de frecuencias relativas

ggplot(frecuencias_rel, aes(x=Age)) +
  geom_line(aes(y=Positive, color="Positive")) +
  geom_line(aes(y=Negative, color="Negative")) +
  scale_color_manual(values=c("blue", "red")) +
  labs(title="Comparación de Frecuencias Relativas de Age",
       x="Age", y="Frecuencia Relativa") +
  theme_minimal()

# Obtener todos los valores de pH presentes en ambos dataframes
all_pH <- unique(c(datos_positive$pH, datos_negative$pH))

# Calcular las frecuencias relativas para cada dataframe
freq_rel_pos <- table(factor(datos_positive$pH, levels = all_pH)) / nrow(datos_positive)
freq_rel_neg <- table(factor(datos_negative$pH, levels = all_pH)) / nrow(datos_negative)

# Crear un dataframe con las frecuencias relativas de ambas datasets
frecuencias_rel_pH <- data.frame(
  pH = as.numeric(as.character(names(freq_rel_pos))), 
  Positive = as.numeric(freq_rel_pos),
  Negative = as.numeric(freq_rel_neg)
)

# Graficar el diagrama comparativo de frecuencias relativas
library(ggplot2)
ggplot(frecuencias_rel_pH, aes(x=pH)) +
  geom_line(aes(y=Positive, color="Positive")) +
  geom_line(aes(y=Negative, color="Negative")) +
  scale_color_manual(values=c("blue", "red")) +
  labs(title="Comparación de Frecuencias Relativas de pH",
       x="pH", y="Frecuencia Relativa") +
  theme_minimal()

# Obtener todos los valores de Specific Gravity presentes en ambos dataframes
all_specific_gravity <- unique(c(datos_positive$`Specific Gravity`, datos_negative$`Specific Gravity`))

# Calcular las frecuencias relativas para cada dataframe
freq_rel_pos <- table(factor(datos_positive$`Specific Gravity`, levels = all_specific_gravity)) / nrow(datos_positive)
freq_rel_neg <- table(factor(datos_negative$`Specific Gravity`, levels = all_specific_gravity)) / nrow(datos_negative)

# Crear un dataframe con las frecuencias relativas de ambas datasets
frecuencias_rel_specific_gravity <- data.frame(
  Specific_Gravity = as.numeric(as.character(names(freq_rel_pos))), 
  Positive = as.numeric(freq_rel_pos),
  Negative = as.numeric(freq_rel_neg)
)

# Graficar el diagrama comparativo de frecuencias relativas
library(ggplot2)
ggplot(frecuencias_rel_specific_gravity, aes(x=Specific_Gravity)) +
  geom_line(aes(y=Positive, color="Positive")) +
  geom_line(aes(y=Negative, color="Negative")) +
  scale_color_manual(values=c("blue", "red")) +
  labs(title="Comparación de Frecuencias Relativas de Specific Gravity",
       x="Specific Gravity", y="Frecuencia Relativa") +
  theme_minimal()

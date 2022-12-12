# PostWork Sesion 3: Análisis Exploratorio de Datos
"
Utilizando el dataframe boxp.csv realiza el siguiente análisis descriptivo. 
No olvides excluir los missing values y transformar las variables a su tipo y escala correspondiente.
"
library(dplyr)
library(DescTools) #para la funcion moda Mode

datos <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/boxp.csv")
str(datos)
class(datos)
summary(datos)

# se deben limpiar los datos para quitar los NA
datos <- datos[complete.cases(datos),]

#se establecen factores para categoria y grupo para que pueda contarse
datos$Categoria <- factor(datos$Categoria)
datos$Grupo <- factor(datos$Grupo, labels = c("cero","uno"))

#1) Calcula e interpreta las medidas de tendencia central de la variable Mediciones
#dado que la moda < mediana < media, existe un sesgo a la derecha
Mode(datos$Mediciones);median(datos$Mediciones);mean(datos$Mediciones)

#Con base en tu resultado anteior, ¿qué se puede concluir respecto al sesgo de Mediciones?
# que hay un sesgo a la derecha

#Calcula e interpreta la desviación estándar y los cuartiles de la distribución de Mediciones
sd(datos$Mediciones) # las mediciones tienen una dispersión un poco cerca de la media 53.76972
cuartiles <- quantile(datos$Mediciones,probs = c(0.25,0.5,0.75))
cuartiles

# 25% de las mediciones tienen un valor de 23.45 o menos
# 50% de las mediciones tienen un valor de 49.30 o menos
# 75% de las mediciones tienen un valor de 82.85 o menos

#Con ggplot, realiza un histograma separando la distribución de Mediciones por Categoría ¿Consideras que sólo una categoría está generando el sesgo?
histograma <- hist(datos$Mediciones, main = "Histograma de mediciones")
?hist

#Con ggplot, realiza un boxplot separando la distribución de Mediciones por Categoría y por Grupo dentro de cada categoría. 
#¿Consideras que hay diferencias entre categorías? ¿Los grupos al interior de cada categoría podrían estar generando el sesgo?
boxplot(datos$Mediciones,horizontal=TRUE)


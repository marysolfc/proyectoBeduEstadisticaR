# PostWork sesion 8

#datos a utilizar
df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")

# =====| 1. Plantea el problema del caso |===========
"

En este estudio se analizan los determinantes socioeconómicos de la inserguridad alimentaria en México
Para ello se analizarán los datos por medio de una regresión lineal con el método de mínimos cuadrados ordinarios (OLS) y con ello
establecer o determinar la relación existente entre el nivel socioeconómico y el gasto en productos no saludables ya que los datos sugieren 
que los hogares con menor nivel socioeconómico tienden a gastar más en productos no saludables que 
las personas con mayores niveles socioeconómicos y que esto, entre otros determinantes, lleva a que un hogar presente 
cierta inseguridad alimentaria.

"

# para comenzar con el estudio, se realiza una exploración de los datos
class(df)
head(df)
str(df)

#Se observa que hay valores NA por lo que aplicamos la función complete.cases para eliminarlos
df <- df[complete.cases(df),]
str(df)
summary(df)

"Para realizar el análisis descriptivo de la información, comenzamos con visualizar los datos contenidos en el dataframe
observamos que existen algunas variables cualitativas por lo que las hemos transformado en factores para poder realizar
un resumen sobre todas las variables
"
df$nse5f <- factor(df$nse5f, labels=c("Bajo","Medio Bajo","Medio","Medio Alto","Alto"), ordered=TRUE)
df$area <- factor(df$area,labels = c("Zona Urbana","Zona Rural"), ordered=TRUE)
df$sexojef <- factor(df$sexojef, labels = c("Hombre","Mujer"), ordered=TRUE)
df$IA <- factor(df$IA, labels = c( "No presenta IA", "Presenta IA"), ordered=TRUE)
df$refin <- factor(df$refin, labels = c( "No", "Si"), ordered=TRUE)

summary(df)

# ======| 2. Realiza un análisis descriptivo de la información |===============

#Ahora que se tienen los datos con valores numéricos podemos obtener un resumen de los mismos con la función summary
summary(df$ln_als)
summary(df$ln_alns)

#Dado que las variables ln_als y ln_alns están en logaritmo, necesitamos aplicar la función exponencial para obtenerlo el valor de la media, mediana, moda
gasto.als <- exp(df$ln_als)
gasto.alns <- exp(df$ln_alns)
summary(gasto.als)
summary(gasto.alns)

media.als <- mean(gasto.als)
media.alns <- mean(gasto.alns)
mediana.als <- median(gasto.als)
mediana.alns <- median(gasto.alns)
library(DescTools)
moda.als <- Mode(gasto.als)[1]
moda.alns <- Mode(gasto.alns)[1]

moda.als;mediana.als;media.als
"[1] 550
[1] 530.5
[1] 593.989"
sd.als <- sd(gasto.als) # las mediciones tienen una dispersión de 359.0136 de la media 593.989

moda.alns;mediana.alns;media.alns
"[1] 30
[1] 55
[1] 107.8948"

sd.alns <- sd(gasto.alns) # las mediciones tienen una dispersión 145.7636 un poco cerca de la media 107.8948

cuartiles.als <- quantile(gasto.als,probs = c(0.25,0.5,0.75))
cuartiles.als
# 25% del gasto en alimentos saludables tiene un valor de 345 o menos
# 50% del gasto en alimentos saludables tiene un valor de 530.5 o menos
# 75% del gasto en alimentos saludables tiene un valor de 760 o menos

cuartiles.alns <- quantile(gasto.alns,probs = c(0.25,0.5,0.75))
cuartiles.alns
# 25% del gasto en alimentos NO saludables tiene un valor de 30 o menos
# 50% del gasto en alimentos NO saludables tiene un valor de 55 o menos
# 75% del gasto en alimentos NO saludables tiene un valor de 130 o menos

# Finalmente visualizamos con una gráfica el comportamiento del gasto en alimentos saludables y no saludables
par(mfrow = c(1,2))

hist(gasto.als, main = "Distribución con sesgo a la derecha", xlab = "Gasto alimentos saludables") #moda < mediana < media
hist(gasto.alns, main = "Distribución con sesgo a la derecha", xlab = "Gasto alimentos NO saludables") #moda < mediana < media

dev.off()


# =======| 3. Calcula probabilidades que nos permitan entender el problema en México |==================
"Se realiza una gráfica para ver el comportamiento de la distribución de los datos del gasto en alimentos saludables y no saludables"

par(mfrow = c(1,2))

curve(dnorm(x, mean = mean(df$ln_als), sd = sd(df$ln_als)),from=2, to=10,col='blue', main = "Distribución Normal",
      ylab = "f(x)", xlab = "Gasto als en ln")

curve(dnorm(x, mean = media.als, sd = sd.als),from=-1000, to=2000,col='green', main = "Distribución Normal",
      ylab = "f(x)", xlab = "Gasto als")
dev.off()

### Estas probabilidades se quedaron propuestas, falta resolver!!!

"Qué probabilidad hay de que una familia gaste en alimentos (saludables y no saludables) en un rango de 400 y 800"

"Qué probabilidad hay de una familia de nivel medio gaste mas de 200 en alimentos no saludables "

"Qué probabilidad hay de que una familia de nivel bajo gaste en alimentos saludables"

"Qué probabilidad hay que una familia de la zona rural presente IA"

"Qué probabilidad hay que una familia de zona urbana presente IA"



# =========| 4. Plantea hipótesis estadísticas y concluye sobre ellas para entender el problema en México |===============

"
El estudio, señala que, el promedio del gasto en alimentos saludables es mayor  que el gasto en alimentos NO saludables
A un NC del 90%, ¿EEE para concluir que eso sucede en nuestro estudio?

Planteamiento de hipótesis:
Ho: prom_gasto_als <= prom_gasto_alns 
Ha: prom_gasto_als > prom_gasto_alns"

# Si las varianzas son iguales v1/v2 = 1 por eso el ratio es 1
#prueba de varianzas
head(df[df$ln_als,9])
#df[df$ln_als,9]
var.test(df[df$ln_als, 9],
         df[df$ln_alns,10], 
         ratio = 1, alternative = "two.sided")

# pvalue > nivel significancia por lo tanto se rechazar que lass varianzas son iguales

t.test(x = exp(df[df$ln_als,9]), y = exp(df[df$ln_alns,10]),
       alternative = "greater",
       mu = 0, var.equal = FALSE)

# con p-value < 2.2e-16
#Conclusión: A niveles de confianza estadar, EEE para rechazar la Ho, el promedio de 
# gasto en alimentos saludables es menor o igual que el gasto en alimentos no saludables



"Ho: las familias de niveles socioeconómicos bajos gastan más en alimentos no saludables que las familias de niveles socioeconómicos más altos
Ha: !Ho
"

# ========| 5. Estima un modelo de regresión, lineal o logístico, para identificiar los determinantes de la inseguridad alimentaria en México |====
#Y = Bo + B1X + B2X2
library(dplyr)

df.select <- select(df,ln_als,numpeho, edadjef, añosedu)

# se muestra la matriz de correlación de las variables cuantitativas
round(cor(df.select),4)  

# se muestra una matriz con las gráficas de dispersión 
pairs(~ gasto.als + numpeho + edadjef + añosedu, 
      data = df, gap = 0.4, cex.labels = 1.5)

"pairs(~ ln_als + numpeho + edadjef + añosedu, 
      data = df, gap = 0.4, cex.labels = 1.5)"

pairs(~ gasto.als + numpeho + edadjef + añosedu + nse5f + area + refin + sexojef + IA, 
data = df, gap = 0.4, cex.labels = 1.5)

"Estimación por Mínimos Cuadrados Ordinarios (OLS)
gasto.als = beta0 + beta1*numpeho + beta2*edadjef + beta3*añosedu  + e
gasto.als = beta0 + beta1*numpeho + beta2*edadjef + beta3*añosedu + beta4*nse5f + beta5*area + beta6*refin + beta7*sexojef + beat8*IA + e"

attach(df) # toma todas las variables del dataframe y los enmascara para acceder a las variables solo nombrandolas
#m1 <- lm(gasto.als ~ numpeho + edadjef + añosedu) #funcion lm lineal model
#summary(m1)
"Nota: hace falta realizar el estudio tomando en cuenta todas las variables y comparar."
m1 <- lm(ln_als ~ numpeho + edadjef + añosedu + nse5f + area + refin + sexojef + IA) 
summary(m1)


#A un nivel de confianza del 99%
#se observa que el coeficiente de la variable edadjef no es significativo ya que tiene un p-value = 0.01 
#Probemos nuestro modelo sin incluir dicha variable:
#gasto.als = beta0 + beta1*numpeho + beta3*añosedu + e
#m2 <- update(m1, ~.-edadjef)
#summary(m2)

m2 <- update(m1, ~.-edadjef - nse5f - sexojef)
summary(m2)

# TÉRMINOS DE INTERACCIÓN
"Para evaluar efectos cruzados, se tomarán las dos variables del modelo"
# gasto.als = beta0 + beta1*numpeho + beta3*añosedu + e

mfull <- lm(ln_als ~ numpeho + añosedu + numpeho:añosedu)

summary(mfull)

"Ahora debemos evaluar la significancia global del modelo, es decir, 
podemos comparar un modelo tomando en cuenta todos los efectos cruzados y compararlo 
contra otro modelo sin efectos cruzados.

Para ello, planteamos el siguiente juego de hipótesis:
H0: beta2 = 0
(gasto.als = beta0 + beta1*numpeho + beta3*añosedu + e)

H1: H0 no es verdad (gasto.als = beta0 + beta1*numpeho + beta2*edadjef + beta3*añosedu + e)

Para este tipo de inferencia usamos el enfoque de análisis de varianza (ANOVA), 
ya que estamos comparando la variabilidad de un modelo no restringido contra la 
variabilidad de un modelo restringido."

anova(m2,mfull)

# Como el p-value es 0.6642 entonces no rechazamos la hipótesis nula
# por lo tanto gasto.als = beta0 + beta1*numpeho + beta3*añosedu + e


# Supuestos de la regresión lineal
"El modelo de regresión lineal clásico establece ciertos supuestos en el término 
de error:
1) Eltérmino de error no tiene correlación significativa con las variables 
explicativas. En caso contrario, tendríamos un problema de endogeneidad.
2) El término de error sigue una distribución normal"

#Se tomaran los residuos estandarizados

StanRes2 <- rstandard(m2)
par(mfrow = c(2, 2))
plot(numpeho, StanRes2, ylab = "Residuales Estandarizados")
plot(añosedu, StanRes2, ylab = "Residuales Estandarizados")

# Busca evidencia para soportar la hipótesis de normalidad en los errores 
#compara los quantiles de la distribucion normal con los quantiles de los residuos
qqnorm(StanRes2)
qqline(StanRes2)

dev.off()

# contrasta si la distribucion muestral de la variables se asemeja a la distribucion normal
shapiro.test(StanRes2)


#Se vuelven a tomar los residuos estandarizados, pero ahora del modeloFull

StanRes <- rstandard(mfull)
par(mfrow = c(2, 2))
plot(numpeho, StanRes, ylab = "Residuales Estandarizados")
plot(añosedu, StanRes, ylab = "Residuales Estandarizados")

# Busca evidencia para soportar la hipótesis de normalidad en los errores 

qqnorm(StanRes)
qqline(StanRes)

dev.off()

"Conclusión:
Según este modelo de regresión lineal, el número de personas en el hogar, los años de educación y la zona geográfica
determinan el gasto en alimentos saludables

"


# =====| 6. Escribe tu análisis en un archivo README.MD y tu código en un script de R y publica ambos en un repositorio de Github. |===========
"
Este estudio ha analizado cuantitativamente los determinantes socioeconómicos del gasto en alimentos saludables en México.
Los resultados sugieren que las familias tienen un nivel educativo y un poder adquisitivo considerablemente más alto que la media nacional. 
El análisis econométrico sugiere que el gasto en alimentos saludables está positivamente correlacionado con 
el número de años de educación, la zona geográfica y el número de personas en el hogar, lo que refleja que 
1) el consumo de alimentos saludables es restringido para los hogares pobres, y 
2) los individuos con mayor educación tienen mayor acceso a información sobre los beneficios que conlleva el consumo de productos 
saludables y los perjuicios que acarrea el consumo de alimentos no saludables. 

"


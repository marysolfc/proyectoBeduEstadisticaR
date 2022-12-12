#Postwork sesión 6: Regresión lineal y clasificación.
"Supongamos que nuestro trabajo consiste en aconsejar a un cliente sobre como mejorar las ventas de un producto particular, y el 
conjunto de datos con el que disponemos son datos de publicidad que consisten en las ventas de aquel producto en 200 diferentes mercados, 
junto con presupuestos de publicidad para el producto en cada uno de aquellos mercados para tres medios de comunicación diferentes:
TV, radio, y periódico. No es posible para nuestro cliente incrementar directamente las ventas del producto. Por otro lado, 
ellos pueden controlar el gasto en publicidad para cada uno de los tres medios de comunicación. Por lo tanto, si determinamos que 
hay una asociación entre publicidad y ventas, entonces podemos instruir a nuestro cliente para que ajuste los presupuestos de publicidad,
y así indirectamente incrementar las ventas.

En otras palabras, nuestro objetivo es desarrollar un modelo preciso que pueda ser usado para predecir las ventas sobre la base de los 
tres presupuestos de medios de comunicación. Ajuste modelos de regresión lineal múltiple a los datos advertisement.csv y elija el modelo 
más adecuado siguiendo los procedimientos vistos

Considera:

Y: Sales (Ventas de un producto)
X1: TV (Presupuesto de publicidad en TV para el producto)
X2: Radio (Presupuesto de publicidad en Radio para el producto)
X3: Newspaper (Presupuesto de publicidad en Periódico para el producto)
"

adv <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-06/data/advertising.csv")
str(adv)

# se muestra la matriz de correlación de las variables
round(cor(adv),4)  

# se muestra una matriz con las gráficas de dispersión 
pairs(~ Sales + TV + Radio + Newspaper, 
      data = adv, gap = 0.4, cex.labels = 1.5)

"Estimación por Mínimos Cuadrados Ordinarios (OLS)
Sales = beta0 + beta1*TV + beta2*Radio + beta3*Newspapeer + e"
attach(adv) # toma todas las variables del dataframe y los enmascara para acceder a las variables solo nombrandolas en lugar de df$Price
m1 <- lm(Sales ~ TV + Radio + Newspaper) #funcion lm lineal model
summary(m1)

#se observa que el coeficiente de la variable Newspaper no es significativo ya que tiene un p-value = 0.954 
#Probemos nuestro modelo sin incluir dicha variable:
#Sales = beta0 + beta1*TV + beta2*Radio + e"
m2 <- update(m1, ~.-Newspaper)
summary(m2)

"El modelo de regresión lineal clásico establece ciertos supuestos en el término 
de error:
1) Eltérmino de error no tiene correlación significativa con las variables 
explicativas. En caso contrario, tendríamos un problema de endogeneidad.
2) El término de error sigue una distribución normal"

#Se tomaran los residuos estandarizados

StanRes2 <- rstandard(m2)
par(mfrow = c(2, 2))
plot(TV, StanRes2, ylab = "Residuales Estandarizados")
plot(Radio, StanRes2, ylab = "Residuales Estandarizados")

# Busca evidencia para soportar la hipótesis de normalidad en los errores 
qqnorm(StanRes2)
qqline(StanRes2)

dev.off()

# contrasta si la distribucion muestral de la variables se asemeja a la distribucion normal
shapiro.test(StanRes2)


# TÉRMINOS DE INTERACCIÓN
"Para evaluar efectos cruzados, se tomarán las dos variables del modelo"
# Sales = beta0 + beta1*TV + beta2*Radio +  beta3*TV*Radio + e

mfull <- lm(Sales ~ TV + Radio + TV:Radio)

summary(mfull)

"Ahora debemos evaluar la significancia global del modelo, es decir, 
podemos comparar un modelo tomando en cuenta todos los efectos cruzados y compararlo 
contra otro modelo sin efectos cruzados.

Para ello, planteamos el siguiente juego de hipótesis:
H0: beta3 = 0
(Sales = beta0 + beta1*TV + beta2*Radio + e)

H1: H0 no es verdad (Sales = beta0 + beta1*TV + beta2*Radio +  beta3*TV*Radio +  e)

Para este tipo de inferencia usamos el enfoque de análisis de varianza (ANOVA), 
ya que estamos comparando la variabilidad de un modelo no restringido contra la 
variabilidad de un modelo restringido."

anova(m2,mfull)

# Como el p-value es aproximadamente 7.633e-07 entonces rechazamos la hipótesis nula
# dando lugar a la hipótesis alternativa
# Sales = beta0 + beta1*TV + beta2*Radio +  beta3*TV*Radio +  e

#Se vuelven a tomar los residuos estandarizados, pero ahora del modeloFull

StanRes <- rstandard(mfull)
par(mfrow = c(2, 2))
plot(TV, StanRes, ylab = "Residuales Estandarizados")
plot(Radio, StanRes, ylab = "Residuales Estandarizados")

# Busca evidencia para soportar la hipótesis de normalidad en los errores 

qqnorm(StanRes)
qqline(StanRes)

dev.off()

# contrasta si la distribucion muestral de la variables se asemeja a la distribucion normal
shapiro.test(StanRes)

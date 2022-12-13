# Proyecto final del curso de Bedu fase 2: programación y estadística con R
## Este repositorio contiene todos los postworks del curso de Bedu fase 2: programación y estadística con R
## PostWork sesion 8

##### Datos a utilizar
`df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")`

#  1. Plantea el problema del caso 

En este estudio se analizan los determinantes socioeconómicos de la inserguridad alimentaria en México
Para ello se analizarán los datos por medio de una regresión lineal con el método de mínimos cuadrados ordinarios (OLS) y con ello
establecer o determinar la relación existente entre el nivel socioeconómico y el gasto en productos no saludables ya que los datos sugieren 
que los hogares con menor nivel socioeconómico tienden a gastar más en productos no saludables que 
las personas con mayores niveles socioeconómicos y que esto, entre otros determinantes, lleva a que un hogar presente 
cierta inseguridad alimentaria.


#### Para comenzar con el estudio, se realiza una exploración de los datos
```
class(df)
head(df)
str(df)
```
#### Se observa que hay valores NA por lo que aplicamos la función complete.cases para eliminarlos
```
df <- df[complete.cases(df),]
str(df)
summary(df)
```

Para realizar el análisis descriptivo de la información, comenzamos con visualizar los datos contenidos en el dataframe
observamos que existen algunas variables cualitativas por lo que las hemos transformado en factores para poder realizar
un resumen sobre todas las variables

```
df$nse5f <- factor(df$nse5f, labels=c("Bajo","Medio Bajo","Medio","Medio Alto","Alto"), ordered=TRUE)
df$area <- factor(df$area,labels = c("Zona Urbana","Zona Rural"), ordered=TRUE)
df$sexojef <- factor(df$sexojef, labels = c("Hombre","Mujer"), ordered=TRUE)
df$IA <- factor(df$IA, labels = c( "No presenta IA", "Presenta IA"), ordered=TRUE)
df$refin <- factor(df$refin, labels = c( "No", "Si"), ordered=TRUE)

summary(df)

```
# 2. Realiza un análisis descriptivo de la información

#### Ahora que se tienen los datos con valores numéricos podemos obtener un resumen de los mismos con la función summary
Pero dado que las variables ln_als (logarítmo natural del gasto en alimentos saludables) y ln_alns (logarítmo natural del gasto en alimentos no saludables) están en logarítmo, necesitamos aplicar la función exponencial para obtener
las medidas de tendencia central: media, mediana, moda y la medida de dispersión como la desviación estándar

```
media.als <- mean(gasto.als)
mediana.als <- median(gasto.als)
moda.als <- Mode(gasto.als)[1]
sd.als <- sd(gasto.als)

media.alns <- mean(gasto.alns)
mediana.alns <- median(gasto.alns)
moda.alns <- Mode(gasto.alns)[1]
sd.alns <- sd(gasto.alns) 

```
Los resultados que obtenemos para el gasto en alimentos saludables es: 

Moda: 550  
Mediana: 530.5  
Media: 593.989  

Con la desviación estándar 359.0136 se observa que las mediciones están muy concentrados a la media 593.989

Para los datos del gasto en alimentos No saludables

Moda: 30  
Mediana: 55  
Media: 107.8948  

Con desviación estándar de 145.7636 se observa que las mediciones tienen una dispersión cercana a la media 107.8948 

Un dato importante es saber la media del gasto en alimentos saludables de las familias de nivel socieconómico alto

`exp(mean(df[df$nse5f == "Alto", "ln_als"])) `

Lo anterior muestra una media = 689.2283 lo cual indica que es más alta que la media nacional (593.989)

Continuando con el análisis descriptivo obtenemos los cuartiles para los datos del gasto en alimentos saludables

```
cuartiles.als <- quantile(gasto.als,probs = c(0.25,0.5,0.75))
```
Lo cual nos indica que:  
25% del gasto en alimentos saludables tiene un valor de 345 o menos  
50% del gasto en alimentos saludables tiene un valor de 530.5 o menos  
75% del gasto en alimentos saludables tiene un valor de 760 o menos  

Mientras que para el gasto en alimentos no saludables
```
cuartiles.alns <- quantile(gasto.alns,probs = c(0.25,0.5,0.75))
```

Se indica que:  
25% del gasto en alimentos NO saludables tiene un valor de 30 o menos  
50% del gasto en alimentos NO saludables tiene un valor de 55 o menos  
75% del gasto en alimentos NO saludables tiene un valor de 130 o menos  

#### Finalmente visualizamos con una gráfica el comportamiento del gasto en alimentos saludables y no saludables
Se puede observar el sesgo a la derecha en ambos casos

```
hist(gasto.als, main = "Distribución con sesgo a la derecha", xlab = "Gasto alimentos saludables") #moda < mediana < media
hist(gasto.alns, main = "Distribución con sesgo a la derecha", xlab = "Gasto alimentos NO saludables") #moda < mediana < media
```
![histogramas](/images/histogramas.JPG)

# 3. Calcula probabilidades que nos permitan entender el problema en México
Se realiza una gráfica para ver el comportamiento de la distribución de los datos del gasto en alimentos saludables y no saludables.

Como en los histogramas anteriores, los datos distribuyen con una distribución normal, por lo que procedemos a visualizar en una gráfica esa distribución.

```
curve(dnorm(x, mean = mean(df$ln_als), sd = sd(df$ln_als)),from=3, to=9,col='blue', main = "Distribución normal alimentos saludables",
      ylab = "f(x)", xlab = "Gasto als en ln")

curve(dnorm(x, mean = mean(df$ln_alns), sd = mean(df$ln_alns)),from=-10, to=20,col='red', main = "Distribución normal alimentos no saludables",
      ylab = "f(x)", xlab = "Gasto alns en ln")

```
![distribucion](/images/distribuciones.JPG)

Con lo anterior nos planteamos las siguientes preguntas
### Qué probabilidad hay de que una familia gaste en alimentos saludables en un rango de 400 y 800
```
pnorm(q=800,media.als, sd = sd.als) - pnorm(q=400,media.als, sd = sd.als)
```
Conclusión: la probabilidad sería de 42.25% y se puede observar en la gráfica
![probabilidad](/images/probabilidad.JPG)

### Con una probabilidad del 30% cuál es el máximo gasto en alimentos saludables que haría una familia de nivel bajo
```
media.nbajo <- exp(mean(df[df$nse5f == "Bajo", "ln_als"]))
sd.nbajo <- exp(sd(df[df$nse5f == "Bajo", "ln_als"]))

qnorm(p = .3, mean = media.nbajo, sd = sd.nbajo, lower.tail = T)
```
#### Conclusión: con una probabilidad del 30%, se gastaría un máximo de 329.702 en alimento saludables


# 4. Plantea hipótesis estadísticas y concluye sobre ellas para entender el problema en México

### El estudio, señala que, el promedio del gasto en alimentos saludables es mayor  que el gasto en alimentos NO saludables
#### A un NC del 90%, ¿EEE para concluir que eso sucede en nuestro estudio?

Con lo anterior hacemos el siguiente:  

Planteamiento de hipótesis:  
Ho: prom_gasto_als <= prom_gasto_alns  
Ha: prom_gasto_als > prom_gasto_alns  

Si las varianzas son iguales v1/v2 = 1 por eso el ratio es 1
Para comprobar, realizamos una prueba de varianzas con el planteamiento de hipótesis  
Ho: v1/v2 = 1 
Ha: v1/v2 != 1

```
var.test(df[df$ln_als, 9],
         df[df$ln_alns,10], 
         ratio = 1, alternative = "two.sided")
```

Como resultado obtenemos pvalue < nivel significancia por lo tanto se rechaza que las varianzas son iguales, con lo que se puede realizar una prueba t-Student
para mostrar evidencia sobre la hipótesis señalada en un principio

```
t.test(x = exp(df[df$ln_als,9]), y = exp(df[df$ln_alns,10]),
       alternative = "greater",
       mu = 0, var.equal = FALSE)
```
Con p-value < 2.2e-16
Conclusión: A niveles de confianza estadar, EEE para rechazar la Ho, en favor de la alternativa, es decir el promedio del gasto en alimentos saludables es 
mayor que el gasto en alimentos no saludables

También planteamos las siguientes hipótesis:  
Ho: las familias de niveles socieconómicos bajos gastan mas o igual en alimentos saludables que los de niveles altos  
Ha: las familias de niveles socioeconómicos bajos gastan menos en alimentos saludables que las familias de niveles  socioeconómicos más altos
```
t.test(df[df$nse5f == "Bajo", "ln_als"],
       df[df$nse5f == "Alto", "ln_als"],
       alternative = "less", mu = media.alns, var.equal = TRUE)
```

Con p-value < 2.2e-16, podemos concluir que:  
A niveles de confianza estadar, EEE para rechazar la Ho, en favor de la alternativa, es decir las familias de niveles socioeconómicos 
bajos gastan menos en alimentos saludables que las familias de niveles socioeconómicos más altos


# 5. Estima un modelo de regresión, lineal o logístico, para identificiar los determinantes de la inseguridad alimentaria en México
Comenzamos por visualizar una matriz de correlación de las variables cuantitativas

```
df.select <- select(df,ln_als,numpeho, edadjef, añosedu)
round(cor(df.select),4)  
```

 Se muestra una matriz con las gráficas de dispersión 
```
pairs(~ gasto.als + numpeho + edadjef + añosedu, 
      data = df, gap = 0.4, cex.labels = 1.5)
```

### Estimación por Mínimos Cuadrados Ordinarios (OLS)
Se propone el siguiente modelo de regresión lineal con todas las variables
gasto.als = beta0 + beta1\*numpeho + beta2\*edadjef + beta3\*añosedu + beta4\*nse5f + beta5\*area + beta6\*refin + beta7\*sexojef + beat8\*IA + e"

```
m1 <- lm(gasto.als ~ numpeho + edadjef + añosedu + nse5f + area + refin + sexojef + IA) 
summary(m1)
```
A un nivel de confianza del 99%
Se observa que el coeficiente de la variable edadjef, nse5f y sexojef no son significativos ya que tiene un p-value mayores que el nivel de significancia 
Probamos nuestro modelo sin incluir esas variables:
gasto.als = beta0 + beta1\*numpeho + beta3\*añosedu + beta5\*area + beta6\*refin + beat8\*IA + e
```
m2 <- update(m1, ~. - edadjef - nse5f - sexojef)
summary(m2)
```

#### TÉRMINOS DE INTERACCIÓN
Para evaluar efectos cruzados, se tomarán las variables del modelo
```
mfull <- lm(gasto.als ~ numpeho + añosedu + numpeho:añosedu + numpeho:area + numpeho:refin + numpeho:IA +añosedu:area + añosedu:refin + añosedu:IA)
summary(mfull)
```

Ahora debemos evaluar la significancia global del modelo, es decir, 
podemos comparar un modelo tomando en cuenta todos los efectos cruzados y compararlo 
contra otro modelo sin efectos cruzados.

Para ello, planteamos el siguiente juego de hipótesis:
H0: beta2 = beta4 = beta7 = 0
(gasto.als = beta0 + beta1\*numpeho + beta3\*añosedu + beta5\*area + beta6\*refin + beat8\*IA + e )

H1: H0 no es verdad (AL MENOS UN COEFICIENTE ES DISTINTO DE 0)
(gasto.als = beta0 + beta1\*numpeho + beta2\*edadjef + beta3\*añosedu + beta4\*nse5f + beta5\*area + beta6\*refin + beta7\*sexojef + beat8\*IA + e )

Para este tipo de inferencia usamos el enfoque de análisis de varianza (ANOVA), 
ya que estamos comparando la variabilidad de un modelo no restringido contra la 
variabilidad de un modelo restringido.

`anova(m2,mfull)`

Como el p-value es menor que el grado se significancia entonces EEE para rechazar la hipótesis nula
por lo tanto gasto.als = beta0 + beta1\*numpeho + beta3\*añosedu + beta5\*area + e


#### Supuestos de la regresión lineal
El modelo de regresión lineal clásico establece ciertos supuestos en el término de error:
1. El término de error no tiene correlación significativa con las variables explicativas. En caso contrario, tendríamos un problema de endogeneidad.
2. El término de error sigue una distribución normal

Se tomaran los residuos estandarizados y se muestran las gráficas
```
StanRes2 <- rstandard(m2)
plot(numpeho, StanRes2, ylab = "Residuales Estandarizados")
plot(añosedu, StanRes2, ylab = "Residuales Estandarizados")
```

Se busca evidencia para soportar la hipótesis de normalidad en los errores por lo que se compara los quantiles de la distribucion normal con los quantiles de los residuos
```
qqnorm(StanRes2)
qqline(StanRes2)
```

Con la función Shapiro-wilk contrasta si la distribucion muestral de la variables se asemeja a la distribucion normal. Debido a que tenemos una muestra de datos amplia, usamos solo una subconjunto de 5000 datos para poder realizar el test.
`shapiro.test(StanRes2[0:5000])`


Se vuelven a tomar los residuos estandarizados, pero ahora del modeloFull
```
StanRes <- rstandard(mfull)
plot(numpeho, StanRes, ylab = "Residuales Estandarizados")
plot(añosedu, StanRes, ylab = "Residuales Estandarizados")

# Busca evidencia para soportar la hipótesis de normalidad en los errores 

qqnorm(StanRes)
qqline(StanRes)
```

### Conclusión
Según este modelo de regresión lineal, el número de personas en el hogar, los años de educación y la zona geográfica
determinan el gasto en alimentos saludables


# 6. Escribe tu análisis en un archivo README.MD y tu código en un script de R y publica ambos en un repositorio de Github.

Este estudio ha analizado cuantitativamente los determinantes socioeconómicos del gasto en alimentos saludables en México.
Los resultados sugieren que las familias tienen un nivel educativo y un poder adquisitivo considerablemente más alto que la media nacional. 
El análisis econométrico sugiere que el gasto en alimentos saludables está positivamente correlacionado con 
el número de años de educación, la zona geográfica y el número de personas en el hogar, lo que refleja que 
1) El consumo de alimentos saludables es restringido para los hogares pobres, y 
2) Los individuos con mayor educación tienen mayor acceso a información sobre los beneficios que conlleva el consumo de productos saludables y los perjuicios que acarrea el consumo de alimentos no saludables. 

# Postwork Sesión 4: Probabilidad y funciones de distribución

#Utilizando la variable total_intl_charge de la base de datos telecom_service.csv de la sesión 3, realiza un análisis probabilístico. 
#Para ello, debes determinar la función de distribución de probabilidad que más se acerque el comportamiento de los datos. Hint: Puedes apoyarte de medidas descriptivas o técnicas de visualización.

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/telecom_service.csv")
summary(df)

#Una vez que hayas seleccionado el modelo, realiza lo siguiente:
  
#  Grafica la distribución teórica de la variable aleatoria total_intl_charge
hist(df$total_intl_charge)
(mean <- mean(df$total_intl_charge))
(sd <- sd(df$total_intl_charge))

x <- seq(-4, 4, 0.01)*sd + mean
y <- dnorm(x, mean = mean, sd = sd) 

plot(x, y, type = "l", xlab = "X", ylab = "f(x)",
     main = "Densidad de Probabilidad Normal", 
     sub = expression(paste(mu == mean, " y ", sigma == sd)))

curve(dnorm(x,mean = mean, sd = sd), from=0, to=6,
col='blue', main="Densidad de probabilidad normal",
ylab = "f(x)",xlab="X")

#¿Cuál es la probabilidad de que el total de cargos internacionales sea menor a 1.85 usd?
pnorm(q = 1.85, mean = mean, sd = sd)
  
#  ¿Cuál es la probabilidad de que el total de cargos internacionales sea mayor a 3 usd?
pnorm(q = 3, mean = mean, sd = sd, lower.tail = F)
  
#  ¿Cuál es la probabilidad de que el total de cargos internacionales esté entre 2.35usd y 4.85 usd?
pnorm(q = 4.85, mean = mean, sd = sd) - pnorm(q = 2.35, mean = mean, sd = sd)

#  Con una probabilidad de 0.48, ¿cuál es el total de cargos internacionales más alto que podría esperar?
qnorm(p=0.48,mean = mean, sd = sd)

#  ¿Cuáles son los valores del total de cargos internacionales que dejan exactamente al centro el 80% de probabilidad?
qnorm(p = 0.2/2, mean = mean, sd = sd); qnorm(p = 0.2/2, mean = mean, sd = sd, lower.tail = FALSE)
qnorm(p = 0.2/2, mean = mean, sd = sd); qnorm(p = 1 - 0.2/2, mean = mean, sd = sd, lower.tail = T)

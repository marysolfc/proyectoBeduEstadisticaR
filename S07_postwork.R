#Postwork sesión 7: Predicciones de la temperatura global
#Utilizando el siguiente vector numérico, realiza lo que se indica:

url = "https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-07/Data/global.txt"
Global <- scan(url, sep="")
head(Global)
class(Global)
str(Global)

#Crea una objeto de serie de tiempo con los datos de Global. La serie debe ser mensual comenzado en Enero de 1856
global.ts <- ts(Global, start = c(1856,1), freq = 12)
  
#Realiza una gráfica de la serie de tiempo anterior
plot(global.ts, 
     main = "Global", 
     xlab = "Tiempo",
     sub = "Enero de 1856 - Diciembre de 2005")

#Ahora realiza una gráfica de la serie de tiempo anterior, transformando a la primera diferencia:
plot(diff(global.ts), 
     main = "Global", 
     xlab = "Tiempo",
     sub = "Enero de 1856 - Diciembre de 2000")

#¿Consideras que la serie es estacionaria en niveles o en primera diferencia?
#Es casi estacionaria, excepto por los primeros años

#Con base en tu respuesta anterior, obten las funciones de autocorrelación y autocorrelación parcial?
acf(diff(global.ts))
pacf(diff(global.ts))

arima(global.ts, order = c(1, 1, 1))
arima(global.ts, order = c(1, 1, 2))
arima(global.ts, order = c(1, 1, 3))
arima(global.ts, order = c(1, 1, 4))

fit <- arima(global.ts, order = c(1, 1, 4))
pr <- predict(fit, 12)$pred 



# Postwork Sesión 5: Teorema del limite central e inferencia estadística

#El data frame iris contiene información recolectada por Anderson sobre 50 flores de 3 especies distintas (setosa, versicolor y virginca), 
#incluyendo medidas en centímetros del largo y ancho del sépalo así como de los pétalos.

#Utilizando pruebas de inferencia estadística, concluye si existe evidencia suficiente para concluir que los datos recolectados por Anderson 
#están en línea con los nuevos estudios.

#Utiliza 99% de confianza para toda las pruebas, en cada caso realiza el planteamiento de hipótesis adecuado y concluye.

str(iris)

#Estudios recientes sobre las mismas especies muestran que:
  
#1) En promedio, el largo del sépalo de la especie setosa (Sepal.Length) es igual a 5.7 cm
"Planteamiento de hipótesis:"
#Ho: mu = 5.7
#Ha: mu != 5.7
t.test(iris[iris$Species == 'setosa', "Sepal.Length"],alternative = 'two.sided', mu=5.7)

# p-value < 2.2e-16 < 0.01. 
#Conclusión: A nivel de confianza del 99%, EEE para rechazar la Ho es decir que en promedio el largo del sépalo de la especie setosa es igual a 5.7


#2) En promedio, el ancho del pétalo de la especie virginica (Petal.Width) es menor a 2.1 cm
"Planteamiento de hipótesis:"
#Ho: mu >= 2.1
#Ha: mu < 2.1
t.test(iris[iris$Species == 'virginica', "Petal.Width"],alternative = 'less', mu=2.1)

#p-value = 0.03132 0.03132 < 0.01 FALSE
#Conclusión: A nivel de confianza del 99%, No EEE para rechazar la Ho es decir que en promedio el ancho del péetalo de la especie virginica es mayor o igual a 2.1


#3) En promedio, el largo del pétalo de la especie virgínica es 1.1 cm más grande que el promedio del largo del pétalo de la especie versicolor.
"Planteamiento de hipótesis:"
#Ho: mu <= 1.1
#Ha: mu > 1.1
var.test(iris[iris$Species == 'virginica', "Petal.Length"], 
         iris[iris$Species == 'versicolor', "Petal.Length"], 
         ratio = 1, alternative = "two.sided")


t.test(iris[iris$Species == "virginica", "Petal.Length"],
       iris[iris$Species == "versicolor", "Petal.Length"],
       alternative = "greater", mu = 1.1, var.equal = TRUE)


#p-value = 0.03202 < 0.01 FALSE
#Conclusión: A nivel de confianza del 99%, No EEE para rechazar la Ho es decir que en promedio el largo del pétalo de la especie virgínica es 1.1 cm más grande que el promedio del largo del pétalo de la especie versicolor

#4) En promedio, no existe diferencia en el ancho del sépalo entre las 3 especies.

boxplot(Sepal.Width ~ Species, data = iris)

anova <- aov(Sepal.Width ~ Species,
             data = iris)

summary(anova)
?aov

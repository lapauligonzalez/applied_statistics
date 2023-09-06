# ------------T2.1

#-----Analisis Descriptivo de la variable "tenure"
#tenure = cantidad de meses que tiene de deuda
install.packages("beeswarm")
data <- read.csv("data_credit_card")
View(data)
x <- data$tenure


#----a)
# El rango de los datos es el maximo menos el mínimo de la muestra.
minimo<- min(x)
maximo <- max(x)
rango <- maximo - minimo

# Función de distribución emprírica = Función de probabilidad acumulada
plot(ecdf(x), main = "Funcion de distribución empírica para tenure")


#----b)
#Histograma - estima la función de densidad - Continuas
hist(x, freq = FALSE, 
     main = "Histograma de tenure",
     xlab = "tenure",
     ylab = "density",
     col = "mediumorchid3") #freq = False me cambia el x-axis a densidad 

#Gráfico de densidad - Continuas - Elijo otra variable del dataset
#filtrando los 1 y 0 porque son puntos pesados
x_filtrado <- data$purchases_freq[data$purchases_freq > 0 & data$purchases_freq < 1]
plot(density(x_filtrado),
     lwd = 4, 
     col = "red2", 
     main = "Density de los datos continuos de purchases_freq sin los puntos pesados")

#no filtrando los 1 y 0 para ver cómo afecta
plot(density(data$purchases_freq),
     lwd = 4, 
     col = "red2", 
     main = "Density de los datos continuos de purchases_freq")


#Barplot - estima la función de probabilidad (no acumulada) - Discretas
plot(table(x)/length(x),
     lwd = 6, 
     col = "mediumorchid3",
     main = "Funcion de probabilidad estimada para tenure", 
     ylab = "Proporción",
     xlab = "Tenure") 


#----c)
media <- mean(x)
mediana <- median(x)
# Como la mediana es mayor a la media entonces la distribución tiene asimetria negativa


#----d)
#Cuantiles y Rango intercuantil (RIC)
quantile(x, c(0.25,0.5,0.75))
boxplot(scale(x), 
        horizontal = TRUE)

#un boxplot más lindo es el gráfico de beeswarm
beeswarm::beeswarm(data$credit_limit, corral = "wrap",
                   corralWidth = 1, pch = 20)


#----e)
#Desvío estándar
varianza <- sum((x - media)^2)/500
desvio_estandar <- sqrt(varianza)

#Coeficiente variación
coef_vari <- desvio_estandar/media

#Coeficiente de asimetría
coef_asim <- sum(((x - media)/desvio_estandar)^3)/500

#Coeficiente de kurtosis
coef_kurtosis <- sum(((x - media)/desvio_estandar)^4)/500

#----f)
#----g)
#----h)

# ------------T2.2
f.empirica <- function(x, x0){
  suma <- sum(x <= x0)
  n <- length(x)
  return(suma/n)
}

ecdf(data_credit_card$purchases)(2)
f.empirica(data_credit_card$purchases,2)

#que porcentaje de usuarios tiene un limite de credito superior a la media?
(1 - f.empirica(data_credit_card$credit_limit, mean(data_credit_card$credit_limit)))*100

#que porcion de usuarios gastan menos de 10000?
f.empirica(data_credit_card$purchases,10000)


# ------------T2.3
#Para calcular un cuantil se usa:
quantile(x, w, type = 1)

#El gasto sólo superado por el 10% de los usuarios
quantile(data_credit_card$purchases, 0.90, type = 1)

#El valor de credit limit garantizado con probabilidad 95%
quantile(data_credit_card$credit_limit, 0.95, type = 1)
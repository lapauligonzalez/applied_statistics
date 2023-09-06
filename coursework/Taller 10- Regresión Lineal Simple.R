data <- na.omit(airquality)

#---a)

pairs(data[,-c(5,6)]) #nos va a hacer scatterplots de pares

#A simple vista se podría ver que tienen correlaciones.

# Y = medición de ozono en un cierto dia
# X = medición de temperatura

#Planteamos el modelo para la funcion de regresión: Y = B0 + B1x + Ei

plot(data$Temp, data$Ozone, pch = 20)

#---b)

# Estimamos los parámetros:

x <- data$Temp
y <- data$Ozone

# B1
B1 <- cov(x,y)/var(x)
#Por cada grado Farhenheit que aumenta la temperatura del dia la esperanza de la 
#concentración de ozono aumenta en 2.439

# B0
B0 <- mean(y) - B1*mean(x)

#S2
y_sombrero <- B0 + B1*x
S2 <- sum((y-y_sombrero)**2)/(length(x)-2)

#otra forma de calcularlo es
Q <- sum((y-y_sombrero)**2)
Te <- sum((y-mean(y))**2)

S2_2 <- Q/(length(x)-2)
R2 <- 1-Q/Te #El R es el coeficiente de correlación 

#Ahora graficamos la recta de regresión
plot(x,y,pch=20)
abline(a = B0, b = B1, col = "red", lwd = 4)
#Los puntos estan bastantes corridos para abajo, quizás estan sesgados los datos pero no vamos a decir nada

# --- Analisis de Residuos

#--Para comprobar supuesto de linealidad
plot(x, rstandard(modelo), pch = 20) 
#no es normal y nos afecta la no linealidad porque nos produce estimacionaes sesgadas.

#--Para comprobar supuesto de normalidad
#Linear model te tira todos los datos
modelo <- lm(data$Ozone ~ data$Temp, data = data)
summary(modelo)

e_std <- rstandard(modelo)

hist(e_std) #Podría ser una distribución gamma
qqnorm(e_std)
qqline(e_std) #Nos da pansa positiva, era asimetría positiva en la distribución.  
#Esto significa que nos datos no son normales, no se puede afirmar la normalidad.

#--Para comprobar supuesto de homocedasticidad
plot(y_sombrero, rstandard(modelo), pch = 20) 
#no es homocedastico, tienen distribución de abanico.  



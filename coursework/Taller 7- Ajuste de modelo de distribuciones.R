#Ejercicio T7.1---------------------------------

data <- melbourne.properties
#Tomamos un sample de 300 hogares y registramos los precios
#En miles
x <- sample(data$Price, 300)/100

#a) Construir una tabla de frecuencias absolutas en 5 clases  
#b) Hacer un test de Contraste Chi-Cuadrado

#Realizamos un ensayo de bondad de ajuste
#utilizando el contraste chi cuadrado

hist(x, freq=FALSE)
#la teoria diria que esta es una lognormal

#vamos a probar hacer el analisis con la weibull

#Ho: X tiene distribucion Weibull
#armamos la tabla para el contraste
#frecuencias absolutas
hist(x, breaks=7)$counts

fa <- c(hist(x, breaks=7)$counts[1:5],8)

#cantidad de clases
nk <- length(fa)


#Adaptar y corregir las siguientes líneas ...

#vectores que contienen li y ls
li<-hist(x, breaks = 7)$breaks[1:nk] #$breaks te trae las clases que queremos
ls<-c(li[2:nk],NA)

#armo un data frame para poner todo
table<-cbind.data.frame(li,ls,fa)

#para sacar las fei necesitamos estimar
#omega y beta
#obtengo estimadores de maxima verosimilitud con optim
Verosim<-function(tita){
  return(sum(log(dweibull(x,tita[1],tita[2]))))
}

tita_opt <- optim(c(1.5, 20000),
                  Verosim,
                  control = list(fnscale=-1))$par#son los titas de maxima verosimilitud

omega<-tita_opt[1]
beta<-tita_opt[2]

#calculo las probabilidades de todas las clases
p<-c()
for(i in 1:nk-1){
  p[i]<-pweibull(ls[i],omega,beta)-pweibull(li[i],omega,beta)
  }
p[nk]<-1-sum(p)

table<-cbind.data.frame(table,p)

#frecuencias esperadas
fe<-round(length(x)*p,4)
table<-cbind.data.frame(table,fe)

#comparativa con la densidad que buscamos
hist(x,freq=FALSE)
curve(dweibull(x,omega,beta), add=TRUE, col='red')

#buscamos el estadistico del test
attach(table)
w<-sum((fa-fe)^2/fe)

#calculamos el p valor
1-pchisq(w,nk-2)

#rechazamos con mucha holgura la Ho
#podemos afirmar que X NO tiene distribucion Weibull

#Hacer una comparación de distribución Log-normal con Weibull con log-verosimilitud
#-------------------hago lo mismo con Ln

remove(table)
#Ho: X tiene distribucion Lognormal
#armamos la tabla para el contraste
#frecuencias absolutas
fa<-c(hist(Price)$counts[1:12],6)

#cantidad de clases
nk<-length(fa)

#vectores que contienen li y ls
li<-hist(Price)$breaks[1:nk]
ls<-c(hist(Price)$breaks[2:nk],NA)

#armo un data frame para poner todo
table<-cbind.data.frame(li,ls,fa)

#para sacar las fei necesitamos estimar
#omega y beta
#obtengo estimadores de maxima verosimilitud con optim
Verosim2<-function(tita){
  return(sum(log(dlnorm(x,tita[1],tita[2]))))
}
tita_opt <- optim(c(1.5, 20000),
                  Verosim2,
                  control = list(fnscale=-1))$par#son los titas de maxima verosimilitud
m<-tita_opt[1]
D<-tita_opt[2]
  
#calculo las probabilidades de todas las clases
p<-c()
for(i in 1:nk-1){
  p[i]<-plnorm(ls[i],m,D)-plnorm(li[i],m,D)
}
p[nk]<-1-sum(p)

table<-cbind.data.frame(table,p)


#frecuencias esperadas
fe<-round(length(x)*p,4)
table<-cbind.data.frame(table,fe)

#comparativa con la densidad que buscamos
hist(x,freq=FALSE)
curve(dlnorm(x,m,D), add=TRUE, col='red')

#buscamos el estadistico del test
attach(table)
w<-sum((fa-fe)^2/fe)

#calculamos el p valor
1-pchisq(w,nk-2)

#dio muy peque?o por tratarse de una muestra
#muy grande, pero es mucho mejor que 
#el de la weibull.

#una opcion en estos casos
#es comparar el estadistico w
#para ver cual es mejor.

#Para hacer la comparación de log-verosimilitud entre weibull y log-normal vamos a usar los valores
#comparamos los dos máximos de las dos distribuciones

sum(log(dweibull(x, omega, beta)))
sum(log(dlnorm(x, m, D)))

#Ejercicio T7.2----------------------------------

#Carga del archivo Amazon
y <- amazon$X0

tau <- mean(y)
rho <- sd(y)

#extraemos los residuos
e <- (y-tau)/rho #estamos estandarizando.  En este modelo e actúa como una z
plot(density(e))

#Test de Jaque Bera
qchisq(0.95, 2)


gamma <- mean(e**3)
k <- mean(e**4)

JB <- length(y)/6*(gamma**2 + 0.25(k-3)**2)

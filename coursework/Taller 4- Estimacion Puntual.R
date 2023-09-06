datos = c(118.4 , 112.96 , 120.88 , 145.4 , 126.59 , 133.71 , 97.59 , 114.51 , 36.42 , 114.96 ,70.66 , 150.51 , 129.74 , 69.97 , 82.77 , 4.96 , 97.79 , 46.27 , 68.24 , 8.35 , 150.68 ,95.12 , 80.92 , 120.17 , 94.5 , 102.99 , 166.53 , 115.48 , 123.32 , 92.17 , 129.18 ,121.46 , 124.15 , 113.68 , 147.97 , 104.31 , 89.41 , 61.81 , 77.53 , 300.91 , 110.86 ,147.94 , 104.48 , 166.08 , 147 , 150.78 , 121.25 , 188.61 , 147.13 , 170.24 , 114.42 ,123.36 , 147.26 , 167.96 , 132.95 , 139.76 , 96.49 , 135.92 , 172.71 , 89.18 , 1.62 ,73.09 , 148.85 , 160.65 , 135.9 , 114.51 , 185.65 , 130.65 , 153.54 , 78.12 , 91.52 ,137.89 , 90.4 , 121.58 , 220.68 , 109.16 , 152.83 , 132 , 137.32 , 127.85 , 139.76 ,39.34 , 113.63 , 143.43 , 187.01 , 114.44 , 116.36 , 151.84 , 107.02 , 134.38 , 221.53 ,142.03 , 112.05 , 103.2 , 144.75 , 110.56 , 142.39 , 75.05 , 117.44 , 135.78)
set.seed(3)

# 1) -------- Método no paramético

q1_1 <- mean(datos)
se_q1_1 <- sd(datos)/sqrt(100)

q2_1 <- quantile(datos,0.9, type = 1)
se_q2_1 <- sd(replicate(1000,{
  x_nuevo <- sample(datos,100, replace = TRUE)
  quantile(x_nuevo,0.9) 
}))

q3_1 <- mean(datos>150)
se_q3_1 <- 

# 2) -------- Método paramétrico Log-Normal por Momentos
  
q1_2 <- mean(datos) #Es igual al anterior porque en el método de momentos éstos se calculan con la misma media
s2 <- sqrt(sum(datos^2)/100 - q1_2^2) #es lo mismo que sqrt(sum((datos - q1_3)^2)/100)
se_q1_2 <- s2/sqrt(100) 

m = log(q1_2/sqrt(1+(s2/q1_2)^2))
D2 = log(1 + (s2/q1_2)^2)
q2_2 <- exp(qnorm(0.9)*sqrt(D2) + m) #lo hacemos con la distribución normal porque se estandarizan los valores
se_q2_2 <- sd(replicate(1000,{
  x_nuevo <- sample(datos,100, replace = TRUE)
  m_nuevo = log(q1_2/sqrt(1+(sd(x_nuevo)/q1_2)^2))
  D2_nuevo = log(1 + (sd(x_nuevo)/q1_2)^2)
  exp(qnorm(0.9)*sqrt(D2) + m) 
}))

q3_2 <- 1 - plnorm(150, m, sqrt(D2)) # lo hacemos 1 - plnorm porque nos calcula la probabilidad de que X<150 por default
se_q3_2 <- #no se
  
# 3) -------- Método paramétrico Normal por Momentos

q1_3 <- sum(datos)/100
s3 <- sqrt(sum(datos^2)/100 - q1_3^2) #es lo mismo que sqrt(sum((datos - q1_3)^2)/100)
se_q1_3 <- s3/sqrt(100) #Me da lijeramente distinto, porque?

q2_3 <- qnorm(0.9, q1_3, s3) 
se_q2_3 <- sd(replicate(1000,{
  x_nuevo <- sample(datos,100, replace = TRUE)
  q1_3_nuevo <- sum(x_nuevo)/100
  s3_nuevo <- sqrt(sum(x_nuevo^2)/100 - q1_3_nuevo^2) 
  qnorm(0.9, q1_3_nuevo, s3_nuevo) 
}))
  
q3_3 <- 1 - pnorm(150, q1_3, s3)
se_q3_3 <- #no se

# 4) -------- Método paramétrico Gamma por Momentos
  
q1_4 <- mean(datos)
s4 <- sqrt(sum(datos^2)/100 - q1_4^2)
se_q1_4 <- s4/sqrt(100)

alfa <-(q1_4/s4)^2
beta <- s4^2/q1_4
q2_4 <- qgamma(0.9, shape = alfa, scale = beta)
se_q2_4 <- sd(replicate(1000,{
  x_nuevo <- sample(datos,100, replace = TRUE)
  q1_4_nuevo <- mean(x_nuevo)
  s4_nuevo <- sqrt(sum(x_nuevo^2)/100 - q1_4_nuevo^2)
  alfa_nuevo <-(q1_4_nuevo/s4_nuevo)^2
  beta_nuevo <- s4_nuevo^2/q1_4_nuevo
  qgamma(0.9, shape = alfa_nuevo, scale = beta_nuevo)
}))
  
q3_4 <- 1 - pgamma(150, shape = alfa, scale = beta)
se_q3_4 <- #no se
  
# 5) -------- Método paramétrico Log-Normal por Máxima Verosimilitud

m_mv = sum(log(datos))/100 #Calculo de parametros por maxima verosimilitud (no se usa la media ni desvío como en metodo de momentos para calcular parametros)
D2_mv = sqrt(sum(log(datos)-m_mv)^2)/100

q1_5 <- exp(m_mv + (D2_mv/2)) #me dio muy distinto a lo del pizarron
se_q1_5 <- sd(replicate(1000,{
  x_nuevo <- sample(datos,100, replace = TRUE)
  m_mv_nuevo = sum(log(x_nuevo))/100 
  D2_mv_nuevo = sqrt(sum(log(x_nuevo)-m_mv_nuevo)^2)/100
  exp(m_mv_nuevo + (D2_mv_nuevo/2))
}))

q2_5 <- exp(qnorm(0.9)*sqrt(D2_mv) + m_mv)
se_q2_5 <- sd(replicate(1000,{
  x_nuevo <- sample(datos,100, replace = TRUE)
  m_mv_nuevo = sum(log(x_nuevo))/100 
  D2_mv_nuevo = sqrt(sum(log(x_nuevo)-m_mv_nuevo)^2)/100
  exp(qnorm(0.9)*sqrt(D2_mv_nuevo) + m_mv_nuevo)
}))

#Pedro lo hizo así:
#boot <- replicate(1000,{
#  x_nuevo <- sample(datos,100, replace = TRUE)
#  m <- mean(log(x_nuevo))
#  D <- sd(log(x_nuevo))
#  exp(D*qnorm(0.9) + m) #usamos el qnorm porque estandarizamos a una normal estandar
#})
#sd(boot) 

q3_5 <- #no se
se_q3_5 <- #no se
                  

#--------------------------------- 

# Estimador del cuantil 0.9 por maxima verosimilitud para distribución gamma
# Este método tambien aplica para las distribuciones weibull y beta

estimador <- function(x){
  
  #funcion que voy a optimizar - LogLikelihood
  loglikelihood <- function(par){ #par es un vector que tiene alfa y beta, que se va a usar para la funcion optim
    sum(log(dgamma(x, shape = par[1], scale = par[2])))
  } 
  
  #maximizar la verosimilitud y extraigo los parametros
  parametros <- optim(c(7.85,15.41), loglikelihood,
        control = list(fnscale = -1))$par #por defecta hace minimización entonces agregamos lo de abajo
  
  #ahora metemos los parametros calculador por MV en el cuantil
  qgamma(0.9, shape = parametros[1],scale = parametros[2])
  
  }

estimador(datos)
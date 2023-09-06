#-------T1.1
#a) Discreta y transversal
#b)
set.seed(4)
x <- runif (1, 10, 11)

#c) 
y <- runif (20, 10, 11)
y


#-------T1.2
#a) Contínua y transversal. N~Bin(n=20, p=0.4)
#b) La muestra es una serie de tiempo.
#c)
set.seed(4)
defectuosas <- sum(y > 10.6)

#d) haciendolo con for:
set.seed(4)
muestra <- c()
for (dia in 1:30){
  e <- runif (20, 10, 11)
  defectuosas <- sum(e > 10.6)
  muestra[dia] <- defectuosas
}
muestra

#haciendolo con replicate
set.seed(4)
muestra = replicate(30,{
  x = runif(20,10,11)
  y = (x>10.6)
  n = sum(y)
  n
})
muestra


#-------T1.3
#a) Contínua
#d) 
x <- runif (30, 10, 11)
set.seed(4)
t = replicate(30, (sum(runif(1000,10,11)))/1000)
hist(t)



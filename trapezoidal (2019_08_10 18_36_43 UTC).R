fx <- function (x, a=2, b=5){
     k <- a*b*(x**(a-1))*((1-(x**a))**(b-1))
  return(k)
}

Fx<- function (x, a=2,b=5){
  w<- 1-((1-x^a)^b)
  return(w)
}

ifx<- function (n,a=2,b=5){
  u<-runif(n)
  y<- (1-(1-u)^(1/b))^(1/a)
  return(y)
}

kum <- ifx(runif(1000),a= 2, b= 5)

plot(ecdf(kum), xlim= c(0,1)) # Distribuição acumulada empírica e teórica
curve(Fx(x, a= 2, b= 5), add=TRUE, col=2)

pp <- function(kum, a=1/2){
  m <- length(kum)
  (1:m-a)/(m+ (1-a)-a)
}

#pp <- function(x, a=1/2){
#  m <- length(x)
#  (1:m-a)/(m+ (1-a)-a)
#}

Pteo <- pp(kum) ## Prob acum. teóricas.
#Pobs <- Fx (sort(kum), a=2, b=5) ## Freq. rel. acum. observadas.
Pobs <- (1:length(kum))/length(kum)

plot(Pobs~Pteo,
     xlab="Frequências relativas acumuladas teóricas",
     ylab="Probabilidades acumuladas observadas")
rug(x=Pteo, side= 2)
rug(x=Pobs)

# Quantis teóricos e observados.
qteo <- ifx(Pteo, a=2, b=5)
qobs <- sort(kum)

# Gráfico Q-Q plot: quantis teóricos vs observados.
plot(qteo ~ qobs)
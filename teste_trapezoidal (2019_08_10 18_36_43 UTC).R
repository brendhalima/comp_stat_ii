
fxTrap <- function(x, a=5, b=7, c=9, d=11, h=16){
  h <- 2/((d-a)+(c-b))
  if(a<=x && x<b)
    fx <- (h*(x-a))/(b-a)
  return(fx)
  if(b<=x && x<c)
    fx <- h
  return(fx)
  if(c<=x && x<d)
    fx <- (h*(d-x))/(c-x)
  return(fx)
}

n < - runif(100)
x <- sample(1:n, 100, replace = TRUE)
curve(fxTrap(x=x, a = a, b = b, c= c, d = d))


FxTrap <- function(x, a=5, b=7, c=9, d=11, h=16){
  if(x<=0)
    Fx <- 0
  return(Fx)
  if(a<=x && x<c)
    Fx <- h*((x-a)^2-(2(c-a)))
  return(Fx)
  if(c<=x && x<=d)
    Fx <- ((h/2)*(c-a)+h*(x-c))
  return(Fx)
  if(d<=x && x<=b)
    Fx <- 1-((h*(b-x^2))/2*(b-d))
  return(Fx)
  if(b<= x)
    Fx <- 1
  return(Fx)
}
  
iFxTrap <- function(x, a=5, b=7, c=9, d=11, h=16){
  if(0<=x && x<= (h/2)*(c-a))
    y <- (a+sqrt(2*(c-a)/h))*sqrt(y)
  if((h/2)*(c-a) <= x && x <= (1-(h/2))*(b-d))
    y <-((c+a)/2)+(y/h)
  if (1-(h/2)*(b-d) <= x && x <=1)
    y <- (b-sqrt((2*(b-d))/h)*sqrt(1-y))
  else(
    y <- 0
  )
  return(y)
}

w <- iFxTrap(runif(5000),)

plot(ecdf(iFxTrap)) # Distribuição acumulada empírica e teórica
curve(FxTrap(Fx, a=a, b=b, c=c, d=d, h=h), add=TRUE, col=2)

Pobs <- (1:length(x))/length(x) ## Freq. rel. acum. observadas.
Pteo <- Fx(sort(x), mi= 5, s= 2) ## Prob acum. teóricas.

plot(Pobs~Pteo,
     xlab="Frequências relativas acumuladas teóricas",
     ylab="Probabilidades acumuladas observadas")
rug(x=Pteo, side= 2)
rug(x=Pobs)

# Quantis teóricos e observados.
qteo <- iFx(Pteo, mi= 5, s= 2)
qobs <- sort(x)

# Gráfico Q-Q plot: quantis teóricos vs observados.
plot(qteo ~ qobs)

x <- sample(1:100, 100, replace = TRUE)
plot(density(x(x = x, a = a, b = b, c= c, d = d)))

plot(ecdf(fx(x = x, a = a, b = b, c= c, d = d)))


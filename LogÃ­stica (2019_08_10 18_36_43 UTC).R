### LOGÍSTICA-----------

#'@param mi é
#'@param s é
#'@return fx é a funcão densidade da logística

fx <- function(x,mi,s){
      (exp((-x+mi)/s))/(s*((1)+exp((-x+mi)/s)^2))
}
curve(fx(x, mi= 5, s= 2), -5, 20)

#'@return Fx é a funcão de distribuição

Fx <- function(x, mi, s) {
      1/(1+exp((-x+mi)/s))
}
curve(Fx(x, mi= 5, s= 2) -5, 20)

#'@return iFx é a função inversa da distribuição

iFx <- function(u, mi, s) {
       mi-((log((1/u)-1))*s)
}

#'@param x retorna a função inversa com seus devidos valores

x <- iFx(runif(1000), mi= 5, s= 2)

plot(ecdf(x), xlim= c(0,1)) # Distribuição acumulada empírica e teórica
curve(Fx(x, mi= 5, s= 2), add=TRUE, col=2)

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
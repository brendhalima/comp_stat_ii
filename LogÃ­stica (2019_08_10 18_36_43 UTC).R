### LOG�STICA-----------

#'@param mi �
#'@param s �
#'@return fx � a func�o densidade da log�stica

fx <- function(x,mi,s){
      (exp((-x+mi)/s))/(s*((1)+exp((-x+mi)/s)^2))
}
curve(fx(x, mi= 5, s= 2), -5, 20)

#'@return Fx � a func�o de distribui��o

Fx <- function(x, mi, s) {
      1/(1+exp((-x+mi)/s))
}
curve(Fx(x, mi= 5, s= 2) -5, 20)

#'@return iFx � a fun��o inversa da distribui��o

iFx <- function(u, mi, s) {
       mi-((log((1/u)-1))*s)
}

#'@param x retorna a fun��o inversa com seus devidos valores

x <- iFx(runif(1000), mi= 5, s= 2)

plot(ecdf(x), xlim= c(0,1)) # Distribui��o acumulada emp�rica e te�rica
curve(Fx(x, mi= 5, s= 2), add=TRUE, col=2)

Pobs <- (1:length(x))/length(x) ## Freq. rel. acum. observadas.
Pteo <- Fx(sort(x), mi= 5, s= 2) ## Prob acum. te�ricas.

plot(Pobs~Pteo,
     xlab="Frequ�ncias relativas acumuladas te�ricas",
     ylab="Probabilidades acumuladas observadas")
rug(x=Pteo, side= 2)
rug(x=Pobs)

# Quantis te�ricos e observados.
qteo <- iFx(Pteo, mi= 5, s= 2)
qobs <- sort(x)

# Gr�fico Q-Q plot: quantis te�ricos vs observados.
plot(qteo ~ qobs)
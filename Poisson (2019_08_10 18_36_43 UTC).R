## Poisson por meio da Exponencial.

## O n�mero de eventos dentro de um intervalo unit�rio � uma Poisson se
## o tempo para ocorr�ncia de um evento tiver distribui��o Exponencial.

# Fx da Poisson


## Gerando n�meros da Exponencial.

iFx <- function(u, lambda){
       (-log(1-u))/lambda
}

lambda <-20

Fx <- function(x, lambda){
  1-exp(-lambda*x)
}

w <- Fx(runif(5000), lambda=lambda)
y <- iFx(runif(5000), lambda=lambda)

## Acumular e truncar para o inteiro acima.
y <- cumsum(y)
y <- ceiling(y)

w <- cumsum(w)
w <- ceiling(w)

## Contar ocorr�ncias em cada intervalo unit�rio.
x <- tabulate(y)
length(x)

xFx <- tabulate(w)
length(xFx)

plot(ecdf(x))
curve(ppois(x, lambda=lambda), type="s", add=TRUE, col="2")

pp <- function(x, a=1/2){
  m <- length(x)
  (1:m-a)/(m+ (1-a)-a)
}

Pobs <- Fx(sort(x), lambda=lambda) ## Freq. rel. acum. observadas.
Pteo <- pp(x) ## Prob acum. te�ricas.

plot(Pobs~Pteo,
     xlab="Frequ�ncias relativas acumuladas te�ricas",
     ylab="Probabilidades acumuladas observadas")
rug(x=Pteo, side= 2)
rug(x=Pobs)

# Quantis te�ricos e observados.
qteo <- iFx(Pteo, lambda= 2)
qobs <- sort(x)

# Gr�fico Q-Q plot: quantis te�ricos vs observados.
plot(qteo ~ qobs)


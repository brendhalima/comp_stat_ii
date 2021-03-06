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

u <- runif(1000)
x <- iFx(u, lambda=0.5)
Pobs <- (1:length(x))/length(x) ## Freq. rel. acum. observadas.
Pteo <- Fx(sort(x), lambda=0.5)

plot(Pteo ~ Pobs)

# Quantis te�ricos e observados.
qteo <- iFx(Pteo, lambda= 2)
qobs <- sort(x)

# Gr�fico Q-Q plot: quantis te�ricos vs observados.
plot(qteo ~ qobs)


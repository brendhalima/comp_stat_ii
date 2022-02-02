## Poisson por meio da Exponencial.

## O número de eventos dentro de um intervalo unitário é uma Poisson se
## o tempo para ocorrência de um evento tiver distribuição Exponencial.

# Fx da Poisson


## Gerando números da Exponencial.

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

## Contar ocorrências em cada intervalo unitário.
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
Pteo <- pp(x) ## Prob acum. teóricas.

plot(Pobs~Pteo,
     xlab="Frequências relativas acumuladas teóricas",
     ylab="Probabilidades acumuladas observadas")
rug(x=Pteo, side= 2)
rug(x=Pobs)

# Quantis teóricos e observados.
qteo <- iFx(Pteo, lambda= 2)
qobs <- sort(x)

# Gráfico Q-Q plot: quantis teóricos vs observados.
plot(qteo ~ qobs)


fx <- function(x) {
  0 + ifelse(x > 0, 1 - x, x + 1) * (x > -1) * (x < 1)
}
fy <- function(x) 3/4 * (1 - x^2) * (x >= -1) * (x <= 1)
M <- 4/3

y <- -0.97
u <- 	0.98
r <- fx(y)/(M*fy(y))

if (u < r) {
  #x <- y
  print("u < r, então valor aceito.")
} else {
  print("u >= r, então valor descartado.")
}


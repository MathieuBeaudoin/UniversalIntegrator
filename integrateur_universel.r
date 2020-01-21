# Intégrateur universel

BORNE_INF <- 0
BORNE_SUP <- 1.96
PAS <- 10000

fx <- function(x) return( (2*pi)**(-.5) * exp(-.5*x**2) )

Fx <- function(a, b, pas, total=0) {
  dx <- (b-a) / pas
  for(i in seq(a, b-dx, by=dx)) {
    total <- total + dx * fx(a+dx/2)
    a <- a+dx
  }
  return(total)
}

integrale <- Fx(BORNE_INF, BORNE_SUP, PAS)
integrale
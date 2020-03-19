library(MASS)
library(mvtnorm)
library(klaR)
library(e1071)

# Funkcja do wyznaczania prawdopodobieństw a posteriori w metodzie LDA
#z lab2 + wzór z zadania 
f.nb <- function(X, m1, m2, S1, S2, pi1, pi2) {
  return(
      (pi1 * dnorm(X, m1[1], S1[1]) %*% t(dnorm(X, m1[2], S1[2])))/ 
      (pi1 * dnorm(X, m1[1], S1[1]) %*% t(dnorm(X, m1[2], S1[2])) 
        + pi2 * dnorm(X, m2[1], S2[1]) %*% t(dnorm(X, m2[2], S2[2])))
  )
}

# Macierz kowariancji dla klas
S <- matrix(c(4,0,0,4),2,2)

# Wartości oczekiwane
mt1 <- c(-3, -1)
mt2 <- c(2, 2)

# Liczba punktów w klasach
n1 <- 40
n2 <- 30
n <- n1 + n2

# Generowanie rozkładów
X1 <- mvrnorm(n1, mt1, S)
X2 <- mvrnorm(n2, mt2, S)

# Zamiana na ramki danych
X1 <- data.frame(X1); colnames(X1) <- c("x", "y")
X2 <- data.frame(X2); colnames(X2) <- c("x", "y") 

# Przypisanie klas
X1$class <- 1; X2$class <- 2

# "Sklejenie" danych do jednej ramki
# oraz przekształcenie typu zmiennej przechowującej klasy
data <- rbind(X1, X2); data$class <- factor(data$class) 

# Estymowane wartości oczekiwane
me1 <- apply(X1[,1:2], 2, mean)
me2 <- apply(X2[,1:2], 2, mean)

# Estymowane macierze kowariancji
Se1 <- cov(X1[,1:2])
Se2 <- cov(X2[,1:2])

# Prawdopodbieństwa a priori
pi1 <- n1 / (n1 + n2)
pi2 <- n2 / (n1 + n2)

XY = seq(-10, 10, 0.1)

# Rysowanie
png("rysunek.png");
with(data, drawparti(class, x, y, method = "naiveBayes", xlab = "X", ylab = "Y", font = 2)) 

# Porównanie z wartościami otrzymanymi z rozkładów
contour(XY, XY, matrix(f.nb(XY, me1, me2, sqrt(diag(Se1)), sqrt(diag(Se2)), pi1, pi2), 
         length(XY)), add = T, levels = 0.5, lwd = 2, lty = 2, col = "blue")
dev.off()

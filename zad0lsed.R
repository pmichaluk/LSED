library(MASS)

#macierz kowariancji
S <- matrix(c(1,0,0,1),2,2)  

#ilosc punktow
n=30 

#srednie
m1<-c(-1, 1)
m2<-c(2,4)
m3<-c(-2,2)

# generowane rozkłady
X1<-mvrnorm(n,m1,S)
X2<-mvrnorm(n,m2,S)
X3<-mvrnorm(n,m3,S)


# zmiana na ramki
X1<-data.frame(X1); colnames(X1) <- c("x", "y")
X2<-data.frame(X2); colnames(X2) <- c("x", "y") 
X3<-data.frame(X3); colnames(X3) <- c("x", "y") 




plot(X1, ylim = c(-5,5), xlim = c(-2,10), pch = 19, col = "blue", xlab = "X", ylab = "Y", font = 2, asp = 1)
abline(v = 0, h = 0, col = "gray")
points(X2, pch = 19, col = "red") 
points(X3, pch = 19, col = "green")

# Średnia dla wszystkich punktów
m <- apply(rbind(X1, X2, X3), 2, mean)

# Macierz zmienności międzygrupowej
B <- (n*(m1 - m) %*% t(m1 - m) + n*(m2 - m) %*% t(m2 - m) + n*(m3 - m) %*% t(m3 - m) )/(3-1)

# Macierz zmienności wewnątrzgrupowej
W <- 1/(n - 3)*((n - 1) * S + (n - 1) * S + (n - 1) * S) 

# Macierz pomocnicza
U <- ginv(W) %*% B

# Wyznaczenia wartosci i wektorow własnych
lambda <- eigen(U)

# Wektor własny odpowiadający maksymalnej wartości własnej
a <- lambda$vectors[,which.max(lambda$values)]

# Rysowanie kierunku a
abline(0, a[2] / a[1], col = "violet", lwd = 2) 

# Funkcja do rzutowania obserwacji na kierunek a
rzutowanie <- function(X, A) {
  Xz <- (X$y * A + X$x) / (A**2 + 1)
  Yz <- A * Xz
  data.frame(x = Xz, y = Yz)
}

#współczynnik kierunkowej prostej
A<-a[2]/a[1]

#wykreslenie zrzutowanych punktow
points(rzutowanie(X1,A))
points(rzutowanie(X1,A),col="blue",pch=19,cex=1.5)
points(rzutowanie(X2, A), col = "red", pch = 19, cex = 1.5)
points(rzutowanie(X3, A), col = "green", pch = 19, cex = 1.5)


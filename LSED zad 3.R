library(MASS)
library(class)


#generowanie obserwacji o zadanych parametrach

draw.data.gauss <- function(S1, S2, m1, m2, n1, n2) {
  
  X1 <- mvrnorm(n1, m1, S1)
  X2 <- mvrnorm(n2, m2, S2)
  
  X1 <- data.frame(X1); colnames(X1) <- c("x", "y")
  X2 <- data.frame(X2); colnames(X2) <- c("x", "y")
  
  X1$class <- 1; X2$class <- 2
  
  data <- rbind(X1, X2); data$class <- factor(data$class)
  
  return(data)
}

# Parametry danych z rozkladu Gaussa
S1 <- matrix(c(4, 2, 2, 4), 2, 2)
S2 <- matrix(c(4, 2, 2, 2), 2, 2)

m1 <- c(-1, -1)
m2 <- c(2, 2)

n1 <- 30
n2 <- 20

# Generowanie obserwacji
data <- draw.data.gauss(S1, S2, m1, m2, n1, n2) 
dataTest <- draw.data.gauss(S1, S2, m1, m2, 10, 5) 


#fun spr skutecznosc

sk <- function(predictions, classes) {
  v <- sapply(1:length(predictions), function(x) {
    c(predictions[x] == classes[x],
      predictions[x] == classes[x] && classes[x] == 2,
      predictions[x] == classes[x] && classes[x] == 1)
  })
  return(c(length(v[1,][v[1,]==TRUE])/length(v[1,]),
           length(v[2,][v[2,]==TRUE]),
           length(v[3,][v[3,]==TRUE])))
}


k <- 1:21;
vect <- sapply(k, function(k) {
  class.knn <- knn(data[,1:2], data[,1:2], data$class, k)
  sk(class.knn, data$class)
})

dataTest <- draw.data.gauss(S1, S2, m1, m2, 100, 50) 
vect2 <- sapply(k, function(k) {
  class.knn <- knn(data[,1:2], dataTest[,1:2], data$class, k)
  sk(class.knn, dataTest$class)
})


for (val in c(1:(10-1))) {
  # Generowanie obserwacji
  data <- draw.data.gauss(S1, S2, m1, m2, n1, n2) 
  dataTest <- draw.data.gauss(S1, S2, m1, m2, 10, 5) 
  
  vect <- vect + sapply(k, function(k) {
    class.knn <- knn(data[,1:2], data[,1:2], data$class, k)
    sk(class.knn, data$class)
  })
  
  vect2 <- vect2 + sapply(k, function(k) {
    class.knn <- knn(data[,1:2], dataTest[,1:2], data$class, k)
    sk(class.knn, dataTest$class)
  })
}
vect2 <- vect2/10;
vect <- vect/10;

png("skutecznosc.png");
par(mfrow = c(2,1))
plot(k, vect[1,], xlab = "k", ylab = "skutecznosc (powtórne postawienie)")
plot(k, vect2[1,], xlab = "k", ylab = "skutecznosc (test)")
dev.off()

png("TP.png");
par(mfrow = c(2,1))
plot(k, vect[2,], xlab = "k", ylab = "True positive (powtórne postawienie)")
plot(k, vect2[2,], xlab = "k", ylab = "True positive (test)")
dev.off()

png("TN.png");
par(mfrow = c(2,1))
plot(k, vect[3,], xlab = "k", ylab = "True negative (powtórne postawienie)")
plot(k, vect2[3,], xlab = "k", ylab = "True negative (test)")
dev.off()


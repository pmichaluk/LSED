library(MASS)
library(rpart)
library(rpart.plot)
library(Hmisc)

rm(list=ls())

data <- iris[1:(length(iris[,1])*0.8),];
newdata <- setdiff(iris, data);
colnames(data)[colnames(data)=="Species"] <- "class"
colnames(newdata)[colnames(newdata)=="Species"] <- "class"
data$class <- factor(data$class)
newdata$class <- factor(newdata$class)

err.rate <- function(org.class, pred.class) {
  CM <- table(org.class, pred.class)
  return(1 - sum(diag(CM)) / sum(CM))
}

bagging.own <- function(data, N) {
  dane <- replicate(N, sample(1:nrow(data), rep = T))
  lda <- lapply(1:N, function(i) lda(class ~ ., data = data[dane[,i],], maxdepth = 1))
  tmp <- list(dane = dane)
  tmp$N <- N
  tmp$data <- data
  tmp$lda <- lda
  tmp1 <- bagging.own.prediction(tmp, data)
  tmp$lda.class <- tmp1$lda.class
  tmp$votes <- tmp1$votes
  tmp$class <- tmp1$class
  tmp$err <- tmp1$err
  return(tmp)
}

bagging.own.prediction <- function(bag, data) {
  tmp <- list()
  lda.class <- sapply(1:bag$N, function(i) 
    predict(bag$lda[[i]], newdata = data, type = "class")$class
  )
  votes <- t(sapply(1:nrow(lda.class), function(i) table(factor(lda.class[i,], levels = levels(data$class)))))
  class <- factor(levels(data$class)[apply(votes, 1, which.max)], levels = levels(data$class))
  tmp$lda.class <- lda.class
  tmp$votes <- votes
  tmp$class <- class
  tmp$err <- err.rate(data$class, tmp$class)
  return(tmp)
} 
values <- c(1, 5, 10, 20, 50)
tab <- sapply(values, function(v) replicate(10, bagging.own(data, v)$err))
tab.new <- sapply(values, function(v) replicate(10, bagging.own.prediction(bagging.own(data, v), newdata)$err))
tab.m <- apply(tab, 2, mean)
tab.s <- apply(tab, 2, sd)
tab.new.m <- apply(tab.new, 2, mean)
tab.new.s <- apply(tab.new, 2, sd)
errbar(values, tab.m, tab.m + tab.s, tab.m - tab.s, ylim = c(0.0, 0.05))
errbar(values, tab.new.m, tab.new.m + tab.new.s, tab.new.m - tab.new.s, add = T, col = "red", errbar.col = "red")

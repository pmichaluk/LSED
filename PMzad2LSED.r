library(MASS)
library(klaR)
library(e1071)
library(mvtnorm)

rm(list=ls())

CM.large1 <- function(org.class, pred.class) {
  
  CM <- table(org.class, pred.class)
  # Skuteczność klasyfikatora
  ACC <- sum(diag(CM)) / sum(CM)
  
  return(c(ACC = round(ACC,4), row.names = NULL))
}

CM.large <- function(org.class, pred.class) {
  
  CM <- table(org.class, pred.class)
  
  # Skuteczność klasyfikatora
  ACC <- sum(diag(CM)) / sum(CM)
  
  # Wartości true positive i true negative
  # zakładamy, że klasa "2" jest "pozytywna"
  TP <- CM[2,2]
  TN <- CM[1,1]
  
  sums <- apply(CM, 1, sum)
  
  TPR <- TP / sums[2]
  FPR <- 1 - TN / sums[1]
  
  return(c(ACC = round(ACC,4), TP = TP, TN = TN, TPR = round(TPR, 4), FPR = round(FPR, 4), row.names = NULL))
}


PRINT.params <- function(data) {
  class.lda <- lda(class ~., data)
  class.qda <- qda(class ~., data)
  class.nb <- naiveBayes(as.factor(class) ~ ., data) 
  data.lda.old <- predict(class.lda, data)
  data.qda.old <- predict(class.qda, data)
  data.nb.old <- predict(class.nb, data)
  res.old <- CM.large(data$class, data.lda.old$class)
  res.old <- rbind(res.old, CM.large(data$class, data.qda.old$class))
  res.old <- rbind(res.old, CM.large(data$class, data.nb.old))
  rownames(res.old) <- c("LDA", "QDA", "NB") 
  print(paste("Parametry klasyfikatorow dla ",
              toString(length(data)-1),"obserwacji:"))
  print(res.old)
}

  allData = data.frame(read.csv(file.choose() , header=TRUE, sep=",", row.names=NULL))
  colnames(allData) <- c("class","alcohol","malic-acid","ash","alcalinity-of-ash","magnesium","total-phenols",
  "flavanoids","nonflavanoid-phenols","proanthocyanins","color-intensity","hue","od280od315","proline")
  allData <- allData[sample(nrow(allData)),]

data = allData;
PRINT.params(data);
data2= allData[,1:3];
PRINT.params(data2);
data5 = allData[,1:6];
PRINT.params(data5);
data10 = allData[,1:11];
PRINT.params(data10);

halfLength = as.integer(length(allData[,1])/2);
quarterLength = as.integer(halfLength/2);
leftLength = length(allData[,1] - halfLength - quarterLength);
                    
PU = allData[1:halfLength,1:3]
PW = allData[(1+halfLength):(halfLength+quarterLength),1:3]
PT = allData[(1+halfLength+quarterLength):length(allData[,1]),1:3]


class.lda <- lda(class ~ ., PU)
class.qda <- qda(class ~ ., PU)
class.nb <- naiveBayes(as.factor(class) ~ ., PU) 


data.lda.old <- predict(class.lda, PW)
data.qda.old <- predict(class.qda, PW)
data.nb.old <- predict(class.nb, PW)


res.old <- CM.large(PW$class, data.lda.old$class)
res.old <- rbind(res.old, CM.large(PW$class, data.qda.old$class))
res.old <- rbind(res.old, CM.large(PW$class, data.nb.old))
dataRownames <- c("LDA", "QDA", "NB") 
rownames(res.old) <- dataRownames

print("Parametry klasyfikatorow:")
print(res.old)
res.old.df <- as.data.frame(res.old)
index = which.max(res.old.df$ACC)
print(paste("Wybrano metode ",dataRownames[index]))
print(paste("Wynik metody",dataRownames[index],"to: "))

if (        dataRownames[index] == "LDA" ) {
  prediction <- predict(class.lda, PT)$class
} else if ( dataRownames[index] == "QDA" ) {
  prediction <- predict(class.qda, PT)$class
} else if ( dataRownames[index] == "NB" ) {
  prediction <- predict(class.nb, PT)
}

result <- CM.large1(PT$class, prediction)
print(as.numeric(result[1]))

data = data2;
k = 5
l <- length(data2[,1])
accSum = 0.0;
s <- sapply(1:k, function(i) {
  PT = data2[(1+(i-1)*as.integer(l/k)):(i*as.integer(l/k)),];
  PU = setdiff(data2, PT);
  class.lda <- lda(class ~ ., PU)
  lda.predictions <- predict(class.lda, PT)
  accSum + as.numeric(CM.large1(PT$class, lda.predictions$class))
})

print("Kroswalidacja LDA: ")
print(sum(s)/k)
  

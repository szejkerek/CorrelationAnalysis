
#WCZYTYWANIE DANYCH
dane <- read.delim("dane.txt")
wzrost <- dane[ , c("wzrost")]  
plec <- dane[ , c("plec")]  
waga <- dane[ , c("waga")]  


#ZADANIE 1
mean(wzrost)
sd(wzrost)
quantile(wzrost)
hist(wzrost)
hist(waga)

#ZADANIE 2
chlopcy <- subset(dane,plec==1)
dziewczyny <- subset(dane,plec==0)

modelReglinp <- function(X){
  return(lm(X$wzrost ~ X$waga))
}

reglinp = function(X, name){
  fit <- modelReglinp(X)
  summary(fit)
  
  plot(X$waga, X$wzrost, main = name, xlab = "Waga, kg", ylab = "Wzrost, cm") 
  abline(fit)
  
}

 reglinp(chlopcy, "chlopcy")
 reglinp(dziewczyny, "dziewczyny")
 reglinp(dane, "chlopcy + dziewczyny")
 
 
#Zadanie 3

prediction <- function(dane){
 
  model <- modelReglinp(dane)
  # 1. Add predictions 
  pred.int <- predict(model, interval = "prediction")
  mydata <- cbind(dane, pred.int)
  # 2. Regression line + confidence intervals
  library("ggplot2")
  p <- ggplot(mydata, aes(dane$waga, dane$wzrost)) +
    geom_point() +
    stat_smooth(method = lm)
  # 3. Add prediction intervals
  p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
    geom_line(aes(y = upr), color = "red", linetype = "dashed")

}


prediction(chlopcy)
prediction(dziewczyny)
prediction(dane)



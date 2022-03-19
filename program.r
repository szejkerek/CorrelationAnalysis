
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

reglinp = function(X, name){
  fit <- lm(X$wzrost ~ X$waga)
  summary(fit)
  
  plot(X$waga, X$wzrost, main = name, xlab = "Waga, kg", ylab = "Wzrost, cm") 
  abline(fit)
}

reglinp(chlopcy, "chlopcy")
reglinp(dziewczyny, "dziewczyny")
reglinp(dane, "chlopcy + dziewczyny")

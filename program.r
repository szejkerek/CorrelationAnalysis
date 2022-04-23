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
  return(fit)
}

reg_chlopcy <- reglinp(chlopcy, "chlopcy")
reg_dziewczyny <- reglinp(dziewczyny, "dziewczyny")
reg_wszyscy <- reglinp(dane, "chlopcy + dziewczyny")

#Zadanie 3
library("MASS")
fitdist_chlopcy<- MASS::fitdistr(reg_chlopcy$coefficients, "normal")
fitdist_dziewczyny<- MASS::fitdistr(reg_dziewczyny$coefficients, "normal")
fitdist_wszyscy<- MASS::fitdistr(reg_wszyscy$coefficients, "normal")
fitdist_chlopcy
fitdist_dziewczyny
fitdist_wszyscy
xdd <- reg_chlopcy[["coefficients"]][["(Intercept)"]]
xdd
wektor_wsp_kier <- c(reg_chlopcy[["coefficients"]][["(Intercept)"]],
               reg_dziewczyny[["coefficients"]][["(Intercept)"]],
               reg_wszyscy[["coefficients"]][["(Intercept)"]])
wektor_wsp_kier
wektor_wyr_wolny <-  c(reg_chlopcy[["coefficients"]][["X$waga"]],
                       reg_dziewczyny[["coefficients"]][["X$waga"]],
                       reg_wszyscy[["coefficients"]][["X$waga"]])
wektor_wyr_wolny
bl_standardowy <- function(x) sd(x)/sqrt(length(x))
bl_standardowy(wektor_wsp_kier)
bl_standardowy(wektor_wyr_wolny)
#błędy są na tyle małe, że 

#Zadanie 4

prediction <- function(dane){
  
  model <- modelReglinp(dane)

  pred.int <- predict(model, interval = "prediction")
  mydata <- cbind(dane, pred.int)

  library("ggplot2")
  p <- ggplot(mydata, aes(dane$waga, dane$wzrost)) +
    geom_point() +
    stat_smooth(method = lm)

  p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
    geom_line(aes(y = upr), color = "red", linetype = "dashed")
  
}


prediction(chlopcy)
prediction(dziewczyny)
prediction(dane)

#Zadanie 5
resiDUDES <- residuals(reg_chlopcy)
resiGURLS <- residuals(reg_dziewczyny)
resiALL <- residuals(reg_wszyscy)
hist(resiDUDES) 
hist(resiGURLS)
hist(resiALL)
#if p-value > 0.05 then the distribution can be considered as normal
shapiro.test(resiDUDES) #not-normal distribution
shapiro.test(resiGURLS) #normal distribution
shapiro.test(resiALL)   #normal distribution


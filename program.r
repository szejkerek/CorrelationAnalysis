
#WCZYTYWANIE DANYCH
dane <- read.delim("dane.txt")
wzrost <- dane[ , c("wzrost")]  
plec <- dane[ , c("plec")]  
waga <- dane[ , c("waga")]  

#WZROST
mean(wzrost)
sd(wzrost)

hist(wzrost)


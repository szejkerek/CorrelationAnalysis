library(tidyverse)
library(ggrepel)



#WCZYTYWANIE DANYCH
dane <- read.delim("dane2.txt")
wzrost <- dane[ , c("wzrost")]
plec <- dane[ , c("plec")]
waga <- dane[ , c("waga")]

#WZROST
mean(wzrost)
sd(wzrost)

sex <- ggplot(data=dane,aes(x=plec,y=wzrost))+geom_boxplot()+xlab("płeć")+ylab("wzrost")
sex
summary(dane)
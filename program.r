#  WCZYTYWANIE BIBLIOTEK
library("MASS")
library("ggplot2")

# WCZYTYWANIE DANYCH
data <- read.delim("dane.txt")

height_data <- data[ , c("wzrost")]  
gender_data <- data[ , c("plec")]  
weight_data <- data[ , c("waga")]  


# ZADANIE 1
mean(height_data)
sd(height_data)
quantile(height_data)
hist(height_data)
hist(weight_data)

# ZADANIE 2
boys  <- subset(data, plec==1)
girls <- subset(data, plec==0)

model_reglinp <- function(X) {
  return( lm(X$wzrost ~ X$waga) )
}

reglinp = function(X, name){
  fit <- model_reglinp(X)

  summary(fit)
  plot(X$waga, X$wzrost, main = name, xlab = "Waga, kg", ylab = "Wzrost, cm") 
  abline(fit)

  return(fit)
}

boys_model     <- reglinp(chlopcy,    "chlopcy")
girls_model    <- reglinp(dziewczyny, "dziewczyny")
everyone_model <- reglinp(data,       "chlopcy + dziewczyny")

# Zadanie 3
slope_coefficients <- c(reg_chlopcy[["coefficients"]][["(Intercept)"]],
                        reg_dziewczyny[["coefficients"]][["(Intercept)"]],
                        reg_wszyscy[["coefficients"]][["(Intercept)"]])
slope_coefficients

const_term_coefficients <-  c(reg_chlopcy[["coefficients"]][["X$waga"]],
                              reg_dziewczyny[["coefficients"]][["X$waga"]],
                              reg_wszyscy[["coefficients"]][["X$waga"]])
const_term_coefficients

std_err <- function(x) sd(x) / sqrt(length(x))

std_err(slope_coefficients)
std_err(const_term_coefficients)

# Zadanie 4
prediction <- function(data) {
  model <- model_reglinp(data)

  pred.int <- predict(model, interval = "prediction")
  mydata   <- cbind(data, pred.int)

  p <- ggplot(mydata, aes(data$waga, data$wzrost)) +
       geom_point() +
       stat_smooth(method = lm)

  p + geom_line(aes(y = lwr), color = "red", linetype = "dashed") +
      geom_line(aes(y = upr), color = "red", linetype = "dashed")
}


prediction(boys)
prediction(girls)
prediction(data)

# Zadanie 5
resiDUDES <- residuals(boys_model)
resiGURLS <- residuals(girls_model)
resiALL   <- residuals(everyone_model)

hist(resiDUDES) 
hist(resiGURLS)
hist(resiALL)

# jeżeli p-value > 0.05, to rozkład danych możemy uznać za normalny
shapiro.test(resiDUDES) # rozkład nie jest normalny
shapiro.test(resiGURLS) # rozkład normalny
shapiro.test(resiALL)   # rozkład normalny


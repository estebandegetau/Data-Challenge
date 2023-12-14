#### TEA I #####################################################################
#'
#' @name 02_bootstrap.R
#' 
#' @description
#' Get empirical values of variance for otherwise unknown estimators' 
#' distributions.
#' 
#' @author Esteban Degetau 
#' 
#' @created 2023-08-31
#' 
#### Bootstrap #################################################################

rm(list = ls())
gc()

#---- Libraries ----------------------------------------------------------------

#---- Muestreo aleatorio simple ------------------------------------------------

nrow(iris)

selec <- sample(1:nrow(iris), 15, replace = T)

muestra <- iris[selec, ]

selec

muestra

alumnos<-c("JOSÉ ANGEL","SANDRA","JESÚS","SERGIO",
           "PAOLAB","MARCO","PERLA","GUSTAVO","PAOLAC",
           "ESTEBAN","HÉCTOR","ELISA","RODOLFO",
           "EDUARDO","RODRIGO","JULIETAO","JULIETAP",
           "RUBÉN","ANDREA","EUGENIO","JUAN CARLOS")

sample(alumnos, 4, replace = T)

# Para asegurar replicabilidad, hay que fijar una semilla

set.seed(31416)
sample(alumnos, 4, replace = T)

#---- Ej. Jackknife v Bootstrap ------------------------------------------------

# Estimador: coeficiente de variación


x <-c(8.26, 6.33, 10.4, 5.27, 5.35, 5.61, 6.12, 6.19, 5.2,
      7.01, 8.74, 7.78, 7.02, 6, 6.5, 5.8, 5.12, 7.41, 6.52, 6.21,
      12.28, 5.6, 5.38, 6.6, 8.74)

CV <- function(x) {sqrt(var(x)) / mean(x)}

CV(x)


sample(x, replace = T)


CV(sample(x, replace = T))


boot <- numeric(1000)

for (i in 1:1000) {boot[i] <- CV(sample(x, replace = T))}

# ¿Será normal?
boot |> hist(freq = F)
curve(dnorm(x, mean(boot), sqrt(var(boot))),
      0.1, 0.4, col = "red", add = T)

qqnorm(boot, col = "red")
qqline(boot, col = "blue")

quantile(boot, c(0.025, 0.975))
 

# CI asumiendo noralidad, aunque ya vimos que no lo es
linf <- CV(x) - abs(qnorm(0.025) * sqrt(var(boot)))
lsup <- CV(x) + abs(qnorm(0.025) * sqrt(var(boot)))


# Corrección por sesgo
bias <- mean(boot) - CV(x)

CV(x) - bias

linf <- CV(x) - bias - abs(qnorm(0.025) * sqrt(var(boot)))
lsup <- CV(x) - bias + abs(qnorm(0.025) * sqrt(var(boot)))

# Jackknife
jack <- numeric(length(x) - 1)
pseudo <- numeric(length(x))
for(i in 1:length(x)) {
for (j in 1:length(x)) {if(j < i) jack[j] <- x[j]
else if(j > i) jack[j - 1] <- x[j]
pseudo[i] <- length(x) * CV(x) - (length(x) - 1) * CV(jack)}
  }
  
  
#' Jackknife es arbitrario en cuanto a que se escoja 1 como el número de obs
#' botadas. También, solo permite n distintas observaciones del estimador,
#' cuando en Bootstrap se pueden sacar muchas más submuestras distintas.
#' 

hist(jack)

#---- Con paquete --------------------------------------------------------------

set.seed(123)
pacman::p_load(boot)

rsq_function <- function(formula, data, obs) {
  d <- data[obs,]
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
}
  

reps <- boot(data=mtcars,
             statistic=rsq_function,
             R=5000,
             formula=mpg~wt)

reps

plot(reps)

boot.ci(reps, type = "basic")

boot.ci(reps, type = "all")

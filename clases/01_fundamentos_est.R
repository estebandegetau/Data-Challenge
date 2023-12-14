#### TEA I #####################################################################
#' 
#' @name 01_fundamentos_est.R
#' 
#' @description
#' Fundamentos de estadística - herramientas de inferencia en R.
#' 
#' @author Esteban Degetau
#' 
#' @created 2023-08-24
#' 
#### Fundamentos de estadística ################################################

rm(list = ls())
gc()

#---- Libraries ----------------------------------------------------------------

#---- Distribuciones discretas -------------------------------------------------

plot(dbinom(0:10, 10, 0.6),
     type = "h", xlab = "k", ylab = "P(X = k)",
     main = "Función de Probabilidad B(10, 0.6)")

plot(stepfun(0:10, pbinom(0:11, 10, 0.6)),
     xlab = "k", ylab = "F(k)",
     main = "Función de distribución B(0, 0.6)")

#---- Verificación empírica ----------------------------------------------------

x <- rbinom(10000, 10, 0.6)

(freqAbs = table(x))

head(x)

(freqRel = prop.table(freqAbs))

probsTeo <- data.frame(x = 0:10, Prob = dbinom(0:10, 10, 0.6))
freqRel <- as.data.frame(freqRel)
str(freqRel)

?str

freqRel$x <- as.integer(as.character(freqRel$x))
str(freqRel)

compara <- merge(freqRel, probsTeo, all = T)

compara


with(compara,{
  plot(x, Freq, type = "b")
  points(x, Prob, col = "red", pch = 4)
  lines(x, Prob, col = "red", lty = 2, lwd = 2)
  legend("topleft", c("frec.relativa", "probabilidad"),
         col = c("black", "red"), lty = 1:2, pch = c(1, 4))
})

#---- Dist de prob contínua ----------------------------------------------------

curve(dnorm(x, 170, 12), xlim = c(130, 210), col = "blue", lwd = 2,
      xlab = "x", ylab = "f(x)", 
      main = "Función de Densidad N(170, 12)")

curve(pnorm(x, 170, 12), xlim = c(130, 210), col = "red", lwd = 2,
            xlab = "x", ylab = "F(x)",
            main = "Función de Distribución N(170, 12)")

pnorm(168, 170, 12) - pnorm(150, 170, 12)

regionX = seq(150, 168, 0.01)
xP <- c(150, regionX, 168)
yP <- c(0, dnorm(regionX, 170, 12), 0)
curve(dnorm(x, 170, 12), xlim = c(130, 210),
      yaxs = "i", ylim = c(0, 0.035), ylab = "f(x)",
      main = "Densidad N(170, 12)")
polygon(xP, yP, col = "orange1")
box()

X <- rnorm(10000, 170, 12)
hist(X, freq = F, col = "red",
     main = "Histograma", sub = "Datos simulados N(170, 12)"
     )

curve(dnorm(x, 170, 12), xlim = c(110, 220),
      col = "blue", lwd = 2, add = T)

plot(ecdf(X))
curve(pnorm(x, 170, 12), xlim = c(110, 220),
      col = "red", lwd = 2, lty = 2, add = T)
legend("topleft", lty = c(1, 2), lwd = c(1, 2),
       col = c("black", "red"),
       legend = c("Distribución empírica",
                  "Distribución teórica"))


df1 <- 25
df2 <- 30
qmin <- 0.5
qmax <- 1.5
region <- seq(0,7,0.01)
xT <- c(qmin,region,qmax)
yT <- c(0,df(region,df1,df2),0)
regionX=seq(qmin,qmax,0.01)
xP <- c(qmin,regionX,qmax)
yP <- c(0,df(regionX,df1,df2),0)
curve(df(x,df1,df2),xlim=c(0,7),col="black",
      main=paste("Distribución F (",df1,",",df2,")"))
polygon(xT,yT,col="red")
polygon(xP,yP,col="white")
box()

#---- Funciones ----------------------------------------------------------------

die <- 1:6
mean(die)
sample(x = die, size = 6)

sample(x = die, size = 6, replace = T)


mifuncion <- function(){}

roll <- function() {
  
  die <- 1:6
  dice <- sample(die, size = 2, replace = T)
  sum(dice)
}

roll()

#---- Teorema central del límite -----------------------------------------------

n <- 25
muestra1 <- rnorm(n, 170, 12)
media1 = mean(muestra1)
media1

muestra2 <- rnorm(n, 170, 12)
media2 = mean(muestra2)
media2

mediaMuestral <- function(n){
  muestra = rnorm(n, 170, 12)
  media = mean(muestra)
  return(media)
}

mediaMuestral(25)

mediaMuestral(25)

m = 10000

muchasMedias <- replicate(m, mediaMuestral(25))
head(muchasMedias)


mean(muchasMedias)
sd(muchasMedias)

12/sqrt(n)

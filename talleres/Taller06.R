if (!require('devtools')) install.packages('devtools')
devtools::install_github('fhernanb/model', force=TRUE)

# Activación de librerías

library(readxl)
library(ggplot2)
library(stargazer)
library(model)
library(ggfortify) 
library(datarium)

# Lectura de los datos

data("marketing")
datos<-as.data.frame(marketing)
attach(datos)
View(datos)

# Análisis de correlaciones

cor(datos)

# Diagrama de dispersión

ggplot(data=datos, aes(x = youtube, y = sales)) +
  geom_point(color = "Steelblue") +
  labs(
    title ="Ventas y anuncios en youtube",
    subtitle = "(en miles de dólares)",
    caption = "Fuente: marketing data",
    x="youtube",
    y="sales") +
  geom_smooth(method = lm,col="red",se=FALSE)+
  theme_light()

# Estimación del modelo simple

modelo1<-lm(sales~youtube,data=datos)
summary(modelo1)
stargazer(modelo1,type="text")

modelo2<-lm(sales~facebook,data=datos)
summary(modelo2)
stargazer(modelo2,type="text")

modelo3<-lm(sales~newspaper,data=datos)
summary(modelo3)
stargazer(modelo3,type="text")

stargazer(modelo1,modelo2,modelo3,type="text")



# Pruebas de hipótesis

# Ho: beta=0, H1: beta=/=0

beta_test(modelo1, parm ="youtube", ref.value=0,
          alternative = "two.saided")

# Intervalo de confianza 

confint(modelo1,parm="youtube",level = 0.95)
confint(modelo2,parm="facebook",level = 0.95)
confint(modelo3,parm="newspaper",level = 0.95)

# Predicción

predict(modelo1, data.frame(youtube=150))
predict(modelo1, data.frame(youtube=150),interval="confidence", level=0.95)
predict(modelo2, data.frame(facebook=150),interval="confidence", level=0.95)
predict(modelo3, data.frame(newspaper=150),interval="confidence", level=0.95)


# Estimación del modelo múltiple
modelo4<-lm(sales~youtube+facebook+newspaper,data=datos)
summary(modelo4)
stargazer(modelo4,type="text")

predict(modelo4, data.frame(youtube=100,facebook=30,newspaper=20),interval="confidence", level=0.95)
predict(modelo4, data.frame(youtube=150,facebook=0,newspaper=0),interval="confidence", level=0.95)
predict(modelo4, data.frame(youtube=0,facebook=150,newspaper=0),interval="confidence", level=0.95)


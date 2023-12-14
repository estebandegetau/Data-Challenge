#### T EA I ##################################################
#' 
#' @name 08_errores_estandar.R
#' 
#' @description Inferencia estadistica con robustez
#' 
#' @describeIn 
#' 
#### Errores estandar ########################################

pacman::p_load(dplyr, ggplot2, wooldridge)

data("saving")

saving <- saving |>
    filter(sav > 0, inc < 20000, sav < inc)

# Grafica
ggplot(saving, aes(x = inc, y = sav)) +
    geom_point() +
    geom_smooth(method = "lm", se = F) +
    labs(title = "Ahorro e ingreso",
         x = "Ingreso anual", 
         y = "Ahorro anual")

# Regresion
pacman::p_load(equatiomatic, lmtest, sandwich, stargazer)

modelo <- lm(sav ~ inc, data = saving)
summary(modelo)

coeftest(modelo, vcov = vcovHC(modelo, type = "HC1"))


modelo_nr <- lm(sav ~ inc + size + educ + age, data = saving)

anova(modelo, modelo_nr)

waldtest(modelo, modelo_nr, vcov = vcovHC(modelo_nr, type = "HC1"))

cov1 <- vcovHC(modelo, type = "HC1")
robust_se <- sqrt(diag(cov1))
stargazer(modelo, modelo, type = "text", se = list(NULL, robust_se))

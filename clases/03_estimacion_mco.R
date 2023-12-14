#### TAE I #####################################################################
#' 
#' @name 03_estimacion_mco.R
#' 
#' @description
#' Estimación por MCO para análisis de regresión. Permite obtener mejores 
#' estimadores con pocos supuestos. Serán insesgados y de mínima varianza, i.e. 
#' eficientes. 
#' 
#' @describeIn here("datos", "SALARIOS.xls") Wooldrige
#' 
#' @author Esteban Degetau
#' 
#' @created 2023-09-07
#' 
#### Estimación por MCO ########################################################

rm(list = ls())
gc()

#---- Libraries ----------------------------------------------------------------

pacman::p_load(tidyverse,
               readxl,
               here)

#---- Read ---------------------------------------------------------------------

salarios <- here("datos", "SALARIOS.xls") |> read_xls()

head(salarios)

datos <- salarios

attach(datos)

View(datos)

#---- Análisis exploratorio ----------------------------------------------------

pacman::p_load(ggplot2, ggthemes)

datos |> ggplot(aes(EDAD, SAL)) +
  geom_point(color = "steelblue") +
  geom_smooth(color = "tomato") +
  scale_y_continuous(label = scales::dollar,
                     limits = c(0, 1000)) +
  scale_x_continuous(breaks = seq(10, 65, 5),
                     limits = c(10, 65)) +
  labs(x = "Edad",
       y = "Salario",
       title = "Salario vs. Edad",
       subtitle = "(114 observaciones)") +
  # theme_economist(base_family = "Verdana") +
  # scale_color_economist() +
  theme_wsj()

datos |> ggplot(aes(x = factor(EDUC,
                               labels = c("Hasta primaria",
                                          "Hasta Secundaria",
                                          "Hasta Media Superior",
                                          "Hasta Superior")),
                    y = SAL)) +
  geom_boxplot() +
  labs(title = "Salario por grado educativo",
       x = "Nivel Educativo",
       y = "Salario") +
  scale_y_continuous(labels = scales::dollar,
                     limits = c(0, 1000)) +
  theme_stata()

#---- Tablas -------------------------------------------------------------------

pacman::p_load(dplyr, kableExtra)

calc <- datos |>
  group_by(M) |>
  summarise(n = n(),
            mean = mean(SAL),
            sd = sd(SAL),
            min = min(SAL),
            max = max(SAL))

kable(calc,format = "pipe")

#---- Regresión ----------------------------------------------------------------

ED1 <- as.numeric(EDUC == 1)
ED2 <- as.numeric(EDUC == 2)
ED3 <- as.numeric(EDUC == 3)

ml1 <- lm(SAL ~ ED1 + ED2 + ED3)
ml2 <- lm(SAL ~ ED1 + ED2 + ED3 + EDAD)
ml3 <- lm(SAL ~ ED1 + ED2 + ED3 + EDAD + PER)
ml4 <- lm(SAL ~ ED1 + ED2 + ED3 + EDAD + PER + M)

pacman::p_load(stargazer)

stargazer(ml1, ml2, ml3, ml4, type = "text")

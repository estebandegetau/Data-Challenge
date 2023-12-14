#### Data Challenge ############################################################
#' 
#' @name 06_another_mosnter.R
#' 
#' @description Create a monster based on prediction differences across GLM
#'              models with and without interactions.
#' 
#' @describeIn here("resultados", "supervised_models.RData") 
#'              Supervised models.
#' 
#' @author Esteban Degetau
#' 
#' @created 2023-11-06
#' 
#### Another monster ##########################################################

rm(list = ls())
gc()

#---- Libraries ---------------------------------------------------------------

pacman::p_load(here,
               tidyverse,
               caret,
               glmnet,
               ranger,
               e1071,
               clValid,
               mlr,
               ggplot2,
               here,
               fixest,
               gtsummary)

#---- Models ------------------------------------------------------------------

load(here("resultados", "supervised_models.RData"))

models_long <- models |>
    pivot_longer(
        cols = matches("model"),
        names_to = "model_name",
        values_to = "model",
        #Remode "model" from name
        names_pattern = "(.*)_model") 


#---- Simualte monsters -------------------------------------------------------

set.seed(20231106)

n <- 1000

monsters <- tibble(
    bone_length = runif(n, 0, 1),
    rotting_flesh = runif(n, 0, 1),
    hair_length = runif(n, 0, 1),
    has_soul = runif(n, 0, 1),
    color = sample(c("white", "black", "clear", "blue", "green", "blood"), n, replace = TRUE)
    ) |>
# Add color dummies
fastDummies::dummy_cols("color", remove_selected_columns = TRUE) |>
# Add interactions
    mutate(
        bone_length_rotting_flesh = bone_length * rotting_flesh,
        bone_length_hair_length = bone_length * hair_length,
        bone_length_has_soul = bone_length * has_soul,
        rotting_flesh_hair_length = rotting_flesh * hair_length,
        rotting_flesh_has_soul = rotting_flesh * has_soul,
        hair_length_has_soul = hair_length * has_soul,
    ) 

#---- Predictions -------------------------------------------------------------

my_predict <- function(model) {
    predict(model, monsters, type = "raw")
}

my_prob <- function(model) {
    predict(model, newdata = monsters, type = "prob") 
}

predictions <- models_long |>
    mutate(
        pred = map(model, my_predict),
        prob = map_df(model, my_prob)
    )

a <- predict(models$glm_model[[3]], newdata = monsters, type = "prob")
str(a)
class(a)

predictions

#---- Operate -----------------------------------------------------------------

discrepancies <- monsters |>
    filter(predicted_numeric != predicted_interactions) 

perc_discrepancies <- discrepancies |> 
    nrow() / nrow(monsters) * 100

monsters |>
    group_by(predicted_numeric, predicted_interactions) |>
    tally() |>
    ungroup() |>
    group_by(predicted_numeric) |>
    mutate(n = n / sum(n) * 100) |>
    pivot_wider(names_from = predicted_numeric, values_from = n) 

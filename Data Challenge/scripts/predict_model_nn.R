#### Data Challenge ############################################################
#' 
#' @name predict_model_nn.R
#' 
#' @description Predicts the outcome of the test set using the neural network
#' 
#' @describeIn here("resultados", "supervised_models.RData") Trained models
#'             here("datos", "test_set.csv")
#' 
#' @return A .csv file with monster id and the predicted outcome.
#' 
#' @author Esteban Degetau
#' 
#' @created 2023-11-08
#' 
#### Predict Model NN #########################################################

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

model_nn <- models$nn_model[[1]]

#---- Data --------------------------------------------------------------------

# test <- read_csv(here("datos", "test_set.csv"))

test <- read_csv(here("datos", "MonsterCienMil.csv"))

#---- Predictions -------------------------------------------------------------

test %>%
  mutate(type = predict(model_nn, .)) |>
 # select(id, type) |>
  filter(id == 35185)

  # write_csv(here("resultados", "test_set_predictions_nn.csv"))



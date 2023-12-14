#### Data challenge ############################################################
#'
#' @name 03_supervised.R
#' 
#' @description Performs supervised methods on the data.
#' 
#' @describeIn here("datos", "train_set.csv")
#' 
#' @Author Esteban Degetau
#' 
#' @created 2023-11-01
#' 
#### Supervised methods ########################################################

rm(list = ls())
gc()

#---- Libraries ---------------------------------------------------------------

pacman::p_load(
    here,
    fastDummies,
    tidyverse,
    ggplot2,
    fpc,
    caret,
    glmnet,
    ranger,
    e1071,
    clValid,
    mlr
    )

library(dplyr)

#---- Data --------------------------------------------------------------------

train <- read_csv("~/Documents/Maestria/Econometría/Data Challenge /train_set.csv") |>
    mutate(
        color = as_factor(color)
    ) |>
    rowid_to_column()


test <- read_csv("~/Documents/Maestria/Econometría/Data Challenge /test.csv") |>
    mutate(
        color = as_factor(color)
    ) |>
    rowid_to_column()

#---- Data preparation --------------------------------------------------------



data_train <- train[,-6] |>
    mutate(data_id = "numeric") |>
     as.data.frame() 

data_test <- test[,-6] |> 
  as.data.frame()



#---- Model ------------------------------------------------------------------

myControl <- trainControl(
	  method = "cv", 
	  number = 10,
	  verboseIter = TRUE
  )


model <- caret::train(type ~ bone_length + rotting_flesh + hair_length + has_soul,
             data = data_train,
             method = "glmnet",
             tuneGrid = expand.grid(
               alpha = 0:1,
               lambda = seq(0.0001, 1, length = 20)),
             trControl = myControl
             )

#---- Limpiar base test --------------------------------------------

test <- test[,-c(1,6)]

test <- test %>% 
  mutate(predict(model,test))










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
    mlr,
    arm
    )

#---- Data --------------------------------------------------------------------

## Cambiar ruta a datos
train <- read_csv(here("datos", "train_set.csv")) |>
    mutate(
        color = as_factor(color)
    ) |>
    rowid_to_column()

summary(train)

#---- Data preparation --------------------------------------------------------


set.seed(20231110)
training <-  sample(1:300, 300)



work <- train |>
    select(!color) |>
    mutate(data_id = "numeric") |>
     as.data.frame() 

work_color <- train |>
  fastDummies::dummy_cols("color", remove_selected_columns = T) |>
  mutate(data_id = "color dummies") |>
  as.data.frame()

# Add every interaction term between numeric variables
work_int <- train |>
    select(!color) |>
    mutate(
        bone_length_rotting_flesh = bone_length * rotting_flesh,
        bone_length_hair_length = bone_length * hair_length,
        bone_length_has_soul = bone_length * has_soul,
        rotting_flesh_hair_length = rotting_flesh * hair_length,
        rotting_flesh_has_soul = rotting_flesh * has_soul,
        hair_length_has_soul = hair_length * has_soul
    ) |>
    mutate(data_id = "interactions") |>
    as.data.frame()


full <- train |>
  fastDummies::dummy_cols("color", remove_selected_columns = T) |>
  mutate(
    bone_length_rotting_flesh = bone_length * rotting_flesh,
    bone_length_hair_length = bone_length * hair_length,
    bone_length_has_soul = bone_length * has_soul,
    rotting_flesh_hair_length = rotting_flesh * hair_length,
    rotting_flesh_has_soul = rotting_flesh * has_soul,
    hair_length_has_soul = hair_length * has_soul
  ) |>
    mutate(data_id = "color dummies and interactions") |>
    as.data.frame()

data <- full_join(work, work_color) |>
    full_join(work_int) |>
    full_join(full) |>

    nest(.by = data_id) 



#---- Models ------------------------------------------------------------------

## Estos son los modelos, creo que van en scripts --> 03_supervised.r 

myControl_neural <- trainControl(
	  method = "cv", 
	  number = 10,
	  verboseIter = TRUE
  )

# Neural Network
my_nn <- function(data) {
    caret::train(
        type ~ .,
        data = data,
        method = "nnet",
        trControl = myControl,
        linout = TRUE
    )
}

# Model Averaged Neural Network
my_avnn <- function(data) {
  caret::train(
    type ~ .,
    data = data,
    method = "avNNet",
    repeats = 15,
    trace = FALSE
  )
}


my_bgl <- function(data) {
  caret::train(
    type ~ .,
    data = data,
    method = "bayesglm",
  )
}

#---- Train on different data sets --------------------------------------------

models <- data |>
  mutate(
    data = map(data, ~ dplyr::select(.x, !rowid)),
    data = map(data, ~ dplyr::select(.x, type, where(~ (sum(is.na(.x)) == 0)))),
    nn_model = map(data, my_nn), 
    avnn_model = map(data, my_avnn),
    bgl_model = map(data, my_avnn)
    
  ) 

## Agregar últimas tres líneas al models del script: scripts --> 03_supervised.r
## para que queden guardados en resultados --> supervised_models.RData

#---- Save models -------------------------------------------------------------

save(models, file = here("resultados", "neural_networks.RData"))

#---- Compare model fit -------------------------------------------------------

## Esto va en el Quarto document

# Neural Network
resampled_nn <- resamples(models$nn_model |> set_names(models$data_id))


dotplot(resampled_nn, metric = "Accuracy")

#Model Averaged Neural Network
resampled_avnn <- resamples(models$avnn_model |> set_names(models$data_id))


dotplot(resampled_avnn, metric="Accuracy")

#Bayesian Generalized Linear Model
resampled_bgl <- resamples(models$bgl_model |> set_names(models$data_id))


dotplot(resampled_bgl, metric="Accuracy")





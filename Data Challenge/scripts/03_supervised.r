#### Data challenge ############################################################
#'
#' @name 03_supervised.R
#' 
#' @description Performs supervised methods on the data.
#' 
#' @describeIn here("datos", "train_set.csv")
#' 
#' @Author Esteban Degetau;
#'         Andres Bermudez;
#'         Andrea Rancaño
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
    conflicted,
    kknn,
    arm
    )

conflicts_prefer(
    dplyr::filter(),
    dplyr::lag(),
    dplyr::select()
)


#---- Data --------------------------------------------------------------------


train <- read_csv(here("datos", "train_set.csv")) |>
    mutate(
        color = factor(color)
    ) 

#---- Data preparation --------------------------------------------------------



work <- train |>
    select(-color) |>
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

myControl <- trainControl(
	  method = "cv", 
	  number = 10,
	  repeats = 20, 
	  verboseIter = TRUE
  )


my_rf <- function(data) {
    caret::train(
        type ~ .,
        data = data,
        tuneLength = 3,
        method = "ranger",
        trControl = myControl,
        importance = "impurity"
    )
}


my_glm <- function(data) {
    caret::train(
        type ~ .,
        data = data,
        method = "glmnet",
        tuneGrid = expand.grid(
            alpha = 0:1,
            lambda = seq(0.0001, 1, length = 20)),
        trControl = myControl
    )
}

# Control for neural networks models
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
        trControl = myControl_neural,
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

my_kknn <- function(data) {
  caret::train(
    type ~ .,
    data = data,
    method = "kknn"
  )
}

#---- Train on different data sets --------------------------------------------

set.seed(20231106)

models <- data |>
    mutate(
        data = map(data, ~ select(.x, type, where(~ (sum(is.na(.x)) == 0)))),

        rf_model = map(data, my_rf),
        glm_model = map(data, my_glm),

        kknn_model = map(data, my_kknn), 

        nn_model = map(data, my_nn), # Works
        avnn_model = map(data, my_avnn, .progress = T), # Works
        bgl_model = map(data, my_bgl) #Works
    )


#---- Save models -------------------------------------------------------------

save(models, file = here("resultados", "supervised_models.RData"))







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
  dplyr::select(-color) |>
  mutate(data_id = "numeric") |>
  as.data.frame()

work_color <- train |>
  fastDummies::dummy_cols("color", remove_selected_columns = T) |>
  mutate(data_id = "color dummies") |>
  as.data.frame()

# Add every interaction term between numeric variables
work_int <- train |>
  dplyr::select(!color) |>
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



#---- Train on different data sets --------------------------------------------

models <- data |>
  mutate(
    data = map(data, ~ dplyr::select(.x, !rowid)),
    data = map(data, ~ dplyr::select(.x, type, where(~ (sum(is.na(.x)) == 0)))),
    nn_model = map(data, my_nn), 
  ) 

# Remove from RAM everything but the models
rm(list = ls()[! ls() %in% c("models")])

#---- Predict on test set -------------------------------------------------------

# Load test set
test_set <-read_csv(here("datos", "testing_set_maest.csv")) 


# NN model with numeric variables
model_a <- models$nn_model[[1]]

test_set$type <- predict(model_a, newdata= test_set, type= "raw")

#---- Save predictions --------------------------------------------------------

test_set |>
  dplyr::select(id, type) |>
  write_csv(here("resultados", "test_set_predictions_nn.csv"))

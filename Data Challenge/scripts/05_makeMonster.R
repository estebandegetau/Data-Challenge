rm (list = ls(all = TRUE))
setwd("/Users/arb/Desktop/MEAP/ECO_AP/DataChallenge")

#---- Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, 
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

#---- Data --------------------------------------------------------------------

# Read data
train <- read_csv(here("datos", "train_set.csv"))
monster <-read_csv(here("datos","MakeAMonster.csv"))
monster <- monster %>%
  rename(
    hair_length = Hair,
    bone_length = Bone,
    rotting_flesh = Flesh,
    has_soul = Soul
  )

#---- Models ------------------------------------------------------------------

load(here("resultados", "supervised_models.RData"))

model_a <- models$glm_model[[1]]

set.seed(20231110)
training <-  sample(1:300, 300)



work <- train |>
  select(!color) |>
  mutate(data_id = "numeric") |>
  as.data.frame() 

myControl <- trainControl(
  method = "cv", 
  number = 10,
  repeats = 20, 
  verboseIter = TRUE
)

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
resampled <- resamples(models$glm_model |> set_names(models$data_id))

summary(resampled)

dotplot(resampled, metric = "Accuracy")


# 3. Make predictions
monster$predictions <- predict(model_a, newdata= monster, type= "raw")
monster$confidence <- predict(model_a, newdata= monster, type= "prob")
head(monster)


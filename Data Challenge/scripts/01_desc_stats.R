#### Data challenge ############################################################
#' 
#' @name 01_desc_stats.R
#' 
#' @description Generates summary statistics for the data challenge.
#' 
#' @describeIn here::here("datos", "train_set.csv") 
#'  Training monsters
#' 
#' @author Esteban Degetau
#' 
#' @created 2023-10-31
#' 
#### Desc stats ################################################################

rm(list = ls())
gc()

#---- Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, 
    ggplot2, 
    here, 
    fixest,
    gtsummary)

#---- Data --------------------------------------------------------------------

# Read data
train <- read_csv(here("datos", "train_set.csv"))

#---- Summary stats -----------------------------------------------------------

### Distribution of Continuous Variables by Creature Type
#### Bone Length

ggplot(train, 
       aes(x = type, 
           y = bone_length, 
           fill = type)) + 
  geom_boxplot() +
  guides(fill = FALSE) + 
  xlab("Creature") + 
  ylab("Bone Length") +
  scale_fill_manual(values = c("#D55E00", "#0072B2", "#009E73"))


#### Rotting Flesh

ggplot(train, 
       aes(x = type, 
           y = rotting_flesh, 
           fill = type)) + 
  geom_boxplot() +
  guides(fill = FALSE) + 
  xlab("Creature") + 
  ylab("Percentage of Rotting Flesh") + 
  scale_fill_manual(values = c("#D55E00", "#0072B2", "#009E73"))


#### Hair Length

ggplot(train, 
       aes(x = type, 
           y = hair_length, 
           fill = type)) + 
  geom_boxplot() +
  guides(fill = FALSE) + 
  xlab("Creature") + 
  ylab("Hair Length") + 
  scale_fill_manual(values = c("#D55E00", "#0072B2", "#009E73"))


#### Soul
ggplot(train, 
       aes(x = type, 
           y = has_soul, 
           fill = type)) + 
  geom_boxplot() +
  guides(fill = FALSE) + 
  xlab("Creature") + 
  ylab("Percentage of Soul Present") + 
  scale_fill_manual(values = c("#D55E00", "#0072B2", "#009E73"))

train |>
ggplot(aes(x = color, fill = color)) +
    geom_bar() +
    facet_wrap(~type) 

# Arturo's data follows original Kaggle challenge properties.

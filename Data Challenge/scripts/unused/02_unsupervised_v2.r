#### Data challenge ############################################################
#' 
#' @name 02_unsupervised_v2.R
#' 
#' @description Perform unsupervised learning on the data and evaluate 
#'              perforance.
#' 
#' @describeIn here::here("datos", "train_set.csv") 
#'  Training monsters
#' 
#' @author Esteban Degetau
#' 
#' @created 2023-10-31
#' 
#### Unsupervised learning #####################################################

rm(list = ls())
gc()

#---- Libraries ---------------------------------------------------------------

pacman::p_load(
  tidyverse,
  gtsummary,
  here,
  ggplot2,
  fixest,
  hopkins,
  clValid,
  labelled,
  corrr,
  ggcorrplot,
  FactoMineR,
  factoextra 
  )

#---- Data --------------------------------------------------------------------

training <- read_csv(here("datos", "train_set.csv"))

#---- Data preparation --------------------------------------------------------

str(training)

training <-  training |>
    mutate(
        color = as_factor(color)
    )

blind <- training |>
  select(-type, -color) 

work <- blind |>
    mutate(data_id = "numeric") |>
    rowid_to_column() |>
     as.data.frame() 

work_color <- training |>
  select(-type) |>
  fastDummies::dummy_cols("color", remove_selected_columns = T) |>
  mutate(data_id = "color dummies") |>
  rowid_to_column() |> 
  as.data.frame()

# Add every interaction term between numeric variables
work_int <- blind |>
    mutate(
        bone_length_rotting_flesh = bone_length * rotting_flesh,
        bone_length_hair_length = bone_length * hair_length,
        bone_length_has_soul = bone_length * has_soul,
        rotting_flesh_hair_length = rotting_flesh * hair_length,
        rotting_flesh_has_soul = rotting_flesh * has_soul,
        hair_length_has_soul = hair_length * has_soul
    ) |>
    mutate(data_id = "interactions") |>
    rowid_to_column() |>
    as.data.frame()


full <- training |>
  select(-type) |>
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
    rowid_to_column() |>
    as.data.frame()

data <- full_join(work, work_color) |>
    full_join(work_int) |>
    full_join(full) |>
    select(-rowid) |>
    nest(.by = data_id)

#---- Functions ---------------------------------------------------------------

# Define CL methods
clmethods <- c("kmeans","pam","hierarchical")

my_internal_validity <- function(data) {

 

    int_val <- clValid(data, 
             nClust = 3,
             clMethods = clmethods, 
             metric = "euclidean", 
             validation = "internal") 

    optimal <- optimalScores(int_val)

    return(optimal)

}


check_prediction <- function(prediction) {

    tibble(
        true = training$type,
        pred = as_factor(prediction)
    ) |>
    group_by(true, pred) |>
    summarise(n = n(), dplyr.summarise.inform = F) |>
    group_by(true) |>
    summarise(per_true = max(n) / sum(n),
              n = sum(n),
              dplyr.summarise.inform = F) |>
    ungroup() |>
    summarise(correct = weighted.mean(per_true, n),
    dplyr.summarise.inform = F) |>
    pull(correct)

}

my_kmeans <- function(data) {

    ans <- kmeans(data, 3)

    ans$cluster |> check_prediction()

    
}

my_hierarchical <- function(data) {

    ans <- data |>
        dist() |>
        hclust(method = "ward.D2") |>
        cutree(k = 3) |>
        check_prediction()
    

    
}

my_pam <- function(data) {

    ans <- pam(data, 3)

    ans$clustering |> check_prediction()

    
}

#---- Perform unsupervised learning -------------------------------------------

work_data <- data |>
    mutate(
        # Clean and scale data
        data = map(data, ~ select(.x, where(~ !is.na(sum(.x)))) |> as.matrix() |> scale()),

        # Compute hopkins statistic
        hopkins = map_dbl(data, hopkins),

        # Internal validity
        internal_validity = map(data, my_internal_validity),


    )

# Export Hopkins test
hopkins <- work_data |>
    select(data_id, hopkins)

save(hopkins, file = here("resultados", "hopkins.RData"))

#---- Resampling tests --------------------------------------------------------

perform_test <- function(test) {
    work_data |>
        mutate(
            # Kmeans
            kmeans = map_dbl(data, my_kmeans),

            # Hierarchical
            hierarchical = map_dbl(data, my_hierarchical),

            # PAM
            pam = map_dbl(data, my_pam)
        ) |>
        select(data_id, kmeans, hierarchical, pam)
}


numeric <- work |>
    select(!c(rowid, data_id)) |>
    as.matrix() |>
    scale()

pam <- pam(numeric, 3)

pam_res <- training |>
    bind_cols(pam = pam$cluster)

pam_res |> save(file = here("resultados", "pam.RData"))



#---- Perform tests  -----------------------------------------------------------
tests <- tibble(test_is = 1:1000) |>
    mutate(test = map(test_is, ~ perform_test(.x), .progress = T))

# Save tests to disk
save(tests, file = here("resultados", "tests_unsupervised.RData"))

# Summarise results
results <- tests |>
    unnest(cols = c(test)) |>
    select(-test_is) |>
    # Pivot longer
    pivot_longer(
        cols = c(kmeans, hierarchical, pam),
        names_to = "method",
        values_to = "accuracy"
    ) |>
    group_by(data_id, method) |>
    summarise(
        mean = mean(accuracy),
        sd = sd(accuracy),
        # Confidence interval for the mean, according to empirical distribution
        lower = mean - (quantile(accuracy, 0.025) * sd),
        upper =  mean + (quantile(accuracy, 0.975) * sd))

# Plot results
results |>
    ggplot( aes(
            x = mean,
            y = data_id, 
            color = method,
             xmin = lower, 
             xmax = upper)) +
  #  geom_point()
    geom_pointrange(
       )

results |> write_csv(here("resultados", "results_unsupervised.csv"))


#---- Perform with different subsamples ---------------------------------------


check_prediction <- function(prediction, true) {

    tibble(
        true = true,
        pred = as_factor(prediction)
    ) |>
    group_by(true, pred) |>
    summarise(n = n(), dplyr.summarise.inform = F) |>
    group_by(true) |>
    summarise(per_true = max(n) / sum(n),
              n = sum(n),
              dplyr.summarise.inform = F) |>
    ungroup() |>
    summarise(correct = weighted.mean(per_true, n),
    dplyr.summarise.inform = F) |>
    pull(correct)

}

my_kmeans <- function(data, true) {

    ans <- kmeans(data, 3)

   check_prediction(ans$cluster, true)

    
}

my_hierarchical <- function(data, true) {

    ans <- data |>
        dist() |>
        hclust(method = "ward.D2") |>
        cutree(k = 3) 

    check_prediction(ans, true)
    

    
}

my_pam <- function(data, true) {

    ans <- pam(data, 3)

    check_prediction( ans$clustering, true)

    
}


perform_test_resampled <- function(test) {
    

    work_data |>
    mutate(
        # Resample
        data_labeled = map(data, ~ bind_cols(.x, type = training$type) |>
            as_tibble() %>%
            sample_n(., 300, replace = T)),
        true_labels = map(data_labeled, ~ .x |> select(type)),
        data = map(data_labeled, ~ select(.x, !type) |>
            as.matrix() |>
            scale()),

        # Kmeans
        kmeans = map2_dbl(.x = data, .y = true_labels, my_kmeans),

        # Hierarchical
        hierarchical = map2_dbl(.x = data, .y = true_labels, my_hierarchical),

        # # PAM
        pam = map2_dbl(.x = data, .y = true_labels, my_pam)
    ) |>
    select(data_id, kmeans, hierarchical, pam)

}



resampled_tests <- tibble(test_is = 1:1000) |>
    mutate(test = map(test_is, ~ perform_test_resampled(.x), .progress = T))




# Save tests to disk
save(resampled_tests, file = here("resultados", "resampled_tests_unsupervised.RData"))

# Summarise results
results <- resampled_tests |>
    unnest(cols = c(test)) |>
    select(-test_is) |>
    # Pivot longer
    pivot_longer(
        cols = c(kmeans, hierarchical, pam),
        names_to = "method",
        values_to = "accuracy"
    ) |>
    group_by(data_id, method) |>
    summarise(
        mean = mean(accuracy),
        sd = sd(accuracy),
        # Confidence interval for the mean, according to empirical distribution
        lower = mean - (quantile(accuracy, 0.025) * sd),
        upper =  mean + (quantile(accuracy, 0.975) * sd))

# Plot results
results |>
    ggplot( aes(
            x = mean,
            y = data_id, 
            color = method,
             xmin = lower, 
             xmax = upper)) +
  #  geom_point()
    geom_pointrange(
       )

# Still, the best method is PAM with only numeric variables

results |> write_csv(here("resultados", "results_unsupervised_resampled.csv"))

results <- read_csv(here("resultados", "results_unsupervised_resampled.csv"))


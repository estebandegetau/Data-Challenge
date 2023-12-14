#### Data challenge ############################################################
#' 
#' @name 02_unsupervised.R
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

work <- blind |> as.data.frame()

work_color <- training |>
  select(-type) |>
  fastDummies::dummy_cols("color", remove_selected_columns = T) |>
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
    )

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
  )

#---- Hopkins statistic --------------------------------------------------------

hopkins(work)

hopkins(work_color)

hopkins(work_int)

# Close to 1 indicates highly clustered data (note not using color variable)

#---- Internal validity --------------------------------------------------------

# Check internal validity using pkg clValid

clmethods <- c("kmeans","pam","hierarchical")

int_val <- clValid(work, 
             nClust = 3,
             clMethods = clmethods, 
             metric = "euclidean", 
             validation = "internal")

summary(int_val)

# Output to be displeyed in report
iv_opt <- optimalScores(int_val) 

# Internal validity using color dummies
int_val_color <- clValid(work_color, 
             nClust = 3,
             clMethods = clmethods, 
             metric = "euclidean", 
             validation = "internal")

summary(int_val_color)
col_opt <- optimalScores(int_val_color)

# Internal validity using interaction terms and color dummies
int_val_int <- clValid(work_int, 
             nClust = 3,
             clMethods = clmethods, 
             metric = "euclidean", 
             validation = "internal")

summary(int_val_int)
int_opt <- optimalScores(int_val_int)

# Bind all internal validity results

int_val <- bind_rows(
    iv_opt,
    col_opt,
    int_opt
) |>
rownames_to_column("test") |>
mutate(
    data = c(rep("work", 3), rep("work_color", 3), rep("work_int", 3)),
    test = str_extract(test, "[a-zA-Z]+(?=\\.\\.\\.)")
) 

# Adding color dummies seems to provide better (internaly) defined cluster

#---- Stability ----------------------------------------------------------------

# Perform staility tests
stability <- clValid(work,
        nClust = 3,
        clMethods = clmethods, 
        validation = "stability",
        metric = "euclidean")

summary(stability)

stab_opt <- optimalScores(stability)

# Stability using color dummies
stability_color <- clValid(work_color,
        nClust = 3,
        clMethods = clmethods, 
        validation = "stability",
        metric = "euclidean")

summary(stability_color)

stab_opt_color <- optimalScores(stability_color)

# Stability using interaction terms and color dummies
stability_int <- clValid(work_int,
        nClust = 3,
        clMethods = clmethods, 
        validation = "stability",
        metric = "euclidean")

summary(stability_int)

stab_opt_int <- optimalScores(stability_int)

# Bind all stability results
stability <- bind_rows(
    stab_opt,
    stab_opt_color,
    stab_opt_int
) |>
rownames_to_column("test") |>
mutate(
    data = c(rep("work", 4), rep("work_color", 4), rep("work_int", 4)),
    test = str_extract(test, "[a-zA-Z]+(?=\\.\\.\\.)")
)

stability |>
    group_by(data) |>
    summarise(socre_mean = mean(Score)) |>
    arrange(desc(socre_mean))

#---- PCA ----------------------------------------------------------------------

# Compute principal components. work data is already scaled
pc.out <- FactoMineR::PCA(work, scale.unit = F)

# Visualize biplot with variable vectors on first two components
pc_biplot <- factoextra::fviz_pca_biplot(pc.out, 
                             geom.ind = "point", 
                             repel = T) +
  theme_classic()


my_cluster_plot <- function(data, cluster = training$type) {

    fviz_cluster(object = list(data = {{ data }}, 
                               cluster = {{ cluster }}),
             geom = "point",
             ellipse = T,
             ellipse.type = "euclid", # Concentration ellipse 
             star.plot = TRUE,
             # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())

}

my_cluster_plot(work)

my_cluster_plot(work_color)

my_cluster_plot(work_int)

my_cluster_plot(full)

#---- Hierarchical clustering --------------------------------------------------

# Hierarchical clustering requires a PCA object
pc.out <- PCA(work_int, scale.unit = F)

# Compute hierarchical clustering, cutting tree at 2 clusters
hcpc.out <- HCPC(pc.out, graph = F, nb.clust = 3)

# Visualize clustering
cluster_plot <- fviz_cluster(hcpc.out,
             geom = "point",
             ellipse = T,
             ellipse.type = "euclid", # Concentration ellipse 
             star.plot = TRUE,
             # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
             ) +
  theme_classic() +
  labs(title = "Segmentación")

cluster_plot

#---- K-means ------------------------------------------------------------------


# Perform kmeans clustering, using 3 clusters
kmeans_ans <- kmeans(work_int, 3)

# Visualize clustering in PC
kmeans_biplot <- fviz_cluster(kmeans_ans, work, geom = "point",
                              ellipse.type = "euclid", # Concentration ellipse 
                              star.plot = TRUE,
                              # Add segments from centroids to items
                              repel = TRUE, # Avoid label overplotting (slow)
                              ggtheme = theme_minimal()) +
  labs(title = "Cluster plot")

kmeans_biplot

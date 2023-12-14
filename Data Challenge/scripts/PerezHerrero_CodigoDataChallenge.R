---
  title: "Hyperparameter Random Search and Feature Selection with `mlr`"
author: "Enrique Pérez Herrero"
date: "November 15, 2016"
output: 
  html_document: 
  fig_width: 7
highlight: tango
number_sections: yes
theme: cosmo
toc: yes
---
  
  # SUMMARY
  
  `caret` and `mlr` are the two main general purpose Machine Learning packages 
available in R. `caret` (2007) was developed before `mlr` (2013). The aim of 
this notebook is to write down a small cooking recipe to use `mlr`package for 
classification problem.

Although the type of predictive models that  `caret` and `mlr` can handle may
differ: both packages can optimize tuning hyperparameters through _Random
Search_.

`caret` web for [Random Hyperparameter 
                 Search](https://topepo.github.io/caret/random-hyperparameter-search.html), warns
that this kind of model optimization _may be inefficient for the some models_, 
mainly models where there are too much parameters to optimize, like for instance
`gmlnet` (or `xboost`)

But **Ghouls, Goblins, and Ghosts... Boo!**  data set is small, so it is ideal
for testing purposes, even when computational brute force is used.

So:
  
  * This method is not recommendable to optimize many parameters in big data sets 
with a scarce computational resources.

* It is preferable and more efficient to grid search, because it avoids
searching in low interest areas.

* Do not expect high performance in the competition.

* It is mandatory to gain experience with alternate tools to `caret`, and learn
to solve the same problem by several ways.


# CLEAN AND LOAD PACKAGES

```{r , message=FALSE, results='hide'}
rm(list = ls(all = TRUE))
gc(reset = TRUE)

library(data.table)
library(ggplot2)
library(glmnet)
library(mlr)
library(pander)
```

## Parameters

```{r}
set.seed(pi)

# Kernel parameters
SAVE_SUBMISSION <- TRUE
KAGGLE <- TRUE

# Model parameters
MODEL <- "Goblins-Kernel-01"
VERBOSE <- FALSE
RANDOM_ITER <- 20L
EXTRA_FEATURES <- 20L
EXPONENT_RANGE <- -2:2
FEATURES_THRESHOLD <- 0.33
```

## User Functions

```{r}
# Loading data
load_data <- function(data_type = "train", kaggle = KAGGLE) {
  data_dir <- ifelse(kaggle, "../input",  "data")
  data_dir <- paste0(data_dir, "/", data_type, ".csv")
  return(fread(data_dir, data.table = FALSE, stringsAsFactors = TRUE))
}

# Saving submission
save_submission <-
  function(data, save_sub = FALSE, model = "XX", kaggle = KAGGLE) {
    data_dir <- ifelse(kaggle, "",  "submissions/")
    if (save_sub) {
      cat(paste("Saving submission model:", model, "\n"))
      data <- data[, c("id", "type")]
      file_dir <-
        paste0("data_dir", model, ".csv")
      write.csv(data, quote = FALSE, row.names = FALSE, file_dir)
    }
  }
```


# LOADING TRAINING DATA

```{r}
training <- load_data("train")
```

# CLASSIFICATION LEARNING TASK

Learning tasks encapsulate the data set (`training`), description `id`,  and 
information about the machine learning problem, as the name of the target
variable `type`

```{r}
classif_task <- makeClassifTask(id = MODEL,
                                data = training,
                                target = "type")

classif_task <- dropFeatures(classif_task, features = "id")
```

# VARIABLE SELECTION

## Checking data variable importance

```{r}
fv <- generateFilterValuesData(classif_task,
                               method = c("information.gain", "rf.importance"))

plotFilterValues(fv, feat.type.cols = TRUE)
```

_Information Gain_ and _Randon Forest Importance_ are very low for `color`, this
variable can be treated just as noise.


# ADDING EXTRA FEATURES GENERATED RANDOMLY

Variable importance in data can be improved adding extra features generated
randomly.

$V_{i} = {bone\_length}^{A}  * rotting\_flesh^{B} * hair\_length^{C} * has\_soul^{D}$
  where the exponents $A$, $B$, $C$ and $D$ are in the range `EXPONENT_RANGE`


```{r}
extra_features <- EXTRA_FEATURES
exponent_range <- EXPONENT_RANGE
numeric_names <- names(Filter(is.double, training))
n_cols <- length(numeric_names)
```


## Creating a matrix of random coefficients

```{r}
m <- sample(exponent_range, size = extra_features * n_cols, replace = TRUE)
m <- matrix(m, nrow = extra_features, ncol = n_cols)
m <- as.data.frame(m)
names(m) <- numeric_names
# Delete duplicates
m <- m[!duplicated(m), ]
m$formula <-
  apply(m, 1, function(x)
    paste0("(training$", numeric_names, "^", x, ")", collapse  = "*"))
```

## Updating task with new features

```{r}
for(i in seq(nrow(m))) {
  training[paste0("V", i)] <- eval(parse(text = m$formula[[i]]))
}
classif_task <- makeClassifTask(id = MODEL,
                                data = training,
                                target = "type")
classif_task <- dropFeatures(classif_task, features = "id")
features_list <- getTaskFeatureNames(classif_task)
```

The features are: `r pander(features_list)`

# VARIABLE SELECTION

##  New features variable importance

```{r}
fv <- generateFilterValuesData(classif_task,
                               method = c("information.gain", "rf.importance"))
plotFilterValues(fv, n.show = ncol(training), feat.type.cols = TRUE)
```

##  Filter features

```{r}
classif_task <- filterFeatures(classif_task,
                               method = "information.gain",
                               threshold = FEATURES_THRESHOLD)
getTaskFeatureNames(classif_task)
```



# CLASSIFICATION LEARNER

```{r}
classif_lrn <- makeLearner("classif.cvglmnet", predict.type = "response")
```

## Setting learner fixed hyperparameters

Parameters here are excluded from random search. 

```{r}
# not used, included for further testing
# classif_lrn <- setHyperPars(classif_lrn, # parameters...)
```


## Learner Hyperparameters list

```{r}
getParamSet(classif_lrn)
```


# TUNING PARAMETERS

## Tuning Random Search control

```{r}
control_random <- makeTuneControlRandom(maxit = RANDOM_ITER)
```

## Accuracy as control measure

```{r}
control_measures <- list(acc)
```

## Cross Validation: 10-fold CV

```{r}
resamp <- makeResampleDesc(method = "CV", iters = 10L)
```

## Parameters to be searched randomly

$\alpha$ = 1 is the lasso penalty, and $\alpha$ = 0 the ridge penalty

```{r}
ps <- makeParamSet(
  makeNumericParam("alpha", lower = 0, upper = 1)
)
```

## Searching optimal parameters

```{r tune, message=FALSE, results='hide'}
res <- tuneParams(
  learner = classif_lrn,
  task = classif_task,
  resampling = resamp,
  measures = control_measures,
  par.set = ps,
  control = control_random,
  show.info = VERBOSE
)
```

# TUNING RESULTS

```{r}
# Best parameters
opt_par <- res$x
opt_par

# Accuracy
opt_acc <- res$y

# Optimization Grid
opt_grid <- as.data.frame(res$opt.path)
```

Accuracy = `r paste0(round(100 * opt_acc, 4), "%")`

## Tuning plot

```{r}
ggplot(data = opt_grid,  aes(x = alpha, y = acc.test.mean)) +
  geom_point() +
  geom_point(aes(x = opt_par$alpha, y = opt_acc), size = 3, color = "red")
```

## Relative Overfitting Rate: ROR

_Relative Overfitting Rate (ROR)_: is a quantity that ranges from 0, with no
overfitting, to 1 with overfitting equal to the _No Information Rate_

```{r  message=FALSE, results='hide'}
ROR <- estimateRelativeOverfitting(resamp,
                                   control_measures,
                                   classif_task,
                                   classif_lrn)
# Aggregate ROR by mean
ROR <- mean(ROR$relative.overfit.acc)
```

ROR = `r  paste0(round(100 * ROR, 2), "%")`

## No Information Rate: NIR

_No Information Rate (NIR)_: is the largest class percentage in the data, as it
is calculated in  `caret` package.

```{r}
getNoInformationRate <- function(task) {
  max(table(getTaskTargets(task)) / getTaskSize(task))
}

NIR <- getNoInformationRate(classif_task)
```

NIR = `r paste0(round(100 * NIR, 2), "%")`

# MODEL FITTING WITH PARAMETERS FOUND

```{r}
classif_lrn <- setHyperPars(classif_lrn,  par.vals = opt_par)
model <- train(learner = classif_lrn, task = classif_task)
```

# PREDICTING TEST SET

## Loading testing data and adding extra features
```{r}
testing <- load_data("test")

for(i in seq(nrow(m))) {
  frm <- gsub("training", "testing", m$formula[[i]])
  testing[paste0("V", i)] <- eval(parse(text = frm))
}
# keep id
testing_id <- testing$id
```

## Selecting important features
```{r}
testing <- testing[getTaskFeatureNames(classif_task)]
```

## Predicting testing data type
```{r}
prediction <- predict(model, newdata = testing)
```

# SAVING SUBMISSION
```{r}
testing$id <- testing_id
testing$type <- prediction$data$response
save_submission(testing, SAVE_SUBMISSION, model = MODEL)
```

# RESULTS

```{r}
# 66 ↓1 Enrique Pérez Herrero 0.74669 14	Mon, 21 Nov 2016 19:35:59 
# Your Best Entry - ↑ You improved on your best score by 0.00378. 
# You just moved up 38 positions on - the leaderboard
```


# TODO
* Improve parameter search including extra parameters in the optimization.
* Use other predictive methods by changing this script.
* Use Iterated F-Racing for optimization. 

# LINKS

* [mlr Release Tutorial](https://mlr-org.github.io/mlr-tutorial/release/html/index.html)
* [mlr Development Tutorial](https://mlr-org.github.io/mlr-tutorial/devel/html/index.html)
* [mrl GitHub](https://github.com/mlr-org/mlr/)
* [mlr: Machine Learning in R:](https://cran.r-project.org/web/packages/mlr/index.html)
* [Improvements on Cross-Validation: The .632+ Bootstrap
   Method](http://www.stat.washington.edu/courses/stat527/s13/readings/EfronTibshirani_JASA_1997.pdf)
_Bradley Efron; Robert Tibshirani_ - *Journal of the American Statistical
Association, Vol. 92, No. 438. (Jun., 1997), pp. 548-560*
  * [mlr Tutorial - arXiv.org](https://arxiv.org/abs/1609.06146)
* [knitr in a knutshell](http://kbroman.org/knitr_knutshell/pages/figs_tables.html)
* [A simple explanation of the Lasso and Least Angle Regression](http://statweb.stanford.edu/~tibs/lasso/simple.html)
* [A Complete Tutorial on Ridge and Lasso Regression in Python](https://www.analyticsvidhya.com/blog/2016/01/complete-tutorial-ridge-lasso-regression-python/)
* [glmnet: Lasso and Elastic-Net Regularized Generalized Linear Models](https://cran.r-project.org/web/packages/glmnet/index.html)
* [Package ‘glmnet’](https://cran.r-project.org/web/packages/glmnet/glmnet.pdf)
* Friedman, J., Hastie, T. and Tibshirani, R. (2008) Regularization Paths for Generalized Linear Models
via Coordinate Descent, [http://www.stanford.edu/~hastie/Papers/glmnet.pdf](http://www.stanford.edu/~hastie/Papers/glmnet.pdf)
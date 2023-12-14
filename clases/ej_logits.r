# Logit exercise

# Libraries
pacman::p_load(
    tidyverse,
    here,
    haven
)

# Data
ins <- read_dta(here("datos", "insurance_12.dta"))

names(ins)

# Linear probability model
ols <- lm(
    insured ~ selfemp + age + familysz + male + deg_nd +
     deg_ged + deg_hs + deg_ba + deg_ma + deg_oth,
    data = ins) 

# Probit
probit <- glm(insured ~ selfemp + age + familysz + male + deg_nd +
     deg_ged + deg_hs + deg_ba + deg_ma + deg_oth,
    data = ins,
    family = binomial(link = "probit"))

# Logit
logit <- glm(insured ~ selfemp + age + familysz + male + deg_nd +
     deg_ged + deg_hs + deg_ba + deg_ma + deg_oth,
    data = ins,
    family = binomial(link = "logit"))

# Profile to predict 

profile <- tibble(
    selfemp = 0,
    age = 26,
    familysz = 0,
    male = 1,
    deg_nd = 0,
    deg_ged = 0,
    deg_hs = 0,
    deg_ba = 1,
    deg_ma = 0,
    deg_oth = 0
)

# Predict
predict(ols, profile, type = "response")

predict(probit, profile, type = "response")

predict(logit, profile, type = "response")

# Exact marginal effect of a 1 year increase in age

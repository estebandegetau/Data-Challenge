# Examen final 165092

pacman::p_load(tidyverse, here)

# 1.c
# Compute se from p-value
p_val <- 0.04


beta_0 <- qnorm(0.35)

beta_1 <-  qnorm(0.362) -beta_0
beta_2 <- qnorm(0.378) -beta_0
beta_3 <- qnorm(0.333) -beta_0
beta_4 <- qnorm(0.483) -beta_0 - beta_3
beta_5 <- qnorm(0.368) -beta_0 - beta_3


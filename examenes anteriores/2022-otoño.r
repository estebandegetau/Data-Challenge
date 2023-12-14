

pacman::p_load(tidyverse)

# 8
p_val <- 0.02
coef <- 0.15

# Get standard error
se <- coef / qnorm(p_val / 2)
se

# P-value for one-sided test
pnorm(coef / se, lower.tail = FALSE)

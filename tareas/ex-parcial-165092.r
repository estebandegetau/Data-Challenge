
pacman::p_load(tidyverse)

# 7
# Homocedastik p-value
p_val <- 0.04

# Hetero robust SE is 15% larger than homocedastik SE
coef <- 1.5
se <- coef / qnorm(p_val)
se_hetero <- se * 1.15
p_val <- 1 - pnorm(coef / se_hetero, lower.tail = FALSE)

p_val |> round(3)

# Re do

p_val_homo <- 0.04

t_homo <- qnorm(1-(p_val_homo/2))
se_homo <- coef / t_homo
se_het <- 1.15 * se_homo
p_val_het <- 2*(1 - pnorm(coef/se_het))

# Problem set 3
# 1. Consider the following data generating process in which n observations
# belong to one of two classes

# parameters for classes
mu_1 <- c(-3, 3)
mu_2 <- c(5, 5)
Sigma <- rbind(c(16, -2), c(-2, 9))
n_1 <- 300
n_2 <- 500

library(mvtnorm)

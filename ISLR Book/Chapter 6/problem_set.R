# Consider the following data generating process with n = 100 observations and p covariates. Initially set the
# number of predictors p = 2 and X ~ Np(µ, S). S is the covariance matrix with the variance on the diagonal
# and small values on the off-diagonal (both values chosen by you). µ = !
# 0 10
# "
# . The (initially) true coefficients
# range from ß = 0.1 - 0 (you can sample values from that range or use equispaced values on that interval) and
# the errors are drawn from a normal distribution e ~ N (0, 1).

# Constants ----
n <- 100
p <- 2
mu <- c(0, 10)
sigma <- rbind(c(16, 0.4), c(0.4, 9))
beta_vector <- c(0.1, 0)
# END Constants ----

# Functions ----
get_mse <- function(predicted, actual) {
    mean((predicted - actual)^2)
}
# END Functions ----
library("MASS")
set.seed(1)
x <- mvrnorm(n, mu, sigma)
error_term <- rnorm(n)
y <- x %*% beta_vector + error_term
exercise_data <- data.frame(x, y)
# a)
set.seed(2)
train <- sample(1:n, n / 2)
test <- (-train)
library("pls")
library("ggplot2")
library("ggfortify")

pcr_components <- prcomp(exercise_data[train, ], scale = TRUE)
autoplot(pcr_components, colour = "y", loadings = TRUE)

pcr_components
summary(pcr_components)
pls_model <- plsr(y ~ ., data = exercise_data[train, ], scale = TRUE, validation = "CV")
summary(pls_model)
pls_predict <- predict(pls_model, exercise_data[test, ], ncomp = 2)
get_mse(pls_predict, y[test]) # 1.267909

# b) compute test errors for ridge, lasso, PCA
set.seed(3)
library("glmnet")
model_x <- model.matrix(y ~ ., exercise_data)[, -1]
model_y <- exercise_data$y

## ridge
ridge_cv_out <- cv.glmnet(model_x[train, ], model_y[train], alpha = 0)
ridge_best_lambda <- ridge_cv_out$lambda.min
ridge <- glmnet(model_x[train, ], model_y[train], alpha = 0, lambda = ridge_best_lambda)
ridge_predict <- predict(ridge, s = ridge_best_lambda, newx = model_x[test, ])
get_mse(ridge_predict, y[test]) # 1.203211

## lasso
lasso_cv_out <- cv.glmnet(model_x[train, ], model_y[train], alpha = 1)
lasso_best_lambda <- lasso_cv_out$lambda.min
lasso <- glmnet(model_x[train, ], model_y[train], alpha = 1, lambda = lasso_best_lambda)
lasso_predict <- predict(lasso, s = lasso_best_lambda, newx = model_x[test, ])
get_mse(lasso_predict, y[test]) # 1.264267
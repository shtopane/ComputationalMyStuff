# Functions ----
get_mse <- function(predicted, actual) {
    mean((predicted - actual)^2)
}
# END Functions ----

# Constants ----

# maximum variables in the Hitters dataset(?)
hitters_max_variables <- 19
# arguments for alpha parameter of glmnet()
perform_ridge <- 0
perform_lasso <- 1
# grid of lambda values, also constant throughout the file
# lambda = [10^10, 10^-2]
lambda_grid <- 10^seq(10, -2, length = 100)
lambda_test4 <- 4
lambda_test_large <- 1e10 # 10^10
lambda_OLS <- 0 # nolint

# END Constants ----

# Data Preparation ----
library("ISLR2")
View(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary)) # 59

# Remove missing Salaries
Hitters <- na.omit(Hitters) # nolint
dim(Hitters)
sum(is.na(Hitters))
# END Data Preparation ----

# 6.5.1 Subset Selection Method ----

# perform best subset selection by identifying the best model that contains a given number
# of predictors, where best is quantified using RSS.
library("leaps")
regfit_full <- regsubsets(Salary ~ ., Hitters)
summary(regfit_full)
# by default reports only the best 8 variable models. We can change that
regfit_full <- regsubsets(Salary ~ ., data = Hitters, nvmax = hitters_max_variables)
reg_summary <- summary(regfit_full)
names(reg_summary)
# R^2
reg_summary$rsq
# Plot other info useful for choosing a model
xlab_text <- "Number of Variables"
par(mfrow = c(2, 2))
plot(reg_summary$rss, xlab = xlab_text, ylab = "RSS", type = "l")
plot(reg_summary$adjr2, xlab = xlab_text, ylab = "Adjusted Rsq", type = "l")
adjr2_max <- which.max(reg_summary$adjr2)
points(adjr2_max, reg_summary$adjr2[adjr2_max], col = "red", cex = 2, pch = 20)

# Plot Cp and BIC statistics(?)
plot(reg_summary$cp, xlab = xlab_text, ylab = "Cp", type = "l")
cp_min <- which.min(reg_summary$cp)
points(cp_min, reg_summary$cp[cp_min], col = "red", cex = 2, pch = 20)
bic_min <- which.min(reg_summary$bic)
plot(reg_summary$bic, xlab = xlab_text, ylab = "BIC", type = "l")
points(bic_min, reg_summary$bic[bic_min], col = "red", cex = 2, pch = 20)

# Plot another thing?
plot(regfit_full, scale = "r2")
plot(regfit_full, scale = "adjr2")
plot(regfit_full, scale = "Cp")
plot(regfit_full, scale = "bic")
# The above plots suggest that the lowest BIC model is the one with 6 variables:
# Hits, Walks, CRBI, DivisionW AtBat and PutOuts
coef(regfit_full, 6)

# Forward and Backward Stepwise Seleciton
regfit_forward <- regsubsets(Salary ~ ., data = Hitters, nvmax = hitters_max_variables, method = "forward")
summary(regfit_forward)
regfit_backward <- regsubsets(Salary ~ ., data = Hitters, nvmax = hitters_max_variables, method = "backward")
summary(regfit_backward)

# 7 variable models are different for different methods
coef(regfit_full, 7)
coef(regfit_forward, 7)
coef(regfit_backward, 7)

# Choosing Among Models Using the Validation-Set Approach and
# Cross-Validation
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters), replace = TRUE)
test <- (!train)
regfit_best <- regsubsets(Salary ~ ., nvmax = hitters_max_variables, data = Hitters[train, ])
# Compute the validation set error for the best model of each model size
# model.matrix build "X" matrix from data
test_matrix <- model.matrix(Salary ~ ., data = Hitters[test, ])
test_matrix
# Now we run a loop, and for each size i, we
# extract the coefficients from regfit.best for the best model of that size,
# multiply them into the appropriate columns of the test model matrix to
# form the predictions, and compute the test MSE.
val_errors <- rep(NA, hitters_max_variables)
seq_len_max_variables <- seq_len(hitters_max_variables)

for (i in seq_len_max_variables) {
    regfit_best_coefficients <- coef(regfit_best, id = i)
    regfit_best_prediction <- test_matrix[, names(regfit_best_coefficients)] %*% regfit_best_coefficients
    val_errors[i] <- mean((Hitters$Salary[test] - regfit_best_prediction)^2)
}

val_errors
which.min(val_errors)
coef(regfit_best, which.min(val_errors))
# This was a little tedious, partly because there is no predict() method
# for regsubsets()
# Create a function for prediction for regsubsets()
predict_regsubsets <- function(object, newdata, id, ...) {
    # extract formula
    predict_formula <- as.formula(object$call[[2]])
    predict_matrix <- model.matrix(predict_formula, newdata)
    predict_coefficients <- coef(object, id = id)
    predict_x_variables <- names(predict_coefficients)

    return(predict_matrix[, predict_x_variables] %*% predict_coefficients)
}

# Finally, we perform best subset selection on the full data set, and select
# the best seven-variable model. It is important that we make use of the
# full data set in order to obtain more accurate coefficient estimates. Note
# that we perform best subset selection on the full data set and select the
# best seven-variable model, rather than simply using the variables that were
# obtained from the training set, because the best seven-variable model on
# the full data set may differ from the corresponding model on the training
# set.
regfit_best <- regsubsets(Salary ~ ., data = Hitters, nvmax = hitters_max_variables)
coef(regfit_best, 7)
# In fact, we see that the best seven-variable model on the full data set has a
# different set of variables than the best seven-variable model on the training
# set.
# Use cross-validation to choose among the best models
k <- 10
n <- nrow(Hitters)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv_errors <- matrix(NA, k, hitters_max_variables, dimnames = list(NULL, paste(1:hitters_max_variables)))

# This code differs from the one in the textbook: page 281
for (j in 1:k) {
    best_fit <- regsubsets(Salary ~ ., data = Hitters[folds != j, ], nvmax = hitters_max_variables)

    for (i in 1:hitters_max_variables) {
        pred <- predict_regsubsets(best_fit, Hitters[folds == j, ], id = i)
        cv_errors[j, i] <- mean((Hitters$Salary[folds == j] - pred)^2)
    }
}

cv_errors
# for a matrix 1 indicates rows, 2 indicates columns
# so apply the mean function over the columns of the cv_errors matrix
mean_cv_errors <- apply(cv_errors, 2, mean)
# ?apply
mean_cv_errors
par(mfrow = c(1, 1))
plot(mean_cv_errors, type = "b")
# We see that cross-validation selects a 10-variable model.

# Use best subset selection to obtain the best 10-variable model
reg_best <- regsubsets(Salary ~ ., data = Hitters, nvmax = hitters_max_variables)
coef(reg_best, 10)
# End 6.5.1 Subset Selection Method ----

# 6.5.2 Ridge Regression and the Lasso ----
x <- model.matrix(Salary ~ ., Hitters)[, -1] # why?, are we leaving out the Salary column, I'm not sure
y <- Hitters$Salary

library("glmnet")

# lambda is there by default but we can specify it as well
# glmnet standardizes the variables by default. If you need it to not be the case,
# pass standardize = FALSE
ridge_mod_all_data <- glmnet(x, y, alpha = perform_ridge, lambda = lambda_grid)
dim(coef(ridge_mod_all_data))
# Coefficients when lambda = 11 498
ridge_mod_all_data$lambda[50] # 11498
coef(ridge_mod_all_data)[, 50]
sqrt(sum(coef(ridge_mod_all_data)[-1, 50]^2)) # 6.36
# Coefficients when lambda = 705
ridge_mod_all_data$lambda[60] # 705
coef(ridge_mod_all_data)[, 60]
sqrt(sum(coef(ridge_mod_all_data)[-1, 60]^2)) # 57.11

# We can use predict to obtain ridge regression for lambda = 50
predict(ridge_mod_all_data, s = 50, type = "coefficients")[1:20, ]

# Split data into training and test set using a method where we choose a subset
# between 1 and n(used as indices)
set.seed(1)
train_seq_len <- seq_len(nrow(x))
train <- sample(train_seq_len, nrow(x) / 2)
test <- (-train)
y_test <- y[test]

ridge_mod_train_data <- glmnet(x[train, ], y[train], alpha = perform_ridge, lambda = lambda_grid, thresh = 1e-12)
ridge_predict_train <- predict(ridge_mod_train_data, s = lambda_test4, newx = x[test, ])
get_mse(ridge_predict_train, y_test)
# Prediction for a model with just an intercept
intercept_MSE <- mean((mean(y[train]) - y_test)^2) # nolint

# Predict a model with very large lambda
ridge_predict_train_large_lambda <- predict(ridge_mod_train_data, s = lambda_test_large, newx = x[test, ])
lambda_very_large_MSE <- get_mse(ridge_predict_train_large_lambda, y_test) # nolint
abs(lambda_very_large_MSE - intercept_MSE)
# This is better than a model with just the intercept

# Predict a model OLS => lambda = 0
# in order for glmnet to yield the same result we use exact = TRUE
ridge_predict_OLS_lambda <- predict(ridge_mod_train_data, s = lambda_OLS, newx = x[test, ], exact = TRUE, x = x[train, ], y = y[train])
OLS_MSE <- get_mse(ridge_predict_OLS_lambda, y_test) # nolint
OLS_MSE
predict(ridge_mod_train_data,
    s = 0, exact = T, type = "coefficients",
    x = x[train, ], y = y[train]
)[1:20, ]

# Instead of fixing lambda, we should use cross-validation to choose this tuning parameter.
# There is a build in cross-validation function cv.glmnet(), default is k = 10
set.seed(1)
ridge_cv_out <- cv.glmnet(x[train, ], y[train], alpha = perform_ridge)
plot(ridge_cv_out)
ridge_best_lambda <- ridge_cv_out$lambda.min
ridge_best_lambda
# Calculate MSE with this best lambda
ridge_predict_best_lambda <- predict(ridge_mod_train_data, s = ridge_best_lambda, newx = x[test, ])
best_lambda_MSE <- get_mse(ridge_predict_best_lambda, y_test) # nolint
best_lambda_MSE

# Refit ridge on the full data set with best lambda
ridge_refit <- glmnet(x, y, alpha = perform_ridge)
predict(ridge_refit, type = "coefficients", s = ridge_best_lambda)[1:20, ]
# Note: none of the coefficients is 0!

# Lasso
lasso_mod <- glmnet(x[train, ], y[train], alpha = perform_lasso, lambda = lambda_grid)
plot(lasso_mod)

# Validate the test error using cross-validation
set.seed(1)
lasso_cv_out <- cv.glmnet(x[train, ], y[train], alpha = perform_lasso)
plot(lasso_cv_out)
lasso_best_lambda <- lasso_cv_out$lambda.min
lasso_predict <- predict(lasso_mod, s = lasso_best_lambda, newx = x[test, ])
lasso_best_MSE <- get_mse(lasso_predict, y_test) # nolint

# Similar MSE to ridge with cross-validated lambda, but here
# some variables will be completely left out(coefficients = 0)
lasso_refit <- glmnet(x, y, alpha = perform_lasso, lambda = lambda_grid)
lasso_coefficients <- predict(lasso_refit, type = "coefficients", s = lasso_best_lambda)[1:20, ]
lasso_coefficients
# END 6.5.2 Ridge Regression and the Lasso ----

# 6.5.3 PCR and PLS Regression ----

# pcr() is from the pls package
library("pls")
set.seed(2)
# scale = TRUE: standardizing each predictor, so that the scale of 
# the variables is the same
# validation = "CV": computes 10-fold cross-validation error for each
# possible value of M
pcr_fit <- pcr(Salary ~ ., data = Hitters, scale = TRUE, validation = "CV")
# pcr() reports **root mean squared error** we need to square it
# to get the usual MSE
summary(pcr_fit)
# Plot MSE
# ?validationplot
validationplot(pcr_fit, val.type = "MSEP") # lowest on PCR = 18

# perform PCR on the training set
set.seed(1)
pcr_fit_train_data <- pcr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr_fit_train_data, val.type = "MSEP") # lowest on PCR = 5
pcr_predict <- predict(pcr_fit_train_data, x[test, ], ncomp = 5)
get_mse(pcr_predict, y_test)
# The test MSE is closer to Ridge regression and Lasso.
# Downside of PCR is that the model is more difficult to interpret.
# We also don't have coefficients!

# Fit PCR on the full data
pcr_fit_full_data <- pcr(y ~ x, scale = TRUE, ncomp = 5)
summary(pcr_fit_full_data)

# Partial Least Squares Regression
set.seed(1)
pls_fit <- plsr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls_fit) # Lowest MSE is when M = 1
pls_predict <- predict(pls_fit, x[test, ], ncomp = 1)
get_mse(pls_predict, y_test)
# Not that much higher than test MSE of ridge, lasso and PCR

# Fit PLS using the whole data and M = 1
pls_fit_full_data_m1 <- plsr(Salary ~ ., data = Hitters, scale = TRUE, ncomp = 1)
summary(pls_fit_full_data_m1)
# Notice that the percentage of variance in Salary that the one-component
# PLS fit explains, 43.05 %, is almost as much as that explained using the final
# five-component model PCR fit, 44.90 %. This is because PCR only attempts
# to maximize the amount of variance explained in the predictors, while PLS
# searches for directions that explain variance in both the predictors and the
# response.
# END 6.5.3 PCR and PLS Regression ----
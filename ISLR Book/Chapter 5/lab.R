# Chapter 5
# 5.3 Lab: Cross-Validation and the Bootstrap

# 1. Validation Set Approach ------

library("ISLR2")

# Functions -----

# prediction_object: object returned from predict(.) function
# data: real data we're trying to estimate
# subset_index: indices we want to exclude from data, e.g. validation set indices
get_sample_mse <- function(prediction_object, data, subset_index) {
    train_errors <- (data - prediction_object)
    # -train index selects only the observations that are not in tra
    # training set
    validation_errors <- train_errors[-subset_index]
    mse <- mean(validation_errors^2)
    return(round(mse, digits = 2))
}
# End functions -----

set.seed(1)
train <- sample(x = 392, size = 196)

# Use "subset" option in lm() to fit a linear regression
# using only the observations corresponding to the training set
train_fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
# Exposes the Auto dataset to the R session. This means that we can access variables
# from this dataset directly(ex: mpg)
# attach(Auto)
train_predict <- predict(train_fit, newdata = Auto)

mse <- get_sample_mse(train_predict, data = Auto$mpg, subset_index = train)
mse

# These error rates are 18.72 and 18.79, respectively. If we choose a different
# training set instead, then we will obtain somewhat different errors on the
# validation set.

train_fit_poly2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
train_fit_poly2_predict <- predict(train_fit_poly2, newdata = Auto)
mse_poly2 <- get_sample_mse(train_fit_poly2_predict, Auto$mpg, train)
mse_poly2

train_fit_poly3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
train_fit_poly3_predict <- predict(train_fit_poly3, newdata = Auto)
mse_poly3 <- get_sample_mse(train_fit_poly3_predict, Auto$mpg, train)
mse_poly3
# END -----

# Do the same thing again and calculate errors
set.seed(2)
train2 <- sample(x = 392, size = 196)
train2_fit <- lm(mpg ~ horsepower, data = Auto, subset = train2)
train2_predict <- predict(train2_fit, newdata = Auto)
mse <- get_sample_mse(train2_predict, Auto$mpg, train2)
mse

train2_fit_poly2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train2)
train2_fit_poly2_predict <- predict(train2_fit_poly2, newdata = Auto)
mse_poly2 <- get_sample_mse(train2_fit_poly2_predict, Auto$mpg, train2)
mse_poly2

train2_fit_poly3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train2)
train2_fit_poly3_predict <- predict(train2_fit_poly3, newdata = Auto)
mse_poly3 <- get_sample_mse(train2_fit_poly3_predict, Auto$mpg, train2)
mse_poly3

# Same results as before quadratic model is best fit

# 2. Leave-One-Out Cross-Validation ----
# glm and lm do the same for linear regression. glm() works with cv.glm() from
# the boot package
library("boot")
glm_fit <- glm(mpg ~ horsepower, data = Auto)
cv_error <- cv.glm(Auto, glm_fit)
# Cross-validation results: error estimate from LOOCV
cv_error$delta

cv_error_container <- rep(0, 10)

for (i in 1:10) {
    glm_fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
    cv_error_container[i] <- cv.glm(Auto, glm_fit)$delta[1]
}
# we see a sharp drop in the estimated test MSE between
# the linear and quadratic fits, but then no clear improvement from using
# higher-order polynomials.
cv_error_container
# ----

# 3. k-Fold Cross-Validation ----
k_fold <- 10
set.seed(17)
cv_k_fold_error_container <- rep(0, 10)

for (i in 1:10) {
    glm_fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
    cv_k_fold_error_container[i] <- cv.glm(Auto, glm_fit, K = k_fold)$delta[1]
}
# When we instead perform
# k-fold CV, then the two numbers associated with delta differ slightly. The
# first is the standard k-fold CV estimate, as in (5.3). The second is a biascorrected version. On this data set, the two estimates are very similar to
# each other.
cv_k_fold_error_container

# 4. The Bootstrap ----

# index: which observations to use to estimate alpha
# returns estimate for alpha based on [index] observations
alpha_fn <- function(data, index) {
    X <- data$X[index] # nolint
    Y <- data$Y[index] # nolint
    (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))
}

alpha_fn(Portfolio, 1:100)
# he next command uses the sample() function to randomly select 100 observations from the range 1 to 100, with replacement. This is equivalent
# to constructing a new bootstrap data set and recomputing ˆa based on the
# new data set.
set.seed(7)
alpha_fn(Portfolio, sample(100, 100, replace = TRUE))

# Perform bootstrap using our custom function, 1000 times
boot(Portfolio, alpha_fn, R = 1000)
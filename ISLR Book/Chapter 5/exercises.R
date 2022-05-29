# Applied
set.seed(1)
library("ISLR2")

# 5 a)
default_fit <- glm(default ~ income + balance, data = Default, family = "binomial")

# 5 b)
# i)
train <- sample(nrow(Default), size = (nrow(Default) / 2))
# ii)
train_fit <- glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
# iii)
prior <- 0.5

# for the whole dataset
default_probability <- predict(train_fit, newdata = Default)
# validation set
validation_set_probability <- default_probability[-train]

validation_set_prediction <- rep("No", length(validation_set_probability))
validation_set_prediction[validation_set_probability > prior] <- "Yes"

# calculate error
mean(Default$default[-train] != validation_set_prediction)

# c)
cross_validation <- function(sample_x = nrow(Default),
                             sample_n = (nrow(Default) / 2),
                             data = Default,
                             switch_model) {
    prior <- 0.5
    train <- sample(sample_x, sample_n)

    if (switch_model) {
        train_fit <- glm(default ~ income + balance + student, data = data, subset = train, family = "binomial")
    } else {
        train_fit <- glm(default ~ income + balance, data = data, subset = train, family = "binomial")
    }

    predict <- predict(train_fit, newdata = data)
    validation_probability <- predict[-train]
    validation_set_prediction <- rep("No", length(validation_probability))
    validation_set_prediction[validation_probability > prior] <- "Yes"

    return(mean(data$default[-train] != validation_set_prediction))
}

cross_validation(sample_n = 200)
cross_validation(sample_n = 6000)
cross_validation(sample_n = 20)

split_attempts <- seq(1000, nrow(Default), by = (nrow(Default) / 10))
error_container <- rep(0, length(split_attempts))
split_attempts_seq <- seq_length(length(split_attempts))

for (i in split_attempts_seq) {
    error_container[i] <- cross_validation(sample_n = split_attempts[i])
}

plot(split_attempts_seq, error_container)

# d) Add dummy student
cross_validation(switch_model = TRUE)
cross_validation(switch_model = FALSE)

simulation_runs <- 100
error_old_model_container <- rep(0, simulation_runs)
error_new_model_container <- rep(0, simulation_runs)

for (i in seq_len(simulation_runs)) {
    error_old_model_container[i] <- cross_validation(switch_model = FALSE)
    error_new_model_container[i] <- cross_validation(switch_model = TRUE)
}
# Including student does not reduce the error rate by much
mean(error_old_model_container)
mean(error_new_model_container)

# 6.
# a)
set.seed(3)

extract_standard_errors_from_glm <- function(model) {
    summary(model)$coefficients[, 2]
}

default_model <- glm(default ~ income + balance, data = Default, family = "binomial")
summary(default_model)
# second column is the standard errors, returned for all regressors + intercept
default_model_standard_errors <- extract_standard_errors_from_glm(default_model)

# b)
boot_fn <- function(data = Default, index) {
    # income <- data$income[index]
    # balance <- data$balance[index]
    model <- glm(default ~ income + balance, data = data, subset = index, family = "binomial")
    # return standard errors
    # standard_errors <- extract_standard_errors_from_glm(model)
    return(coef(model))
}

boot_fn(index = sample(nrow(Default), (nrow(Default) / 2), replace = TRUE))
# c)
library("boot")
boot(Default, boot_fn, R = 1000)
# d) Bootstrap SE(beta) are higher, but more accurate, since
# they don't rely on assumptions about the variance of the error term
summary(default_model)

# 7.
set.seed(2)
head(Weekly)
names(Weekly)
# a)
direction_model <- glm(Direction ~ Lag1 + Lag2, data = Weekly, family = "binomial")
summary(direction_model)
# b)
# select all rows of weekly from 2 till end
train <- Weekly[-1, ]
# all_but_first_weekly <- Weekly[2:nrow(Weekly), ]
direction_model2 <- glm(Direction ~ Lag1 + Lag2, data = train, family = "binomial")
summary(direction_model2)
# c)
test <- Weekly[1, ]
predict <- predict(direction_model2, newdata = test, type = "response")
# P(Direction = "Up"|Lag1, Lag2) > 0.5
ifelse(predict > 0.5, "Up", "Down") == test$Direction # FALSE => first observation wrongly classified
# d)
n <- nrow(Weekly)
n_seq <- seq_len(n)
error_container <- rep(0, n)

for (i in n_seq) {
    index <- seq(i, n)
    print(length(index))
    model <- glm(Direction ~ Lag1 + Lag2, data = Weekly, subset = index, family = "binomial")
    # not sure if we should use the same subset
    test <- Weekly[i, ]
    predict <- predict(model, newdata = test, type = "response")
    print(predict)
    predicted_value <- ifelse(predict > 0.5, "Up", "Down")
    error_container[i] <- ifelse(predicted_value != test$Direction, 1, 0)
}

# e) 43.7% Wrong, which is not so good
mean(error_container == 1) # 0.437
# OR
length(error_container[error_container == 1]) / n # 0.437

# 8.
# a) Generate data
set.seed(1)
x <- rnorm(100)
y <- x - 2 * x^2 + rnorm(100)

# n = 100, p = 1(?)
# y = (X - 2) * X^2
# b) plot
plot(x, y) # quadratic function(?)

# c)
set.seed(2)
data <- data.frame(y, x, x2 = x^2, x3 = x^3, x4 = x^4)

model1 <- glm(y ~ x, data = data)
cv_error1 <- cv.glm(data, model1)
cv_error1$delta

model2 <- glm(y ~ x + x2, data = data)
cv_error2 <- cv.glm(data, model2)
cv_error2$delta

model3 <- glm(y ~ x + x2 + x3, data = data)
cv_error3 <- cv.glm(data, model3)
cv_error3$delta

model4 <- glm(y ~ x + x2 + x3 + x4, data = data)
cv_error4 <- cv.glm(data, model4)
cv_error4$delta

cv_error_container <- list(cv_error1$delta, cv_error2$delta, cv_error3$delta, cv_error4$delta)
# d)
set.seed(8)
data <- data.frame(y, x, x2 = x^2, x3 = x^3, x4 = x^4)

model1 <- glm(y ~ x, data = data)
cv_error1 <- cv.glm(data, model1)
cv_error1$delta

model2 <- glm(y ~ x + x2, data = data)
cv_error2 <- cv.glm(data, model2)
cv_error2$delta

model3 <- glm(y ~ x + x2 + x3, data = data)
cv_error3 <- cv.glm(data, model3)
cv_error3$delta

model4 <- glm(y ~ x + x2 + x3 + x4, data = data)
cv_error4 <- cv.glm(data, model4)
cv_error4$delta
cv_error_container2 <- list(cv_error1$delta, cv_error2$delta, cv_error3$delta, cv_error4$delta)

# Results are the same regardless of seed. We're using the same data set and we're just
# shuffling the data around

# e)
# second model x^2 has the lowest error, i.e. the true model

# f)
summary(model1) # x signif. at 5%
summary(model2) # x and x2 signif. at any level
summary(model3) # x3 not significant
summary(model4) # x3, x4 not significant
# It seems that we'll identify the true model again at > 1% level

# 9
# a)
head(Boston)
medv_mu <- mean(Boston$medv)
medv_mu
# b)
medv_sd <- sd(Boston$medv) / sqrt(nrow(Boston))
medv_sd
# c)
set.seed(1)
mean_fn <- function(variable, index) {
    return(mean(variable[index]))
}
booted_mu <- boot(Boston$medv, mean_fn, R = 1000) # SE: 0.39
medv_mu # 22.53
medv_sd # 0.4088611
# d)
booted_mu$t0 - 2 * sd(booted_mu$t) # lower bound
booted_mu$t0 + 2 * sd(booted_mu$t) # upper bound

# e)
medv_median <- median(Boston$medv)
medv_median

# f)
median_fn <- function(variable, index) {
    return(median(variable[index]))
}

boot_median <- boot(Boston$medv, median_fn, R = 1000)
boot_median # std. error 0.386

# g)
medv_mu10quantile <- quantile(Boston$medv, 0.1)
medv_mu10quantile
# h)
set.seed(10)
quantile10_fn <- function(variable, index) {
    return(quantile(variable[index], 0.1))
}
booted_quantile10 <- boot(Boston$medv, quantile10_fn, R = 1000)
booted_quantile10 # std. error 0.50
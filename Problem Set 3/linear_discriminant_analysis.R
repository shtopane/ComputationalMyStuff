# 1. Exercise
# 1.1 Load packages
library("mvtnorm")
library("MASS")
library("ggplot2")

set.seed(555)

# CONSTANTS
class1_name <- "Class 1"
class2_name <- "Class 2"
class1_mu <- c(-3, 3)
class2_mu <- c(5, 5)
class1_n <- 300
class2_n <- 500
sigma_cap <- rbind(c(16, -2), c(-2, 9))

generate_data_lda <- function(class1_n, class2_n, class1_mu, class2_mu, class1_sigma_cap = sigma_cap, class2_sigma_cap = sigma_cap) {
    class_type <- factor(c(rep(class1_name, class1_n), rep(class2_name, class2_n)))

    class1_x <- mvrnorm(class1_n, class1_mu, class1_sigma_cap) # nolint
    class2_x <- mvrnorm(class2_n, class2_mu, class2_sigma_cap) # nolint
    together <- rbind(class1_x, class2_x)

    data <- data.frame(class_type, "C1" = together[, 1], "C2" = together[, 2])

    return(data)
}

train_data <- generate_data_lda(class1_n, class2_n, class1_mu, class2_mu)

plot1 <- ggplot(train_data, aes(C1, C2, color = class_type)) +
    geom_point()

plot1

# 1. b) LDA
train_lda <- lda(class_type ~ C1 + C2, data = train_data)

# Why do we use train_data, e.g. original again?
train_lda_predict <- predict(train_lda, newdata = train_data)
head(train_lda_predict$class, 10)
head(train_lda_predict$posterior, 10)
head(train_lda_predict$x, 10)

plot2 <- ggplot(train_data, aes(C1, C2, color = train_lda_predict$class)) +
    geom_point() +
    ggtitle("LDA Prediction")
plot2

# Logistic regression
get_class_logit <- function(prediction, class1_name, class2_name) {
    factor(ifelse(prediction > 0.5, class2_name, class1_name))
}

train_logit <- glm(class_type ~ C1 + C2, data = train_data, family = "binomial")
train_logit_predict <- predict(train_logit, newdata = train_data, type = "response")
train_logit_class_type <- get_class_logit(train_logit_predict, class1_name, class2_name)

plot3 <- ggplot(train_data, aes(C1, C2, color = train_logit_class_type)) +
    geom_point() +
    ggtitle("Logit Prediction")
plot3

# 1.c
get_average_prediction_error <- function(actual, predicted) {
    mean(actual != predicted)
}

train_lda_error <- get_average_prediction_error(train_data$class_type, train_lda_predict$class)
train_logit_error <- get_average_prediction_error(train_data$class, train_logit_class_type)

# Generate test data, predict and calculate error
test_data <- generate_data_lda(class1_n, class2_n, class1_mu, class2_mu)

test_lda_predict <- predict(train_lda, newdata = test_data)
test_logit_predict <- predict(train_logit, newdata = test_data, type = "response")
test_logit_predict_class_type <- get_class_logit(test_logit_predict, class1_name, class2_name)

test_lda_error <- get_average_prediction_error(test_data$class_type, test_lda_predict$class)
test_logit_error <- get_average_prediction_error(test_data$class_type, test_logit_predict_class_type)

# 1.d) What other performance dimension could you analyze?

# 2.a) Simulation study
simulation_runs <- 100
train_lda_error_container <- rep(NA, simulation_runs)
train_logit_error_container <- rep(NA, simulation_runs)
test_lda_error_container <- rep(NA, simulation_runs)
test_logit_error_container <- rep(NA, simulation_runs)

for (i in 1:simulation_runs) {
    train_data <- generate_data_lda(class1_n, class2_n, class1_mu, class2_mu)
    test_data <- generate_data_lda(class1_n, class2_n, class1_mu, class2_mu)

    # Estimate lda + logit for train data
    train_lda <- lda(class_type ~ C1 + C2, data = train_data)
    train_lda_predict <- predict(train_lda, newdata = train_data)

    train_logit <- glm(class_type ~ C1 + C2, data = train_data, family = "binomial")
    train_logit_predict <- predict(train_logit, newdata = train_data, type = "response")
    train_logit_predict_class_type <- get_class_logit(train_logit_predict, class1_name, class2_name)

    # Calculate errors
    train_lda_error_container[i] <- get_average_prediction_error(train_data$class_type, train_lda_predict$class)
    train_logit_error_container[i] <- get_average_prediction_error(train_data$class_type, train_logit_predict_class_type)

    # Estimate lda + logit for test data
    test_lda_predict <- predict(train_lda, newdata = test_data)
    test_logit_predict <- predict(train_logit, newdata = test_data, type = "response")
    test_logit_predict_class_type <- get_class_logit(test_logit_predict, class1_name, class2_name)

    # Calculate errors
    test_lda_error_container[i] <- get_average_prediction_error(test_data$class_type, test_lda_predict$class)
    test_logit_error_container[i] <- get_average_prediction_error(test_data$class_type, test_logit_predict_class_type)
}

# Plot LDA errors
plot(
    1:simulation_runs,
    train_lda_error_container,
    xlab = "Simulation runs",
    ylab = "Error rate",
    main = "LDA Error rate for Train and Test data",
    col = "blue",
    pch = 21
)
points(test_lda_error_container, col = "red")
abline(h = mean(train_lda_error_container))
abline(h = mean(test_lda_error_container))
legend(
    x = "topleft",
    bty = "n",
    legend = c("Train", "Test"), lty = c(1, 1), col = c("blue", "red")
)


# Plot Logit errors
plot(
    1:simulation_runs,
    train_logit_error_container,
    xlab = "Simulation runs",
    ylab = "Error rate",
    main = "Logit Error rate for Train and Test data",
    col = "blue",
    pch = 21
)
points(test_logit_error_container, col = "red")
abline(h = mean(train_logit_error_container))
abline(h = mean(test_logit_error_container))
legend(
    x = "topleft",
    bty = "n",
    legend = c("Train", "Test"), lty = c(1, 1), col = c("blue", "red")
)

# 2.b) Attempt to worsen the relative performance of LDA by allowing the covariance matrices to differ
sigma_adjustment_factor <- seq(1, 10, by = 0.2)
sigma_adjustment_factor_length <- length(sigma_adjustment_factor)
# error_length <- sigma_adjustment_factor_length * simulation_runs

test_lda_new_error_container <- rep(NA, simulation_runs)
test_logit_new_error_container <- rep(NA, simulation_runs)
test_lda_new_error_mean_container <- rep(NA, sigma_adjustment_factor_length)
test_logit_new_error_mean_container <- rep(NA, sigma_adjustment_factor_length)

for (i in 1:sigma_adjustment_factor_length) {
    # Compute new sigma matrix, adjusted by the current adjustment factor
    sigma_adjusted_class_2 <- sigma_cap * sigma_adjustment_factor[i]

    for (j in 1:simulation_runs) {
        train_data <- generate_data_lda(
            class1_n,
            class2_n,
            class1_mu,
            class2_mu,
            class1_sigma_cap = sigma_cap,
            class2_sigma_cap = sigma_adjusted_class_2
        )

        test_data <- generate_data_lda(
            class1_n,
            class2_n,
            class1_mu,
            class2_mu,
            class1_sigma_cap = sigma_cap,
            class2_sigma_cap = sigma_adjusted_class_2
        )

        train_lda <- lda(class_type ~ C1 + C2, data = train_data)
        train_logit <- glm(class_type ~ C1 + C2, data = train_data, family = "binomial")

        test_lda_predict <- predict(train_lda, newdata = test_data)
        test_logit_predict <- predict(train_logit, newdata = test_data, type = "response")
        test_logit_predict_class_type <- get_class_logit(test_logit_predict, class1_name, class2_name)

        test_lda_new_error_container[j] <- get_average_prediction_error(train_data$class_type, test_lda_predict$class)
        test_logit_new_error_container[j] <- get_average_prediction_error(train_data$class_type, test_logit_predict_class_type)
    }

    test_lda_new_error_mean_container[i] <- mean(test_lda_new_error_container)
    test_logit_new_error_mean_container[i] <- mean(test_logit_new_error_container)
}

plot(1:sigma_adjustment_factor_length, test_lda_new_error_mean_container, type = "l")
lines(test_logit_new_error_mean_container, col = "red")

# 2.b) Worsen LDA by changing sample sizes for the 2 classes
simulation_class1_sample_size <- seq(50, 500, le = 10)
simulation_class1_sample_size <- as.integer(simulation_class1_sample_size)
simulation_class2_sample_size <- seq(800, 500, le = 10)
simulation_class2_sample_size <- as.integer(simulation_class2_sample_size)

simulation_test_lda_error <- rep(NA, simulation_runs)
simulation_test_logit_error <- rep(NA, simulation_runs)

simulation_test_lda_mean_error <- rep(NA, 10)
simulation_test_logit_mean_error <- rep(NA, 10)

for(i in 1:10){
    class1_sample <- simulation_class1_sample_size[i]
    class2_sample <- simulation_class2_sample_size[i]

    for(j in 1:simulation_runs){
        train_data <- generate_data_lda(
            class1_n = class1_sample,
            class2_n = class2_sample,
            class1_mu,
            class2_mu,
        )
        test_data <- generate_data_lda(
            class1_n = class1_sample,
            class2_n = class2_sample,
            class1_mu,
            class2_mu,
        )

        train_lda <- lda(class_type ~ C1 + C2, data = train_data)
        train_logit <- glm(class_type ~ C1 + C2, data = train_data, family = "binomial")

        test_lda_predict <- predict(train_lda, newdata = test_data)
        test_logit_predict <- predict(train_logit, newdata = test_data, type = "response")
        test_logit_predict_class_type <- get_class_logit(test_logit_predict, class1_name, class2_name)

        simulation_test_lda_error[j] <- get_average_prediction_error(test_data$class_type, test_lda_predict$class)
        simulation_test_logit_error[j] <- get_average_prediction_error(test_data$class_type, test_logit_predict_class_type)
    }

    simulation_test_lda_mean_error[i] <- mean(simulation_test_lda_error)
    simulation_test_logit_mean_error[i] <- mean(simulation_test_logit_error)
}

plot(1:10, simulation_test_lda_mean_error)
points(simulation_test_logit_mean_error)
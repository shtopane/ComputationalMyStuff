# Exercise 1
# Parameters
set.seed(555)

sample_size <- 40
beta <- cbind(c(1, 1.5, -1.5, 1.5, 0.5))
sigma <- 1.2
bootstrap_size <- 100000

get_standard_error <- function(sample) {
    sqrt(var(sample) / length(sample))
}

get_standard_deviation <- function(sample) {
    get_standard_error(sample) / sqrt(length(sample))
}

generate_data <- function(n, beta, sigma) {
    rnorm_mean <- 0
    rnorm_sd <- 1

    error <- rnorm(n, rnorm_mean, rnorm_sd)
    x1 <- rnorm(n, rnorm_mean, sigma)
    intercept <- rep(1, n)
    x <- cbind(intercept, x1, x1^2, x1^3, x1^4)
    y <- x %*% beta + error

    data <- data.frame(
        "Y" = y,
        "X0" = x[, 1],
        "X1" = x[, 2],
        "X2" = x[, 3],
        "X3" = x[, 4],
        "X4" = x[, 5]
    )

    return(data)
}

train_data <- generate_data(sample_size, beta, sigma)

train_lm <- lm(Y ~ . - 1, data = train_data)

hist(train_data$Y)
hist(train_lm$fitted.values)
# true_standard_error <- get_standard_error(train_data$Y)
estimated_standard_error <- get_standard_error(train_lm$fitted.values)

confidence_intervals <- matrix(NA, nrow = sample_size, ncol = 2)

for (i in 1:sample_size) {
    lower_ci <- train_lm$fitted.values[i] - 2 * estimated_standard_error
    upper_ci <- train_lm$fitted.values[i] + 2 * estimated_standard_error
    confidence_intervals[i, ] <- c(lower_ci, upper_ci)
}
# Functions
# b) Log likelihood
myloglikelihood <- function(beta) {
  loglikelihood <-
    sum(-y * log(1 + exp(-(X %*% beta))) - (1 - y) * log(1 + exp(X %*% beta)))
  return(loglikelihood)
}

# b) Normal Likelihood
mylikelihood <- function(beta) {
  linear_model <- beta[1] + beta[2] * x + beta[3] * x2

  likelihood <-
    prod((exp(linear_model) / (1 + exp(linear_model)))^y * (1 - (exp(linear_model) / (
      1 + exp(linear_model)
    )))^(1 - y))
  return(likelihood)
}

calculate_prob_x <- function(estimators_vector, x_vector) {
  result <- (exp(x_vector[1] * estimators_vector[1] + x_vector[2] * estimators_vector[2] + x_vector[3] * estimators_vector[3])
  / (1 + exp(x_vector[1] * estimators_vector[1] + x_vector[2] * estimators_vector[2] + x_vector[3] * estimators_vector[3]))
  )

  return(result)
}

find_max_value_after_curve <- function(x, y) {
  result <- x[which(y == (max(y, na.rm = TRUE)))]
  return(result)
}

get_marginal_effect_beta_1 <- function(length = n,
                                       # x2 vector default value
                                       x2 = 0,
                                       # estimated quantity from likelihood or loglikelihood function
                                       estimate,
                                       dx_container = NULL,
                                       probability_container = NULL) {
  if (!is.null(estimate)) {
    # Create containers
    dx_container <- rep(0, length(x))
    probability_container <- rep(0, length(x))

    for (i in 1:length) {
      prob_i <-
        exp(estimate[1] + estimate[2] * x[i] + estimate[3] * x2) / (1 + exp(estimate[1] + estimate[2] * x[i] + estimate[3] * x2))

      dx_container[i] <- estimate[2] * prob_i * (1 - prob_i)
      probability_container[i] <- prob_i
    }
    return(
      list(
        dx_container = dx_container,
        probability_container = probability_container
      )
    )
  }

  return(NULL)
}

set.seed(123)
# Parameters
n <- 1000
x <- sort(runif(n = n, min = 18, max = 60))
x2 <- rbinom(n, 1, prob = 0.5)
X <- cbind(rep(1, n), x, x2) # nolint

beta0 <- -2
beta1 <- 0.1
beta2 <- 1
beta <- cbind(beta0, beta1, beta2)

linear_model <- beta0 + beta1 * x + beta2 * x2
prob_i_x <- exp(linear_model) / (1 + exp(linear_model))

# a) ?
y <- rbinom(n = length(x), size = 1, prob = prob_i_x)
data <- data.frame(
  "Y" = y,
  # "X0" = X[, 1], don't add intercept
  "X1" = X[, 2],
  "X2" = X[, 3]
)

# c) Plot the likelihood function and the log-likelihood function for a range of values for the two parameters
# separately and show that they are maximized at the same value. ----

# c) 1. Likelihood
# evaluate_for = "likelihood / loglikelihood"
plot_like <- function(evaluate_for, beta0, beta1, beta2) {
  # betas <- c(beta0, beta1, x)
  betas <- c(beta0, beta1, beta2)
  result <- NULL

  if (evaluate_for == "likelihood") {
    result <- mylikelihood(betas)
  } else if (evaluate_for == "loglikelihood") {
    result <- myloglikelihood(betas)
  }

  return(result)
}

# General variables for plotting
expr_plot_like <- Vectorize(plot_like)
evaluated_function_name <- "likelihood"
beta_0_to_from <- list(
  from = -3.00,
  to = 0
)
beta_1_to_from <- list(
  from = 0,
  to = 0.1897
)
beta_2_to_from <- list(
  from = 0,
  to = 2.00
)

# For beta0

result_beta0 <- curve(
  expr = expr_plot_like(evaluated_function_name, x, beta1, beta2),
  from = beta_0_to_from$from,
  to = beta_0_to_from$to,
  xlab = "Range of beta 0",
  ylab = "mylikelihood(beta0)",
  main = "Evaluating Likelihood function for beta0"
)
max_of_result_beta0 <- find_max_value_after_curve(result_beta0$x, result_beta0$y)
# max_of_result_beta0 <-
#   result_beta0$x[which(result_beta0$y == (max(result_beta0$y, na.rm = TRUE)))]
max_of_result_beta0
abline(v = max_of_result_beta0, col = "red", lty = 2)

# For beta 1
result_beta1 <- curve(
  expr = expr_plot_like(evaluated_function_name, beta0, x, beta2),
  from = beta_1_to_from$from,
  to = beta_1_to_from$to,
  xlab = "Range of beta 1",
  ylab = "mylikelihood(beta1)",
  main = "Evaluating Likelihood function for beta1"
)
max_of_result_beta1 <- find_max_value_after_curve(result_beta1$x, result_beta1$y)
# max_of_result_beta1 <-
#   result_beta1$x[which(result_beta1$y == (max(result_beta1$y, na.rm = TRUE)))]
max_of_result_beta1
abline(v = max_of_result_beta1)

# For beta 2
result_beta2 <-
  curve(
    expr = expr_plot_like(evaluated_function_name, beta0, beta1, x),
    from = beta_2_to_from$from,
    to = beta_2_to_from$to,
    xlab = "Range of beta 2",
    ylab = "mylikelihood(beta2)",
    main = "Evaluating Likelihood function for beta2"
  )
max_of_result_beta2 <- find_max_value_after_curve(result_beta2$x, result_beta2$y)
# max_of_result_beta2 <-
#   result_beta2$x[which(result_beta2$y == (max(result_beta2$y, na.rm = TRUE)))]
max_of_result_beta2
abline(v = max_of_result_beta2)

# c) 2. Log-likelihood
# For beta0
evaluated_function_name <- "loglikelihood"
result_log_beta_0 <-
  curve(
    expr = expr_plot_like(evaluated_function_name, x, beta1, beta2),
    from = beta_0_to_from$from,
    to = beta_0_to_from$to,
    xlab = "Range of beta 0",
    ylab = "myloglikelihood(beta0)",
    main = "Evaluating Log-Likelihood function for beta0"
  )
max_of_result_log_beta_0 <- find_max_value_after_curve(result_log_beta_0$x, result_log_beta_0$y)
# max_of_result_log_beta_0 <-
#   result_log_beta_0$x[which(result_log_beta_0$y == (max(result_log_beta_0$y, na.rm = TRUE)))]
max_of_result_log_beta_0
abline(v = max_of_result_log_beta_0)
# For beta1
result_log_beta_1 <-
  curve(
    expr = expr_plot_like(evaluated_function_name, beta0, x, beta2),
    from = beta_1_to_from$from,
    to = beta_1_to_from$to,
    xlab = "Range of beta 1",
    ylab = "myloglikelihood(beta1)",
    main = "Evaluating Log-Likelihood function for beta1"
  )
max_of_result_log_beta_1 <- find_max_value_after_curve(result_log_beta_1$x, result_log_beta_1$y)
# max_of_result_log_beta_1 <-
#   result_log_beta_1$x[which(result_log_beta_1$y == (max(result_log_beta_1$y, na.rm = TRUE)))]
max_of_result_log_beta_1
abline(v = max_of_result_log_beta_1)
# For beta2
result_log_beta_2 <-
  curve(
    expr = expr_plot_like(evaluated_function_name, beta0, beta1, x),
    from = beta_2_to_from$from,
    to = beta_2_to_from$to,
    xlab = "Range of beta 2",
    ylab = "myloglikelihood(beta2)",
    main = "Evaluating Log-Likelihood function for beta2"
  )
max_of_result_log_beta_2 <- find_max_value_after_curve(result_log_beta_2$x, result_log_beta_2$y)
# max_of_result_log_beta_2 <-
#   result_log_beta_2$x[which(result_log_beta_2$y == (max(result_log_beta_2$y, na.rm = TRUE)))]
max_of_result_log_beta_2
abline(v = max_of_result_log_beta_2)
# End for c) -----

# d) Estimate ß0, ß1, ß2 via maximum likelihood and calculate the standard errors. Use the estimation template
# provided in the lecture.
library(maxLik)
# Getting an estimate for log-likelihood
start_param <- c(0, 1, 1)
loglikelihood_estimate <- maxBFGS(myloglikelihood,
  finalHessian = TRUE,
  start = start_param
)
loglikelihood_estimate$estimate

loglikelihood_covariance_matrix <-
  - (solve(loglikelihood_estimate$hessian))

loglikelihood_sde <- sqrt(diag(loglikelihood_covariance_matrix))
loglikelihood_sde

# e) Propose and calculate a suitable method for the interpretation of the coefficients as discussed in the lecture.
# Not sure if this is what is asked?
x_len <- length(x)

# Marginal effects when x2 = 0
loglikelihood_result <- get_marginal_effect_beta_1(
  length = x_len,
  estimate = loglikelihood_estimate$estimate,
  x2 = 0
)
# Marginal effects when x2 = 1
loglikelihood_result1 <- get_marginal_effect_beta_1(
  length = x_len,
  estimate = loglikelihood_estimate$estimate,
  x2 = 1
)
# Plotting marginal effects of beta1 when x2=0
plot(
  x,
  loglikelihood_result$dx_container,
  type = "l",
  main = "Log-Likelihood estimate",
  ylab = "dx"
)

# # Predicted for x=0
plot(
  x,
  loglikelihood_result$probability_container,
  type = "l",
  ylim = c(0, 1)
)
abline(h = 0.1, col = "red")
abline(h = 0.5, col = "red")
abline(h = 0.9, col = "red")
# Predicted for x=1
plot(
  x,
  loglikelihood_result1$probability_container,
  type = "l",
  ylim = c(0, 1)
)
abline(h = 0.1, col = "red")
abline(h = 0.5, col = "red")
abline(h = 0.9, col = "red")

# f) Visualize results(Confidence intervals?)

model <- glm(
  formula =
    y ~ X1 + X2,
  family = binomial(link = "logit"),
  data = data
)
# Getting some regressor X for which the coefficient is NA...
summary(model)
# Log-odds
summary(model)$coefficients

# Confidence intervals Log-odds, Beta 1
confint.default(model)[2, ]
# # Confidence intervals Odds, Beta 1
exp(confint.default(model)[2, ]) # between 9.21% and 14.09%

# Model prediction
set.seed(666)
# Draw new data?
n_test <- 1000
x_test <- sort(runif(n = n_test, min = 18, max = 60))
x2_test <- rbinom(n = n_test, 1, prob = 0.5)
X_test <- cbind(rep(1, n_test), x, x2) # nolint
data_test <- data.frame("X1" = X[, 2], "X2" = X[, 3])

# with increasing X -> increasing probability
predict(model, newdata = data_test, type = "response")

# Visualizing results: Confidence Intervals
set.seed(689)
x_to_visualize <- sort(runif(n = n_test, min = 18, max = 60))

# When x2 = 0
plot_confidence_intervals <- function(x2 = 0, xlab) {
  predicted_model <-
    predict(
      model,
      newdata = data.frame(X1 = x_to_visualize, X2 = x2),
      type = "response",
      se.fit = TRUE
    )
  predicted_model_fit <- predicted_model$fit
  predicted_model_upper <-
    predicted_model$fit + predicted_model$se.fit * 1.96 # 95% confidence interval
  predicted_model_lower <-
    predicted_model$fit - predicted_model$se.fit * 1.96 # 95% confidence interval

  plot(
    x,
    y,
    pch = 16,
    cex = 1,
    ylab = "Probability",
    xlab = xlab
  )
  grid()
  polygon(c(rev(x_to_visualize), x_to_visualize),
    c(rev(predicted_model_lower), predicted_model_upper),
    col = "grey90",
    border = NA
  )

  lines(x_to_visualize, predicted_model_fit, lwd = 2)
  lines(x_to_visualize, predicted_model_upper, lwd = 2, col = "red")
  lines(x_to_visualize, predicted_model_lower, lwd = 2, col = "red")

  abline(h = 0.1, lty = 2)
  abline(h = 0.5, lty = 2)
  abline(h = 0.9, lty = 2)
}

# Plot for X2=0
plot_confidence_intervals(x2 = 0, xlab = "X1's when X2=0")
# Plot for X2=1
plot_confidence_intervals(x2 = 1, xlab = "X1's when X2=1")

# 2. Simulation study
n <- 1000
beta0 <- -2
beta1 <- 0.1
beta2 <- 1
beta <- cbind(beta0, beta1, beta2)

# Simulation
set.seed(2198)
simulation_runs <- 55
ape_count <- rep(NA, simulation_runs)
mse_count <- rep(NA, simulation_runs)

for (i in 1:simulation_runs) {
  train_x <- runif(n, min = 18, max = 60)
  train_x2 <- rbinom(n, size = 1, prob = 0.5)
  train <- cbind(rep(1, n), train_x, train_x2)
  pi_x <- calculate_prob_x(beta, train)
  y <- rbinom(n, size = 1, prob = pi_x)

  # Estimate Model
   # 2. Estimate betas
  train_model <- glm(formula = y ~ train_x + train_x2, family = "binomial")
  train_beta_hats <- c(train_model$coefficients[1], train_model$coefficients[2], train_model$coefficients[3])


  train_pi_x <- calculate_prob_x(train_beta_hats, train)
  train_y <- rbinom(n = n, size = 1, prob = train_pi_x)

  # Generate test data
  test_x <- runif(n, min = 18, max = 60)
  test_x2 <- rbinom(n, size = 1, prob = 0.5)
  test <- cbind(rep(1, n), test_x, test_x2)

  test_pi_x <- calculate_prob_x(beta, test)
  test_y <- rbinom(n, size = 1, prob = test_pi_x)
  test_pi_x_predicted <- calculate_prob_x(train_beta_hats, test)
  test_y_predicted <- rbinom(n, size = 1, prob = test_pi_x_predicted)

  mse_count[i] <- 1 / n * sum(!(y == train_y))
  ape_count[i] <- 1 / n * sum(!(test_y == test_y_predicted))
}

# Plotting
plot(mse_count, xlab = "Number of Simulation", ylab = "Error")
points(ape_count, col = "red")
abline(h = mean(mse_count))
abline(h = mean(ape_count), col = "red")
legend(x = "topleft", legend = c("APE", "MSE"), lty = c(1, 1), col = c("red", "black"))

# b) Change prob in x2 from 0.5 to 0.2
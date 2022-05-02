# Functions
# b) Log likelihood
myloglikelihood <- function(beta) {
  loglikelihood <-
    sum(-y * log(1 + exp(-(X %*% beta))) - (1 - y) * log(1 + exp(X %*% beta)))
  return(loglikelihood)
}

# b) Normal Likelihood
mylikelihood <- function(beta) {
  # likelihood <- I(beta) # this produces very large numbers
  
  # (2) - our linear model
  #likelihood <- beta[0] + beta[1] * x + beta[2] * x2
  
  # (3) max
  
  linear_model <- beta[1] + beta[2] * x + beta[3] * x2
  # prod((exp(linear_model) / (1 + exp(linear_model)))^y * (1 - (exp(linear_model)/(1 - exp(linear_model))))^(1 - y))
  likelihood <-
    prod((exp(linear_model) / (1 + exp(linear_model))) ^ y * (1 - (exp(linear_model) / (
      1 + exp(linear_model)
    ))) ^ (1 - y))
  # likelihood <- prod((prob_i_x^y) * ((1 - prob_i_x)^(1 - y))) # this behaves the same as (2)
  return(likelihood)
}

getMarginalEffectBeta1 <- function(length = n,
                                   x2 = 0,
                                   # likelihood OR log-likelihood
                                   estimate_for = "likelihood",
                                   dx_container = NULL,
                                   probability_container = NULL) {
  estimate <- NULL
  
  if (estimate_for == "likelihood") {
    estimate <- likelihood.estimate$estimate
  } else if (estimate_for == "loglikelihood") {
    estimate <- loglikelihood.estimate$estimate
  }
  
  if (!is.null(estimate)) {
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
X <- cbind(rep(1, n), x, x2)

beta0 <- -2
beta1 <- 0.1
beta2 <- 1
beta <- cbind(beta0, beta1, beta2)

linear_model <- beta0 + beta1 * x + beta2 * x2
prob_i_x <- exp(linear_model) / (1 + exp(linear_model))

# a) ?
y <- rbinom(n = length(x), size = 1, prob = prob_i_x)

# c) Plot the likelihood function and the log-likelihood function for a range of values for the two parameters
# separately and show that they are maximized at the same value. ----

# c) 1. Likelihood
# evaluate_for = "likelihood / loglikelihood"
plotlike <- function(evaluate_for, beta0, beta1, beta2) {
  #betas <- c(beta0, beta1, x)
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
expr_plotlike <- Vectorize(plotlike)
evaluated_function_name <- "likelihood"
beta_0_to_from <- list(from = -3.00,
                       to = 0)
beta_1_to_from <- list(from = 0,
                       to = 0.1897)
beta_2_to_from <- list(from = 0,
                       to = 2.00)

# For beta0

result_beta0 <- curve(
  expr = expr_plotlike(evaluated_function_name, x, beta1, beta2),
  from = beta_0_to_from$from,
  to = beta_0_to_from$to,
  xlab = "Range of beta 0",
  ylab = "mylikelihood(beta0)",
  main = "Evaluating Likelihood function for beta0"
)
max_of_result_beta0 <-
  result_beta0$x[which(result_beta0$y == (max(result_beta0$y, na.rm = TRUE)))]
max_of_result_beta0
abline(v = max_of_result_beta0)

# For beta 1
result_beta1 <- curve(
  expr = expr_plotlike(evaluated_function_name, beta0, x, beta2),
  from = beta_1_to_from$from,
  to = beta_1_to_from$to,
  xlab = "Range of beta 1",
  ylab = "mylikelihood(beta1)",
  main = "Evaluating Likelihood function for beta1"
)
max_of_result_beta1 <-
  result_beta1$x[which(result_beta1$y == (max(result_beta1$y, na.rm = TRUE)))]
max_of_result_beta1
abline(v = max_of_result_beta1)

# For beta 2
result_beta2 <-
  curve(
    expr = expr_plotlike(evaluated_function_name, beta0, beta1, x),
    from = beta_2_to_from$from,
    to = beta_2_to_from$to,
    xlab = "Range of beta 2",
    ylab = "mylikelihood(beta2)",
    main = "Evaluating Likelihood function for beta2"
  )
max_of_result_beta2 <-
  result_beta2$x[which(result_beta2$y == (max(result_beta2$y, na.rm = TRUE)))]
max_of_result_beta2
abline(v = max_of_result_beta2)

# c) 2. Log-likelihood
# For beta0
evaluated_function_name <- "loglikelihood"
result_log_beta_0 <-
  curve(
    expr = expr_plotlike(evaluated_function_name, x, beta1, beta2),
    from = beta_0_to_from$from,
    to = beta_0_to_from$to,
    xlab = "Range of beta 0",
    ylab = "myloglikelihood(beta0)",
    main = "Evaluating Log-Likelihood function for beta0"
  )
max_of_result_log_beta_0 <-
  result_log_beta_0$x[which(result_log_beta_0$y == (max(result_log_beta_0$y, na.rm = TRUE)))]
max_of_result_log_beta_0
abline(v = max_of_result_log_beta_0)
# For beta1
result_log_beta_1 <-
  curve(
    expr = expr_plotlike(evaluated_function_name, beta0, x, beta2),
    from = beta_1_to_from$from,
    to = beta_1_to_from$to,
    xlab = "Range of beta 1",
    ylab = "myloglikelihood(beta1)",
    main = "Evaluating Log-Likelihood function for beta1"
  )
max_of_result_log_beta_1 <-
  result_log_beta_1$x[which(result_log_beta_1$y == (max(result_log_beta_1$y, na.rm = TRUE)))]
max_of_result_log_beta_1
abline(v = max_of_result_log_beta_1)
# For beta2
result_log_beta_2 <-
  curve(
    expr = expr_plotlike(evaluated_function_name, beta0, beta1, x),
    from = beta_2_to_from$from,
    to = beta_2_to_from$to,
    xlab = "Range of beta 2",
    ylab = "myloglikelihood(beta2)",
    main = "Evaluating Log-Likelihood function for beta2"
  )
max_of_result_log_beta_2 <-
  result_log_beta_2$x[which(result_log_beta_2$y == (max(result_log_beta_2$y, na.rm = TRUE)))]
max_of_result_log_beta_2
abline(v = max_of_result_log_beta_2)
# End for c) -----

library(maxLik)
# Getting an estimate for both functions
start_param <- c(0, 1, 1)
loglikelihood.estimate <- maxBFGS(myloglikelihood,
                                  finalHessian = TRUE,
                                  start = start_param)
loglikelihood.estimate$estimate
### TODO: Pass previous estimation as this function's start parameter? This is cheating maybe
likelihood.estimate <- maxBFGS(mylikelihood,
                               finalHessian = TRUE,
                               start = loglikelihood.estimate$estimate)

likelihood.estimate$estimate

likelihood.covariance_matrix <-
  -(solve(likelihood.estimate$hessian))
loglikelihood.covariance_matrix <-
  -(solve(loglikelihood.estimate$hessian))

likelihood.sde <- sqrt(diag(likelihood.covariance_matrix))
loglikelihood.sde <- sqrt(diag(loglikelihood.covariance_matrix))

x_len <- length(x)

# Likelihood
likelihood.result <- getMarginalEffectBeta1(
  length = x_len,
  estimate_for = "likelihood",
  x2 = 0,
  dx_container = rep(0, x_len),
  probability_container = rep(0, x_len)
)
plot(
  x,
  likelihood.result$dx_container,
  type = "l",
  main = "Likelihood",
  ylab = "dx"
)

likelihood.result1 <- getMarginalEffectBeta1(
  length = x_len,
  estimate_for = "likelihood",
  x2 = 1,
  dx_container = rep(0, x_len),
  probability_container = rep(0, x_len)
)
lines(x, likelihood.result1$dx_container, col = "red")

loglikelihood.result <- getMarginalEffectBeta1(
  length = x_len,
  estimate_for = "loglikelihood",
  x2 = 0,
  dx_container = rep(0, x_len),
  probability_container = rep(0, x_len)
)
plot(
  x,
  loglikelihood.result$dx_container,
  type = "l",
  main = "Log-Likelihood estimate",
  ylab = "dx"
)

loglikelihood.result1 <- getMarginalEffectBeta1(
  length = x_len,
  estimate_for = "loglikelihood",
  x2 = 1,
  dx_container = rep(0, x_len),
  probability_container = rep(0, x_len)
)
lines(x, loglikelihood.result1$dx_container, col = "red")

# Plot probabilities
# plot(x, loglikelihood.result$probability_container, main = "Estimated Probabilities")
# lines(x, loglikelihood.result1$probability_container, col = "red")
# 
# plot(x, loglikelihood.result$probability_container, ylim = c(0, 1))
# lines(x, loglikelihood.result1$probability_container, col = "red")

# TEST f)?
logit <- function(x) {
  logit <- log(x / (1 - x))
  return (logit)
}

plot(
  logit(prob_i_x),
  prob_i_x,
  type = "l",
  xlim = c(-5, 5),
  main = "The Logit Transformation",
  xlab = "logit",
  ylab = "probability",
  cex.main = 1.2,
  cex.lab = 1,
  cex.axis = 1
)

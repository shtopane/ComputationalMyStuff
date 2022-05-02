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
                                   # x2 vector default value
                                   x2 = 0,
                                   # likelihood OR log-likelihood
                                   estimate_for = "likelihood",
                                   dx_container = NULL,
                                   probability_container = NULL) {
  estimate <- NULL
  
  if (estimate_for == "likelihood") {
    # deprecated case
    estimate <- likelihood.estimate$estimate
  } else if (estimate_for == "loglikelihood") {
    estimate <- loglikelihood.estimate$estimate
  }
  
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
X <- cbind(rep(1, n), x, x2)

beta0 <- -2
beta1 <- 0.1
beta2 <- 1
beta <- cbind(beta0, beta1, beta2)

linear_model <- beta0 + beta1 * x + beta2 * x2
prob_i_x <- exp(linear_model) / (1 + exp(linear_model))

# a) ?
y <- rbinom(n = length(x), size = 1, prob = prob_i_x)
data <- data.frame("Y" = y,
                   # "X0" = X[, 1], don't add intercept
                   "X1" = X[, 2],
                   "X2" = X[, 3])

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

# d) Estimate β0, β1, β2 via maximum likelihood and calculate the standard errors. Use the estimation template
# provided in the lecture.
library(maxLik)
# Getting an estimate for log-likelihood
start_param <- c(0, 1, 1)
loglikelihood.estimate <- maxBFGS(myloglikelihood,
                                  finalHessian = TRUE,
                                  start = start_param)
loglikelihood.estimate$estimate

loglikelihood.covariance_matrix <-
  -(solve(loglikelihood.estimate$hessian))

loglikelihood.sde <- sqrt(diag(loglikelihood.covariance_matrix))
loglikelihood.sde

# e) Propose and calculate a suitable method for the interpretation of the coefficients as discussed in the lecture.
# Not sure if this is what is asked?
x_len <- length(x)

# Marginal effects when x2 = 0
loglikelihood.result <- getMarginalEffectBeta1(length = x_len,
                                               estimate_for = "loglikelihood",
                                               x2 = 0)
# Marginal effects when x2 = 1
loglikelihood.result1 <- getMarginalEffectBeta1(length = x_len,
                                                estimate_for = "loglikelihood",
                                                x2 = 1)
# Plotting marginal effects of beta1 when x2=0
plot(
  x,
  loglikelihood.result$dx_container,
  type = "l",
  main = "Log-Likelihood estimate",
  ylab = "dx"
)
# Plotting marginal effects of beta1 when x2=1
lines(x, loglikelihood.result1$dx_container, col = "red")

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
# Not getting NA this way..
estimated_betas <-
  c(
    summary(model)$coefficients[1],
    summary(model)$coefficients[2],
    summary(model)$coefficients[3]
  )
# Odd-ratio for beta1
exp(estimated_betas[2])

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
X_test <- cbind(rep(1, n_test), x, x2)
data_test <- data.frame("X1" = X[, 2], "X2" = X[, 3])

# with increasing X -> increasing probability
predict(model, newdata = data_test, type = "response")

# Visualizing results: Confidence Intervals
set.seed(689)
x_to_visualize <- sort(runif(n = n_test, min = 18, max = 60))

# When x2 = 0
plot_confidence_itervals <- function(x2=0, xlab){
  probs <-
    predict(
      model,
      newdata = data.frame(X1 = x_to_visualize, X2 = x2),
      type = "response",
      se.fit = TRUE
    )
  probs_fit <- probs$fit
  probs_upper <- probs$fit + probs$se.fit * 1.96 # 95% confidence interval
  probs_lower <- probs$fit - probs$se.fit * 1.96 # 95% confidence interval
  
  plot(
    x,
    y,
    pch=16,
    cex=1,
    ylab="Probability",
    xlab=xlab
  )
  grid()
  polygon(
    c(rev(x_to_visualize), x_to_visualize),
    c(rev(probs_lower), probs_upper),
    col="grey90",
    border=NA
  )
  
  lines(x_to_visualize, probs_fit, lwd=2)
  lines(x_to_visualize, probs_upper, lwd=2, col="red")
  lines(x_to_visualize, probs_lower, lwd=2, col="red")
  
  abline(h=0.1, lty=2)
  abline(h=0.5, lty=2)
  abline(h=0.9, lty=2)
}

# Plot for X2=0
plot_confidence_itervals(x2=0, xlab = "X1's when X2=0")
# Plot for X2=1
plot_confidence_itervals(x2=1, xlab="X1's when X2=1")

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

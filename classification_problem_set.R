# Functions
# b) Log likelihood
myloglikelihood <- function(beta) {
  loglikelihood <-
    sum(-y * log(1 + exp(-(X %*% beta))) - (1 - y) * log(1 + exp(X %*% beta)))
  return(loglikelihood)
}
# b) Normal Likelihood
mylikelihood <- function(beta) {
  likelihood <-
    sum(-y * 1 + exp(-(X %*% beta)) - (1 - y) * (1 + exp(X %*% beta)))
  return(likelihood)
}

# Parameters
n <- 1000
x <- sort(runif(n = n, min = 18, max = 60))
x2 <- rbinom(n, 1, prob = 0.5)
X <- cbind(rep(1, n), x, x2)

beta0 <- 1
beta1 <- 0.1
beta2 <- 1
beta <- cbind(beta0, beta1, beta2)

linear_model <- beta0 + beta1 * x + beta2 * x2
prob_i_x <- exp(linear_model) / (1 + exp(linear_model))

# a) ?
y <- rbinom(n = length(x), size = 1, prob = prob_i_x)

# c) Plot the likelihood function and the log-likelihood function for a range of values for the two parameters
# separately and show that they are maximized at the same value.

# Testing functions from https://www.r-bloggers.com/2019/08/maximum-likelihood-estimation-from-scratch/

library(maxLik)
# Getting an estimate for both functions
likelihood.estimate <- maxBFGS(mylikelihood,
                               finalHessian = TRUE,
                               start = c(0, 1, 1))
loglikelihood.estimate <- maxBFGS(mylikelihood,
                                  finalHessian = TRUE,
                                  start = c(0, 1, 1))

likelihood.covariance_matrix <- -(solve(likelihood.estimate$hessian))
loglikelihood.covariance_matrix <-
  -(solve(loglikelihood.estimate$hessian))

likelihood.sde <- sqrt(diag(likelihood.covariance_matrix))
loglikelihood.sde <- sqrt(diag(loglikelihood.covariance_matrix))



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
    return(dx_container)
  }
  
  return(NULL)
}

x_len <- length(x)
likelihood.dx_container <- rep(0, x_len)

likelihood.dx_container <-
  getMarginalEffectBeta1(
    length = x_len,
    x2 = 0,
    estimate_for = "likelihood",
    dx_container = likelihood.dx_container
  )
plot(x, likelihood.dx_container, type = "l", main = "Likelihood estimate", ylab = "dx")
likelihood.dx_container1 <- rep(0, x_len)
likelihood.dx_container1 <-  getMarginalEffectBeta1(
  length = x_len,
  x2 = 1,
  estimate_for = "likelihood",
  dx_container = likelihood.dx_container1
)
lines(x, likelihood.dx_container1, col="red")

loglikelihood.dx_container <- rep(0, x_len)
loglikelihood.dx_container <- getMarginalEffectBeta1(
  length = x_len,
  estimate_for = "loglikelihood",
  x2 = 0,
  dx_container = loglikelihood.dx_container
)
plot(x, loglikelihood.dx_container, type= "l", main = "Log-Likelihood estimate", ylab = "dx")

loglikelihood.dx_container1 <- rep(0, x_len)
loglikelihood.dx_container1 <- getMarginalEffectBeta1(
  length = x_len,
  estimate_for = "loglikelihood",
  x2 = 1,
  dx_container = loglikelihood.dx_container1
)
lines(x, loglikelihood.dx_container1, col="red")

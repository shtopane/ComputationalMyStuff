# Logit transformation
## 1. Generate a vector of probabilities
pi <- seq(0, 0.9999999, le = 100)

logit <- function(x) {
  logit <- log(x / (1 - x))
  return (logit)
}

plot(
  logit(pi),
  pi,
  type = "l",
  xlim = c(-5, 5),
  main = "The Logit Transformation",
  xlab = "logit",
  ylab = "probability",
  cex.main = 1.2,
  cex.lab = 1,
  cex.axis = 1
)

# Maximum Likelihood estimation Logit
# General Syntax

# Load packages
library(maxLik)

loglikelikelihood <- function(beta) {
  ll <- "my likelihood function"
  #depending on your optimization routine,
  #check whether you need the negative or the positive log likelihood!
  return(ll)
}

#estim<-maxBFGS(loglike,finalHessian=TRUE,start=c(.,.))###initialize the optimization,
#pass on starting values and store the results in estim
#estim.par<-estim$estimate ### store the parameter estimates in a variable "estim.par"

# An empirical example for the logit model
set.seed(40)

n <- 1000 # sample size
beta0 <- -2 # intercept
beta1 <- 0.1 # coefficient of regressor
beta2 <- 1
beta <- cbind(beta0, beta1, beta2)

# Data
x <-
  sort(runif(n = n, min = 18, max = 60)) # generating the regressor
x2 <- rbinom(n, 1, 0.5)
# Equation (6) Classification.html
model <- beta0 + beta1 * x + beta2 * x2
prob_i_x <- exp(model) / (1 + exp(model))
# drawing y with the transformed logit probabilities
y <- rbinom(n = length(x), size = 1, prob = prob_i_x)

data <- cbind(x, prob_i_x)

X <- cbind(rep(1, n), x, x2)

loglikelikelihood <- function(beta) {
  ll <-
    sum(-y * log(1 + exp(-(X %*% beta))) - (1 - y) * log(1 + exp(X %*% beta)))
}

estimate <-
  maxBFGS(loglikelikelihood,
          finalHessian = TRUE,
          start = c(0, 1, 1)) #initialize estimation procedure
estimate.parameter <- estimate$estimate
estimate.hessian <- estimate$hessian

Covariance_matrix <- -(solve(estimate$hessian))
sde <- sqrt(diag(Covariance_matrix))

# How do we interpret the parameter estimates?
# marginal effect
beta1_marginal_effect.x2 <- 0
# Marginal effect for beta1
getEstimateExpression <- function(index_of_x, x2_constant) {
  result <-
    exp(
      estimate.parameter[1] + estimate.parameter[2] * x[index_of_x] + estimate.parameter[3] *
        x2_constant
    ) / (
      1 + exp(
        estimate.parameter[1] + estimate.parameter[2] * x[index_of_x] + estimate.parameter[3] *
          x2_constant
      )
    )
  return(result)
}

estimate_expression <-
  exp(
    estimate.parameter[1] + estimate.parameter[2] * x[500] + estimate.parameter[3] *
      beta1_marginal_effect.x2
  ) / (
    1 + exp(
      estimate.parameter[1] + estimate.parameter[2] * x[500] + estimate.parameter[3] *
        beta1_marginal_effect.x2
    )
  )


prob_i <- getEstimateExpression(index_of_x = 500, x2_constant = beta1_marginal_effect.x2)
dx <- estimate.parameter[2] * (prob_i) * (1 - prob_i)

# Calculate marginal effect for beta1 for a range of x values and x2=0

# Prepare function for experiment
getMarginalEffectForARangeOfX <-
  function(length,
           x2,
           container = NULL,
           probability_container = NULL) {
    for (i in 1:x_len) {
      estimate_exp <- exp(
        estimate.parameter[1] + estimate.parameter[2] * x[i] + estimate.parameter[3] *
          x2
      ) / (
        1 + exp(
          estimate.parameter[1] + estimate.parameter[2] * x[i] + estimate.parameter[3] *
            x2
        )
      )
      
      prob_i <- getEstimateExpression(i, x2)
      container[i] <-
        estimate.parameter[2] * (prob_i) * (1 - prob_i)
      
      if (!is.null(probability_container)) {
        probability_container[i] <- prob_i
      }
    }
    
    if (!is.null(probability_container)) {
      return(probability_container)
    } else if (!is.null(container)) {
      return(container)
    }
  }

x_len <- length(x)
dx_container <- rep(0, x_len)
dx_container <-
  getMarginalEffectForARangeOfX(x_len, beta1_marginal_effect.x2, dx_container)

# Plot
plot(x, dx_container, type = "l")

# Calculate marginal effect for beta1 for a range of x values and x2=1
beta1_marginal_effect2.x2 <- 1
dx_container1 <- rep(0, x_len)
dx_container1 <-
  getMarginalEffectForARangeOfX(x_len, beta1_marginal_effect2.x2, dx_container1)

# Plot line on the same plot as before - marginal effect for beta1 when x2 = 1
lines(x, dx_container1, col = "red")

# Predicted probabilities for beta2 when x2 = 0
beta2_marginal_effect.x2 <- 0
prob_i_container <- rep(0, x_len)
prob_i_container <-
  getMarginalEffectForARangeOfX(
    x_len,
    beta2_marginal_effect.x2,
    container = NULL,
    probability_container = prob_i_container
  )

plot(x, prob_i_container, type = "l")

# Predicted probabilities for beta2 when x2 = 1
beta2_marginal_effect2.x2 <- 1
prob_i_container1 <- rep(0, x_len)
prob_i_container1 <- getMarginalEffectForARangeOfX(
  x_len,
  beta2_marginal_effect2.x2,
  container = NULL,
  probability_container = prob_i_container1
)

lines(x, prob_i_container1, col = "red")


# Simulate this model with the probabilities as described above
# with the following values:
# n = 1000; b0 = -2, b1= 0.1, b2 = 1;
# x0i = 1 for all i, x1i ~ U(18,60), x2i ~ B(0.5)
# We've already do that above?
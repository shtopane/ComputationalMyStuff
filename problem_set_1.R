set.seed(666)
# Parameters
N <- 1000
true_betas <- c(5,-0.5)

X2.mean <- 0
X2.sd <- sqrt(1.5)

eps.sd <- sqrt(10)
eps.mean <- 0

# a) Generate a training sample (xi, yi)^T
X2 <- rnorm(N, X2.mean, X2.sd)
X1 <- rep(1, N)
X <- cbind(X1, X2)
eps <- rnorm(N, eps.mean, eps.sd)
Y <- X%*%true_betas + eps

training_data <- generate_samples_for_regression_two_vars(
  N,
  true_betas,
  X2.mean,
  X2.sd,
  eps.mean,
  eps.sd
)

# b) Generate a test sample using the same N
# Note: Am I asked to repeat the same process?

test_data <- generate_samples_for_regression_two_vars(
  N,
  true_betas,
  X2.mean,
  X2.sd,
  eps.mean,
  eps.sd
)

# c) Calculate beta_hat: for which data?
training_data.beta_hat1 <- cov(training_data$X2, training_data$Y)/var(training_data$X2)
test_data.beta_hat1 <- cov(test_data$X2, test_data$Y) / var(test_data$X2)
## Using lm
training_data.lm <- lm(Y ~ I(X2^4), data=training_data)
test_data.lm <- lm(Y ~ I(X2^4), data=test_data)

coef(training_data.lm)
coef(test_data.lm)

# c) calculate MSE and Ave for the 2 samples
training_data.MSE <- mean((training_data$Y - training_data.lm$fitted.values)^2)
test_data.MSE <- mean(test_data.lm$residuals^2)
## Avg prediction error
# Not sure how to calculate this!
training_data.ave <- (training_data$Y[1] - training_data.lm$fitted.values[1])^2


# 2. Simulations
set.seed(100)
MSE.container <- rep(0,N)
Ave.container <- rep(0,N)
poly_powers <- rep(c(1,2,3,4), N/4)

for(i in 1:N){
  data <- generate_samples_for_regression_two_vars(
    N,
    true_betas,
    X2.mean,
    X2.sd,
    eps.mean,
    eps.sd
  )
  
  lm <- lm(Y ~ I(X^poly_powers[i]), data = data)
  MSE.container[i] <- mean(lm$residuals^2)
  Ave.container[i] <- mean((lm$fitted.values - data$Y)^2)
}

plot(MSE.container)
plot(Ave.container)

generate_samples_for_regression_two_vars <- function(N, betas, mean_X, sigma_X, mean_eps, sigma_eps){
  X2 <- rnorm(n=N, mean=mean_X, sd=sigma_X)
  X <- cbind(rep(1,N), X2)
  eps <- rnorm(n=N, mean=mean_eps, sd=sigma_eps)
  Y <- X %*% betas + eps
  
  data <- data.frame("Y" = Y, "X1"=X[, 1], "X2" = X[, 2])
  return(data)
}

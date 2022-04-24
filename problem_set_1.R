set.seed(666)
# Parameters
N <- 1000
true_betas <- c(5, -0.5)

X2.mean <- 0
# This is correct
X2.sd <- sqrt(1.5)

# This is correct
eps.sd <- sqrt(10)
eps.mean <- 0

# a) Generate a training sample (xi, yi)^T
training_data <- generate_samples_for_regression_two_vars(N,
                                                          true_betas,
                                                          X2.mean,
                                                          X2.sd,
                                                          eps.mean,
                                                          eps.sd)


# c) Calculate beta_hat: for which data?
## Using lm
training_data.lm <- lm(Y ~ X2, data = training_data)
training_data.coef <- coef(training_data.lm)

# b) Generate test data
# Calculate test data using betas from training data
test_data <- generate_samples_for_regression_two_vars(
  N,
  betas = training_data.coef,
  X2.mean,
  X2.sd,
  eps.mean,
  eps.sd
)

test_data.lm <- lm(Y ~ X2, data = test_data)
test_data.coef <- coef(test_data.lm)

# c) calculate MSE and Ave for the 2 samples
# TODO: Not sure about this!
test_data.PE <- mean((training_data$Y - test_data.lm$fitted.values)^2)

training_data.MSE <-
  mean((training_data$Y - training_data.lm$fitted.values) ^ 2)

# d) Using the training sample from above, calculate the training MSE and the avg. prediction error when
# sequentially increasing the degree of the polynomial for X2 from zero (constant only) to four in the
# estimation equation
powers <- c(1,2,3,4)
powers_len <- length(powers)
training_data.lm_poly_models <- list(MSE = rep(0, powers_len))
test_data.lm_poly_models <- list(PE = rep(0, powers_len))

for(i in 1:powers_len){
  # Evaluate poly models and extract coefficients 
  lm_temp <- lm(Y ~ I(X2^powers[i]), data = training_data)
  lm_temp.coef <- coef(lm_temp)
  
  # Calculate MSE for training data: mean((Y - Y_fit)^2)
  training_data.lm_poly_models$MSE[i] <- mean((training_data$Y - lm_temp$fitted.values)^2)
  
  # Extract data from test data and construct Y vector using X's from test_data + betas from training data
  test_X <- cbind(test_data$X1, test_data$X2)
  test_Y <- test_X %*% lm_temp.coef
  # Calculate prediction error for test data
  test_data.lm_poly_models$PE[i] <- mean((training_data$Y - test_Y)^2)
}

# 2. Simulations
set.seed(100)
MSE.container <- rep(0, N)
Ave.container <- rep(0, N)
# WRONG! poly_powers <- rep(c(1, 2, 3, 4), N / 4)
poly_powers <- c(rep(1, N/4), rep(2, N/4), rep(3, N/4), rep(4, N/4))

for (i in 1:N) {
  data <- generate_samples_for_regression_two_vars(N,
                                                   true_betas,
                                                   X2.mean,
                                                   X2.sd,
                                                   eps.mean,
                                                   eps.sd)
  
  lm <- lm(Y ~ I(X2 ^ poly_powers[i]), data = data)
  train_betas <- coef(lm)
  
  # Generate test data drawing from distributions but using beta coefficients from training data
  test_data1 <- generate_samples_for_regression_two_vars(N,
                                                         betas = train_betas,
                                                         X2.mean,
                                                         X2.sd,
                                                         eps.mean,
                                                         eps.sd)
  
  # Calculate MSE for train data
  train_prediction <- data$X1 * train_betas[1] + data$X2 * train_betas[2]
  train_deviation <- data$Y - train_prediction
  MSE.container[i] <- mean(train_deviation^2)
  
  # Calculate PE for test data
  test_prediction <- test_data1$X1 * train_betas[1] + test_data1$X2 * train_betas[2]
  test_deviation <- test_data1$Y - test_prediction
  Ave.container[i] <- mean(test_deviation^2)
}

mean(MSE.container)
mean(Ave.container)

index <- 250 # partition of data: model with power of 1 is from index 1 till index 249,
# model with power of 2 is from 250 till 749 and so on till 1000
MSE.poly_models_means <- rep(0, powers_len)
Ave.poly_models_means <- rep(0, powers_len)

for(j in 1:powers_len){
    start_index <- (index * (j - 1)) + 1
    end_index <- (index * j)
    print(start_index)
    print(end_index)
    MSE.poly_models_means[j] <- mean(MSE.container[start_index:end_index])
    Ave.poly_models_means[j] <- mean(Ave.container[start_index:end_index])
}

ylim_offset = 0.1
pch = 19

plot(powers, MSE.poly_models_means,
  main = "MSE vs Avg error for different polys of X",
  ylab = "MSE & Avg error",
  xlab = "Poly powers",
  col='blue',
  ylim=c((min(Ave.poly_models_means) - ylim_offset), (max(MSE.poly_models_means) + ylim_offset)),
  pch=pch
)
lines(x=powers, y=MSE.poly_models_means, col="blue", pch=pch)
points(powers,
       Ave.poly_models_means,
       col="red", pch=pch
)
lines(x=powers, y=Ave.poly_models_means, col="red", pch=pch)
# Add a legend
legend(x=10.5,legend=c("MSE", "AVG pred error"),
       col=c("blue", "red"), lty=1:2, cex=0.8)
generate_samples_for_regression_two_vars <-
  function(N,
           betas,
           mean_X,
           sigma_X,
           mean_eps,
           sigma_eps) {
    X2 <- rnorm(n = N, mean = mean_X, sd = sigma_X)
    X <- cbind(rep(1, N), X2)
    eps <- rnorm(n = N, mean = mean_eps, sd = sigma_eps)
    Y <- X %*% betas + eps
    
    data <- data.frame("Y" = Y,
                       "X1" = X[, 1],
                       "X2" = X[, 2])
    return(data)
  }

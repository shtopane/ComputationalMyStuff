# Applied

# Constants ----
lambda_grid <- 10^seq(10, -2, length = 100)
# Plot constants
xlab_text <- "Number of variables"
plot_split <- c(2, 2)
# Exercise 8
n <- 100
beta_vector <- c(1, 2, 2, 1)
exercise8_max_variables <- 10
best_selection_plotted_variables <- c("rss", "adjr2", "cp", "bic")
# END Constants ----

# Functions ----
plot_regsubsets <- function(reg_object, variable_names) {
    par(mfrow = plot_split)
    # Plot variable 1
    # accessing elements using ["var"] returns a list and we can't plot that
    var1 <- get(variable_names[1], reg_object) # reg_object[variable_names[1]]
    plot(var1, xlab = xlab_text, ylab = variable_names[1], type = "l")
    var1_min <- which.min(var1)
    points(var1_min, var1[var1_min], col = "red", cex = 2, pch = 20)
    # Plot variable 2
    var2 <- get(variable_names[2], reg_object) # reg_object[variable_names[2]]
    plot(var2, xlab = xlab_text, ylab = variable_names[2], type = "l")
    var2_max <- which.max(var2)
    points(var2_max, var2[var2_max], col = "red", cex = 2, pch = 20)

    # Plot Cp and BIC(variable 3 and 4)
    var3 <- get(variable_names[3], reg_object) # reg_object[variable_names[3]]
    plot(var3, xlab = xlab_text, ylab = variable_names[3], type = "l")
    var3_min <- which.min(var3)
    points(var3_min, var3[var3_min], col = "red", cex = 2, pch = 20)

    var4 <- get(variable_names[4], reg_object) # reg_object[variable_names[4]]
    plot(var4, xlab = xlab_text, ylab = variable_names[4], type = "l")
    var4_min <- which.min(var4)
    points(var4_min, var4[var4_min], col = "red", cex = 2, pch = 20)

    return(c(var1_min, var2_max, var3_min, var4_min))
}

extract_significant_coefficients <- function(lm_object, sign_level = 0.05) {
    # data.frame(summary(lm_object)$coef[summary(lm_object)$coef[, 4] <= sign_level, 4])
    lm_object_coef <- summary(lm_object)$coef
    # Return all columns from the lm summary
    returned_columns <- c(1, 2, 3, 4)
    data.frame(lm_object_coef[lm_object_coef[, 4] <= sign_level, returned_columns])
}
# END Functions ----
# Exercise 8 ----

# a)
set.seed(1)
x <- rnorm(n)
error_term <- rnorm(n)
y <- beta_vector[1] + beta_vector[2] * x + beta_vector[3] * x^2 + beta_vector[4] * x^3 + error_term
# plot(x, y)
# c)
library("leaps")

regsubsets_data <- data.frame(
    y,
    x,
    "x2" = x^2,
    "x3" = x^3,
    "x4" = x^4,
    "x5" = x^5,
    "x6" = x^6,
    "x7" = x^7,
    "x8" = x^8,
    "x9" = x^9,
    "x10" = x^10
)
regfit_full <- regsubsets(y ~ ., regsubsets_data, nvmax = exercise8_max_variables)
regfit_summary <- summary(regfit_full) # 2 and 3 variable are most important
names(regfit_summary)
regfit_summary$rsq
# Plot where Adj. R2 is maximized: at 8
plot_regsubsets(regfit_summary, best_selection_plotted_variables)
# Both show that min is at 3 variables
# This plots show some frequencies, the darker it is the better(?)
plot(regfit_full, scale = "r2")
plot(regfit_full, scale = "adjr2")
plot(regfit_full, scale = "Cp")
plot(regfit_full, scale = "bic")
coef(regfit_full, 3)
# x: 1.9865742   x2: 2.2103249   x3: 1.0377563
# real: x: 2 x2: 2 x3: 1

# d) use forward and backward selection and compare to c)
regfit_forward <- regsubsets(y ~ ., regsubsets_data, nvmax = exercise8_max_variables, method = "forward")
summary(regfit_forward)
regfit_backward <- regsubsets(y ~ ., regsubsets_data, nvmax = exercise8_max_variables, method = "backward")
summary(regfit_backward)
# x3 different
# for forward 1 var is most important?
# for backward 3 var is most important?
# It seems that all models with vars > 2 are different
coef(regfit_full, 3)
coef(regfit_forward, 3)
coef(regfit_backward, 3)
# Backward seem to be the most inaccurate
# e) lasso
library("glmnet")
# generate x , y
model_x <- model.matrix(y ~ ., regsubsets_data)[, -1]
model_y <- y
lasso_mod <- glmnet(model_x, model_y, alpha = 1, lambda = lambda_grid)
plot(lasso_mod)
# use cross-validation for lambda
set.seed(2)
lasso_cv_out <- cv.glmnet(model_x, model_y, alpha = 1)
plot(lasso_cv_out)
lasso_best_lambda <- lasso_cv_out$lambda.min # 0.052
lasso_coefficients <- predict(lasso_mod, type = "coefficients", s = lasso_best_lambda)
lasso_coefficients[1:4] # for x:x4
beta_vector
# it seems that lasso performed worse in terms of coefficients than selection

# f) do the same with new model
beta_7 <- 1.5
y_new <- beta_vector[1] + beta_7 * x^7 + error_term
plot(x, y_new)
# best model selection
regfit2_full <- regsubsets(y_new ~ ., regsubsets_data, nvmax = exercise8_max_variables)
regfit2_summary <- summary(regfit2_full)

regfit2_extreme_values <- plot_regsubsets(regfit2_summary, best_selection_plotted_variables)
regfit2_extreme_values
# Model selection by statistic
coef(regfit2_full, regfit2_extreme_values[1]) # r2
coef(regfit2_full, regfit2_extreme_values[2]) # adj2
coef(regfit2_full, regfit2_extreme_values[3]) # cp
coef(regfit2_full, regfit2_extreme_values[4]) # bic

# Lasso
model2_x <- model.matrix(y_new ~ ., regsubsets_data)[, -1] # throw out intercept
model2_y <- y_new
lasso2_mod <- glmnet(model2_x, model2_y, alpha = 1, lambda = lambda_grid)
plot(lasso2_mod)
# use cross-validation for lambda
set.seed(3)
lasso2_cv_out <- cv.glmnet(model2_x, model2_y, alpha = 1)
plot(lasso2_cv_out)
lasso2_best_lambda <- lasso2_cv_out$lambda.min # 2.65
lasso2_coefficients <- predict(lasso2_mod, type = "coefficients", s = lasso2_best_lambda)
lasso2_coefficients
# all other variables are thrown away, only 7th is left. coef: 1.45
# END Exercise 8 ----

# Exercise 9 ----
library("ISLR2")
# a)
train_seq_len <- seq_len(nrow(College))
train <- sample(train_seq_len, nrow(College) / 2)
test <- (-train)
y_test <- College[test]

# b)
college_lm <- lm(Apps ~ ., data = College, subset = train)
summary(college_lm)
extract_significant_coefficients(college_lm) # 9 significant variables out of 18

# c) Ridge
library("glmnet")
college_x <- model.matrix(Apps ~ ., data = College)[, -1] # throw out intercept
college_y <- College$Apps
college_ridge <- glmnet(college_x[train, ], college_y[train], alpha = 0, lambda = lambda_grid)
# cross-validate lambda
set.seed(4)
college_ridge_cv_out <- cv.glmnet(college_x[train, ], college_y[train], alpha = 0)
plot(college_ridge_cv_out)
college_ridge_best_lambda <- college_ridge_cv_out$lambda.min # 323.5558
# Predict + test error
college_ridge_predict <- predict(college_ridge, s = college_ridge_best_lambda, newx = college_x[test, ])
mean((college_ridge_predict - college_y[test])^2) # MSE: 2527089

# d) Lasso
# cross-validate lambda
set.seed(5)
college_lasso_cv_out <- cv.glmnet(college_x[train, ], college_y[train], alpha = 1)
plot(college_lasso_cv_out)
college_lasso_best_lambda <- college_lasso_cv_out$lambda.min
college_lasso <- glmnet(college_x[train, ], college_y[train], alpha = 1, lambda = college_lasso_best_lambda)
# Report coefficients that are not zero
college_lasso_coef <- predict(college_lasso, type = "coefficients", s = college_lasso_best_lambda)
college_lasso_coef[college_lasso_coef != 0]
sum(college_lasso_coef != 0) # 14?

college_lasso_predict <- predict(college_lasso, s = college_lasso_best_lambda, newx = college_x[test, ])
mean((college_lasso_predict - college_y[test])^2) # 1710207

# e) PCR
require(pls)
set.seed(6)
college_pcr <- pcr(Apps ~ ., data = College[train, ], scale = TRUE, validation = "CV")
summary(college_pcr)
validationplot(college_pcr, val.type = "MSEP")
# Lowest error at 16 components
college_pcr_predict <- predict(college_pcr, college_x[test, ], ncomp = 16)
mean((college_pcr_predict - college_y[test])^2) # MSE: 1746647
# f)
set.seed(7)
college_pls <- plsr(Apps ~ ., data = College[train, ], scale = TRUE, validation = "CV")
summary(college_pls) # M = 9
college_pls_predict <- predict(college_pls, college_x[test, ], ncomp = 9)
mean((college_pls_predict - college_y[test])^2) # MSE: 1704102
# they don't seem to do much more than linear regression
# END Exercise 9 ----
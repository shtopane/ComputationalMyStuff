v1 <- seq(1, 30, le = 10)
length(v1)

## Exercises
# Sequences of Numbers, Vectors----
# Generate x = 3 6 8
x <- c(3, 6, 8)

# Calculate <U+0001D465>/2, <U+0001D465>2, <U+0001D465><U+23AF><U+23AF>v.
x.half <- x / 2
x.square <- x^2
x.sqrt <- sqrt(x)

# Display the second element of x
x[2]

# Create a vector with the first and third element of x
x.sampled <- c(x[1], x[3])

# Easier!
x[-2] # removes the second element and leaves the first one
# end the third one

# Remove the third element from <U+0001D465>.
x[1:length(x) - 1]

# Generate y = 2 5 1
y <- c(2, 5, 1)
x - y
x * y
t(x) %*% y
x %*% t(y)

len <- length(x)

# FILTERING: My version!
for (i in 1:len) {
  if (y[i] > 1.5) {
    print(x[i])
  }


  if (x[i] == 6) {
    print("display elements of y for whic the el in x == 6")
    print(y[i])
  }
}

# FILTERING: Easier!
x[y > 1.5]
y[x == 6]

# Generate a vector of integers from 4-10.
int_4_to_10 <- seq(4, 10)
# Using seq and rep,
# (a) generate a vector ranging from 2-3 in 0.1 steps,
# (b) generate a vector that repeats the elements of <U+0001D465> 4 times,
# i.e. the first four elements of the new vector are four times the first element of <U+0001D465>, etc.
vec_2_to_3_little_step <- seq(2, 3, by = 0.1)
x.x_times_4 <- rep(x, each = 4)

# Matrices ----
A <- rbind(c(1, 2, 5), c(4, 7, 3))
dim(A)
B <- rbind(c(1, 4, 2), c(7, 5, 3))
C <- rbind(c(1, -1), c(-1, 3))
diag(C)
solve(t(C) %*% C)

# Logical Operators ----
z <- c(1:20)
#  Which elements of x are smaller than 15.
z.el_smaller_than_15 <- z < 15
# Which elements of x are smaller than 10 and larger than 5.
z.el_smaller_10_larger_5 <- z < 10 & z > 5

# x are smaller than 10 and larger than 5 subset of x smaller than 15, so just
# assign that one
k <- z.el_smaller_than_15

# Sums and Frequencies and using functions ----
d <- c(77, 93, 92, 68, 88, 75, 100)
mean(d)
sort(d)
sort(d, decreasing = TRUE)
min(d)
max(d)
# There is a func that returns the indices of all occurrences
# of min and max values

# Statistical Distributions ----
# Note: build a test for this - check it 2 different ways
# if you'll get the same value! pbinom can be implemented
# with dbinom and more options
dbinom(8, 10, 0.8)
dbinom(4, 12, 0.2) # correct?
pbinom(4, 12, 0.2) # also correct?

# normal with - mean 72 and sd 15.2
set.seed(123)
exam <- rnorm(1000, mean = 72, sd = 15.2)
hist(exam)

dnorm(84, mean = 72, sd = 15.2) # not correct
pnorm(84, mean = 72, sd = 15.2, lower.tail = FALSE) # correct!

# Exercise 3 - REDO IT. Actually two cases:
# 1) Uniform distro
# 2) Normal distro
a <- runif(1000, min = 0, max = 1)
mean(a)
sd(a)

# Limit the normal distribution: draw from "truncated normal"?
# This is the case where we have a mean shift in the disto

# Loops and functions ----
# 1
x <- 1
y <- 40

# Empty vector - resizes?

for (i in 1:10) {
  x <- x + 3
  y <- y - 2
}

# 2
a <- 15:10
b <- 20:15
i <- 1
x <- rep(0, length(a))

while (i <= length(a)) {
  x[i] <- a[i] * b[length(a) - i + 1]
  i <- i + 1
}

# 3
myFunc <- function(name) {
  first_el_name <- substr(name, 1, 1)
  return(list(first = first_el_name, test = "My first Function"))
}

myFunc.result <- myFunc("asdasdasdasds")

# 4
set.seed(666)
vec <- rnorm(30, mean = 0, sd = 1)
vec.2 <- vec[1:10]
vec.sqared_first_10 <- vec.2^2
l <- 0.8
vec.22 <- rep(NA, length(vec.2))

for (i in 1:length(vec.2)) {
  if (abs(vec.2[i]) > l) {
    vec.22[i] <- vec.2[i]^2
  }
}

testFunc <- function(u, k, l) {
  u.filtered <- u[1:k]
  u.copy <- u.filtered

  u.squared <- u.filtered^2
  condition_met_count <- 0

  for (i in 1:k) {
    if (abs(u[i]) > l) {
      u.copy[i] <- u[i]^2
      condition_met_count <- condition_met_count + 1
    }
  }

  return(list(squared = u.squared, cond_met = condition_met_count))
}

testFunc(vec, 10, 0.8)
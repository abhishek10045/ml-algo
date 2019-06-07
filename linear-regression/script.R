hypothesis <- function(theta, x) {
    x %*% theta
}

cost_function <- function(theta, x, y) {
    sum((hypothesis(theta, x) - y) ^ 2) / (2 * length(y))
}


partial_derivative_cost_function <- function(theta, x, y) {
    (t(x) %*% (hypothesis(theta, x) - y)) / length(y)
}

batch_gradient_descent <- function(theta, alpha, itr, x, y) {
    cost <- c()
    for (i in 1:itr) {
        cost <- c(cost, cost_function(theta, x, y))
        theta <- theta - alpha * partial_derivative_cost_function(theta, x, y)
    }
    list(theta, cost)
}

normal_equation <- function(x, y) {
    solve(t(x) %*% x) %*% t(x) %*% y
}

x <- matrix(c(rep(1, 100), 1:100), ncol = 2)
y <- 1:100 * 5 + runif(100, -25, 25)

# gradient descent method
theta <- c(1, 1)
alpha <- 0.0005
itr  <- 50
l <- batch_gradient_descent(theta, alpha, itr, x, y)
theta <- l[[1]]
cost <- l[[2]]
n_itr <- 1:itr
plot(cost ~ n_itr)
theta
cost_function(theta, x, y)

# normal equtaion method
theta <- normal_equation(x, y)
theta

x_1 <- 1:100
plot(y ~ x_1)

x <- matrix(c(rep(1, 3), 1:3), ncol = 2)
y <- c(4, 7, 10)

theta <- c(1, 1)
alpha <- 0.3
itr  <- 500
l <- batch_gradient_descent(theta, alpha, itr, x, y)
theta <- l[[1]]
cost <- l[[2]]
n_itr <- 1:itr
plot(cost ~ n_itr)
theta

solve(t(x) %*% x) %*% t(x) %*% y
normal_equation(x, y)

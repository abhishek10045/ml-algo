hypothesis <- function(theta, x) {
    x %*% theta
}

cost_function <- function(theta, lambda, x, y) {
    (sum((hypothesis(theta, x) - y) ^ 2) + (lambda * sum(theta[-1] ^ 2))) / (2 * length(y))
}

partial_derivative_cost_function <- function(theta, lambda, x, y) {
    ((t(x) %*% (hypothesis(theta, x) - y)) + (lambda * c(0, theta[-1]))) / length(y)
}

batch_gradient_descent <- function(theta, alpha, lambda, itr, x, y) {
    cost <- c()
    for (i in 1:itr) {
        cost <- c(cost, cost_function(theta, lambda, x, y))
        theta <- theta - alpha * partial_derivative_cost_function(theta, lambda, x, y)
    }
    list(theta, cost)
}

x <- matrix(c(rep(1, 3), 1:3), ncol = 2)
y <- c(4, 7, 10)
lambda <- 0.1


theta <- c(1, 1)
alpha <- 0.3
itr  <- 500
l <- batch_gradient_descent(theta, alpha, lambda, itr, x, y)
theta <- l[[1]]
cost <- l[[2]]
n_itr <- 1:itr
plot(cost ~ n_itr)
theta
cost_function(theta, x, y)

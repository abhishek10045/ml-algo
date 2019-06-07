hypothesis <- function(x, theta) {
    x %*% theta
}

cost_function <- function(x, y, theta, lambda) {
    (sum((hypothesis(x, theta) - y) ^ 2) + (lambda * sum(theta[-1] ^ 2))) / (2 * length(y))
}

partial_derivative_cost_function <- function(x, y, theta, lambda) {
    ((t(x) %*% (hypothesis(x, theta) - y)) + (lambda * c(1, theta[-1]))) / length(y)
}

batch_gradient_descent <- function(x, y, theta = rep(1, ncol(x)), alpha = 0.01, itr = 100,  lambda = 0) {
    cost <- vector(mode = "numeric", length = itr)
    for (i in 1:itr) {
        cost[i] <- cost_function(x, y, theta, lambda)
        theta <- theta - alpha * partial_derivative_cost_function(x, y, theta, lambda)
    }
    list(theta, cost)
}

hypothesis <- function(theta, x) {
    sigmoid(x %*% theta)
}

cost_function <- function(theta, lambda, x, y) {
    h_theta <- hypothesis(theta, x)
    (-sum(y * log(h_theta) + (1 - y) * log(1 - h_theta)) + (lambda * sum(theta[-1] ^ 2)) / 2) / length(y)
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

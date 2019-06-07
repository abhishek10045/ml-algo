library(sigmoid)
hypothesis <- function(theta, x) {
    sigmoid(x %*% theta)
}

cost_function <- function(theta, x, y) {
    h_theta <- hypothesis(theta, x)
    -sum(y * log(h_theta) + (1 - y) * log(1 - h_theta)) / length(y)
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

x_1 <- c(1, 2, 3, 8, 10)
x_2 <- c(4, 2, 1, 12, 11)

x <- matrix(data = c(rep(1, 5), x_1, x_2), ncol = 3)
y <- c(1, 1, 1, 0, 0)

theta <- c(-6, 1, 1)
alpha <- 0.5
itr  <- 500
l <- batch_gradient_descent(theta, alpha, itr, x, y)
theta <- l[[1]]
cost <- l[[2]]
n_itr <- 1:itr
plot(cost ~ n_itr)
theta
cost_function(theta, x, y)


hypothesis(theta, x)
cost_function(theta, x, y)

plot(x_2, x_1, col = ifelse(y == 1, 'red', 'blue'))
abline(a = -theta[1] / theta[2], b = -theta[3] / theta[2], col = 'green')


theta <- rep(1, 3)
x <- 1:3

hypothesis(theta, x) == 1 / (1 + exp(-6))



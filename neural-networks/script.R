library(sigmoid)
x_0 <- rep(1, 2)
x_1 <- c(2, 8)
x_2 <- c(1, 9)
x_3 <- c(3, 9)
x <- matrix(c(x_0, x_1, x_2, x_3), ncol = 4)
y <- c(1, 0)
m <- 5
s <- c(3, 3, 1)
L <- 3


theta_1 <- matrix(c(2, 1, 3, -1, -3, -2, 0, 1, 1, 2, -2, -1), ncol = 4, byrow = TRUE)
theta_2 <- matrix(c(-3, 2, 0, 1), ncol = 4)

theta <- list(theta_1, theta_2)
# 
# a_1 <- x
# a_1
# 
# z_2 <- a_1 %*% t(theta[[1]]) 
# z_2
# 
# a_2 <- sigmoid(z_2)
# a_2 <- cbind(rep(1, nrow(a_2)), a_2)
# a_2
# 
# z_3 <- a_2 %*% t(theta[[2]])
# z_3
# 
# a_3 <- sigmoid(z_3)
# a_3
# theta <- init_theta(c(3, 3, 1), 3)
# hypothesis(x, theta, 3)

###########################################################################

# working coorectly

init_theta <- function(s, L, epsilon = 10) {
    theta <- vector(mode = "list", L - 1)
    for (i in 1:(L - 1)) {
        theta[[i]] <- matrix(runif(s[i + 1] * (s[i] + 1), -epsilon, epsilon), nrow = s[i + 1])
    }
    theta
}

hypothesis <- function(x, theta, L) {
    a <- vector(mode = "list", length = L)
    a[[1]] <- x
    for (i in 2:(L-1)) {
        z <- a[[i - 1]] %*% t(theta[[i - 1]])
        a[[i]] <- sigmoid(z)
        a[[i]] <- cbind(rep(1, nrow(a[[i]])), a[[i]])
    }
    z <- a[[L - 1]] %*% t(theta[[L - 1]])
    a[[L]] <- sigmoid(z)
    a
}

cost_function <- function(x, y, theta, L, lambda) {
    h_theta <- hypothesis(x, theta, L)[[L]]
    (-sum(y * log(h_theta) + (1 - y) * log(1 - h_theta)) + (lambda * sum(sapply(theta, function(x) { sum(x[, -1] ^ 2) }))) / 2) / length(y)
}

partial_derivative_cost_function <- function(x, y, theta, L, lamdba) {
    a <- hypothesis(x, theta, L)
    error <- vector(mode = "list", L - 1)
    error[[L - 1]] <- a[[L]] - y
    for (i in (L - 2):1) {
        error[[i]] <- t(t(theta[[i + 1]]) %*% t(error[[i + 1]])) * ((a[[i + 1]] * (1 - a[[i + 1]])))
        error[[i]] <- matrix(error[[i]][, -1], nrow = nrow(error[[i]]))
    }
    partial_derivative <- vector(mode = "list", L - 1)
    for (i in 1:(L - 1)) {
        theta[[i]][, 1] <- 0
        partial_derivative[[i]] <- (((t(error[[i]]) %*% a[[i]]) + (lamdba * theta[[i]])) / length(y))
    }
    partial_derivative
}

gradient_check <- function(x, y, L, e = 0.000001,  lambda = 0) {
    derv <- vector(mode = "list", length = L - 1)
    for (l in 1:(L - 1)) {
        derv[[l]] <- matrix(nrow = nrow(theta[[l]]), ncol = ncol(theta[[l]]))
        for (i in 1:nrow(theta[[l]])) {
            for (j in 1:ncol(theta[[l]])) {
                t_plus <- theta
                t_plus[[l]][i, j] <- t_plus[[l]][i, j] + e
                t_minus <- theta
                t_minus[[l]][i, j] <- t_minus[[l]][i, j] - e
                derv[[l]][i, j] <- (cost_function(x, y, t_plus, L, lambda) - cost_function(x, y, t_minus, L, lambda)) / (2 * e)
            }
        }
    }
    pd <- partial_derivative_cost_function(x, y, theta, L, lambda)
    for (l in 1:(L - 1)) {
        derv[[l]] <- round(derv[[l]] - pd[[l]], 3)
    }
    derv
}

batch_gradient_descent <- function(x, y, theta, L, alpha = 0.01, itr = 100, lambda = 0) {
    cost <- vector(mode = "numeric", length = itr)
    for (i in 1:itr) {
        cost[i] <- cost_function(x, y, theta, L, lambda)
        pd <- partial_derivative_cost_function(x, y, theta, L, lambda)
        for (l in 1:(L - 1)) {
            theta[[l]] <- theta[[l]] - alpha * pd[[l]]
        }
    }
    list(theta, cost)
}

###########################################################################


x_1 <- c(1, 2, 3, 8, 10)
x_2 <- c(4, 2, 1, 12, 11)

x <- matrix(data = c(rep(1, 5), x_1, x_2), ncol = 3)
y_1 <- c(1, 1, 0, 0, 0)
y_2 <- c(0, 0, 0, 1, 1)
y_3 <- c(0, 0, 1, 0, 0)

y <- matrix(data = c(y_1, y_2, y_3), ncol = 3)
theta <- init_theta(c(2, 2, 3), 3)

hypothesis(x, theta, 3)
cost_function(x, y, theta, 3, 0)
itr <- 500

i <- 1:itr


cost_function(x, y, theta, 3, 0.0)
b <- batch_gradient_descent(x, y_1, theta, 3, alpha = 0.1, itr = itr)
t <- b[[1]]

t
c <- b[[2]]
c
plot(c ~ i)
    
cost_function(x, y, theta, 3, 0.1)

gradient_check(x, y, 3, 0.1)

partial_derivative_cost_function(x, y, theta, 3, 0.1)

pd <- partial_derivative_cost_function(x, y, theta, 3, 0.1)


pd
derv
theta
matrix()



x <- matrix(data = c(x_1, x_2), ncol = 2)
z <- x[, -1]
z
class(z)

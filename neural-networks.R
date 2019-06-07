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

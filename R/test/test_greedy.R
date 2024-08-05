source("R/tree_datastructure.R")
source("R/greedy.R")
library(tidyverse)

n <- 200
sigma <- 0.2
X <- runif(n, 0, 1)
epsilon <- rnorm(n, 0, sigma)
Y <- sin(2*pi*X) + epsilon

plot(X, Y)

testtree <- greedy(
    matrix(X, ncol=1),
    matrix(Y, ncol=1)
)

X_hat <- seq(0, 1, 0.05)
Y_hat <- sapply(X_hat, \(x) testtree$decide(c(x)))
plot(X_hat, Y_hat)

# iris <- as_tibble(iris)
# iris
# 
# greedy(
#     as.matrix(select(iris, -c(Sepal.Length, Species))),
#     as.matrix(select(iris, Sepal.Length))
# )


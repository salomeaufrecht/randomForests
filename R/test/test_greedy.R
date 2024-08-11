source("R/tree_datastructure.R")
source("R/greedy.R")
library(tidyverse)

print("One dimensional example:")

n <- 150
sigma <- 0.1
X <- runif(n, 0, 1)
epsilon <- rnorm(n, 0, sigma)
Y <- sin(2*pi*X) + epsilon

testtree <- greedy(
    matrix(X, ncol=1),
    matrix(Y, ncol=1)
)

testtree$plot_data()


iristest <- as.matrix(iris[, 1:4])

training_data_x <- iristest[, 1:3, drop=FALSE]
training_data_y <- iristest[, 4, drop=FALSE]

testtree <- greedy(
    iristest[, 1:3, drop=FALSE],
    iristest[, 4, drop=FALSE]
)

print("testing tree: ")
random_rows <- sample(1:nrow(iristest), 100)
testmatrix <- matrix(
    c(iristest[random_rows, ],
    sapply(random_rows, \(x) testtree$decide(iristest[x, 1:3]))
    ),
    ncol = 5
)
colnames(testmatrix) <- c(colnames(iristest), "Predicted Petal Width")
print(testmatrix)

average_error <- sum(abs(testmatrix[, 4] - testmatrix[, 5]))/nrow(testmatrix)
cat("Average error: ", average_error)

random_row <- sample(1:nrow(iristest), 1)
print(iristest[random_row, ])
print(testtree$decide(iristest[random_row, 1:3]))

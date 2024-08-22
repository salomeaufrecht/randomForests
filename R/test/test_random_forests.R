library(tidyverse)
library(randomForests)

iristest <- as.matrix(iris[, 1:4])

random_rows <- sample(1:nrow(iristest), 70)

greedy_tree <- greedy(
    iristest[random_rows, 1:3, drop=FALSE],
    iristest[random_rows, 4, drop=FALSE],
)

forest_function <- random_forest(iristest[random_rows, 1:3, drop=FALSE],
                               iristest[random_rows, 4, drop=FALSE])

print("testing tree: ")
for (i in 1:10) {
    random_rows <- sample(1:nrow(iristest), 100)
    greedy_matrix <- matrix(
        c(iristest[random_rows, ],
        sapply(random_rows, \(x) greedy_tree$decide(iristest[x, 1:3]))
        ),
        ncol = 5
    )
    
    forest_matrix <- matrix(
        c(iristest[random_rows, ],
        sapply(random_rows, \(x) forest_function(iristest[x, 1:3]))
        ),
        ncol = 5
    )
    
    greedy_error <- sum(abs(greedy_matrix[, 4] - greedy_matrix[, 5]))/nrow(greedy_matrix)
    forest_error <- sum(abs(forest_matrix[, 4] - forest_matrix[, 5]))/nrow(forest_matrix)
    cat("Average error with greedy: ", greedy_error, "\n")
    cat("Average error with random Forest: ", forest_error, "\n")
}


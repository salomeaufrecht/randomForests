#' this is an example documentation
#' 
#' there are more tags...
#' compile with devtools::document()
#' @export greedy
#' @param x whatever
#' 
greedy <- function(training_data_x, trainings_data_y) {
    tree <- Tree$new(length=1)
    leafs <- c(1)
    k <- 1
    A <- training_data_x
    while(length(leafs) > 0) {
        
    }
}

decide <- function(A, j, s) {
    return(A[A[, j] < s])
}

minimize <- function(A, d, trainings_data_x, trainings_data_y) {
    X <- trainings_data_x
    n <- length(X)
    Y <- trainings_data_y
    
    for (j in 1:d) {
        s <- X[-n, j] + (X[-1, j]-X[-n, j])/2
        left_values <- which(A[,j] < s)
        A1 <- A[left_values, ]
        A2 <- A[!left_values, ]
        # use argmin for s
        
        # All values between 
        # for (s in X[-n] + (X[-1]-X[-n])/2) {
        #     A1 <- decide(A, j, s)
        #     sum <- 
        # }
    }
}
#' this is an example documentation
#' 
#' there are more tags...
#' compile with devtools::document()
#' @export greedy
#' @param x whatever
#' 
greedy <- function(training_data_x, training_data_y) {
    stopifnot(nrow(training_data_x) == nrow(training_data_y))
    
    tree <- Tree$new(training_data_x, training_data_y)
    tree$data[1, "y"] <- mean(training_data_y)
    new_tree(tree, 1, 1, 1:nrow(training_data_x))
    return(tree)
}

new_tree <- function(tree, index, k, A_parent_indices) {
    A_parent_indices <- matrix(A_parent_indices, ncol=1)
    if(nrow(A_parent_indices) < 5 || k > 10) return()
    min_values <- minimize_risk(tree, index, A_parent_indices) 
    tree$data[index, 2:3] <- c(min_values$j, min_values$s)
    child_indices <- tree$add_children(index, y1=min_values$y1, y2=min_values$y2)
    left_values <- min_values$left_values
    A1 <- left_values
    A2 <- setdiff(A_parent_indices, left_values)
    
    new_tree(tree, child_indices[1], k+1, A1)
    new_tree(tree, child_indices[2], k+1, A2)
}

minimize_risk <- function(tree, index, A_indices) {
    d <- tree$d
    
    min_risk <- .Machine$integer.max
    min_j <- -1
    min_s <- -1
    min_left_values <- NA
    
    n <- nrow(A_indices)
    Y <- tree$training_data_y
    
    #print(A_indices)
    #A_X <- tree$training_data_x[A_indices,,drop=FALSE]
    
    for (j in 1:d) {
        # positions of A_indices in order matrix
        order_A_indices <- tree$inverse_order_matrix[A_indices, j]
        sorted_A_indices <- A_indices[order(order_A_indices)]
        A_X <- tree$training_data_x[sorted_A_indices, j]
        split_values <- A_X[-n] + (A_X[-1]-A_X[-n])/2
        for (i in seq_along(split_values)) {
            # Because the values are sorted we can just use 1:i for all indices that are smaller
            left_values <- 1:i
            s <- split_values[i]
            c1_hat <- 1/length(left_values) * sum(Y[sorted_A_indices[left_values]])
            c2_hat <- 1/(n - length(left_values)) * sum(Y[sorted_A_indices[-left_values]])
            risk <- sum( (Y[sorted_A_indices[left_values]]  - c1_hat)**2 ) +
                    sum( (Y[sorted_A_indices[-left_values]] - c2_hat)**2 )
            if(risk < min_risk) {
                min_risk <- risk
                min_j <- j
                min_s <- s
                min_left_values <- left_values
            }
        }
    }
    
    y1 <- 1/length(min_left_values) * sum(Y[sorted_A_indices[min_left_values]])
    y2 <- 1/(n-length(min_left_values)) * sum(Y[sorted_A_indices[-min_left_values]])
    
    # position in training_data matrix
    min_left_values <- sorted_A_indices[min_left_values]
    
    return(list(j=min_j, s=min_s, y1=y1, y2=y2, left_values=min_left_values))
}

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
    new_tree(tree, 1, 1, 1:nrow(training_data_x))
    return(tree)
}

new_tree <- function(tree, index, k, A_parent_indices) {
    A_parent_indices <- matrix(A_parent_indices, ncol=1)
    if(nrow(A_parent_indices) < 5 || k >=  7) return()
    min_values <- minimize_risk(tree, index, A_parent_indices) 
    tree[index] <- c(min_values$j, min_values$s)
    child_indices <- tree$add_children(index, y1=min_values$y1, y2=min_values$y2)
    left_values <- min_values$left_values
    A1 <- A_parent_indices[left_values]
    A2 <- A_parent_indices[!left_values]
    
    new_tree(tree, child_indices[1], k+1, A1)
    new_tree(tree, child_indices[2], k+1, A2)
}

minimize_risk <- function(tree, index, A_indices) {
    d <- tree$d
    
    min_risk <- .Machine$integer.max
    min_j <- -1
    min_s <- -1
    min_left_values <- NA
    
    A_X <- tree$training_data_x[A_indices,,drop=FALSE]
    n <- nrow(A_X)
    Y <- tree$training_data_y
    
    for (j in 1:d) {
        # All sensible possible split values 
        split_values <- A_X[-n, j] + (A_X[-1, j]-A_X[-n, j])/2
        for (s in split_values) {
            left_values <- A_X[,j] < s
            if(all(!left_values)) next
            if(all(left_values)) next
            c1_hat <- 1/n * sum(Y[A_indices[left_values]])
            c2_hat <- 1/n * sum(Y[A_indices[!left_values]])
            risk <- sum( (Y[A_indices[left_values]]  - c1_hat)**2 ) +
                    sum( (Y[A_indices[!left_values]] - c2_hat)**2 )
            if(risk < min_risk) {
                min_risk <- risk
                min_j <- j
                min_s <- s
                min_left_values <- left_values
            }
        }
    }
    
    
    y1 <- 1/n * sum(Y[A_indices[min_left_values]])
    y2 <- 1/n * sum(Y[A_indices[!min_left_values]])
    
    return(list(j=min_j, s=min_s, y1=y1, y2=y2, left_values=min_left_values))
}
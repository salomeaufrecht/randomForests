#' Greedy algorithm
#' 
#' This function generates a decision tree with a greedy algorithm
#' @export
#' @param training_data_x Matrix of inputs
#' @param training_data_x Matrix of outputs (of given inputs)
#' @param split_count Max amount of split depth (0 for no limit)
#' @param data_in_leaves Amount data that should be in each leaf
#' @param classification_tree Should the tree be a classification tree?
#' @param random_subset Modification used for random_forest where only a random subset of dimensions is considered
#' 
greedy <- function(training_data_x, training_data_y, split_count=10, data_in_leaves=5, classification_tree=FALSE, random_subset=FALSE) {
    stopifnot(nrow(training_data_x) == nrow(training_data_y))
    tree <- Tree$new(training_data_x, training_data_y, classification_tree)
    tree$data[1, "y"] <- mean(training_data_y)
    if(classification_tree) tree$data[1, "y"] <- as.numeric(names(which.max(table(Y))))
    new_tree(tree=tree,
             index=1,
             k=1,
             max_k=ifelse(split_count<=0, Inf, split_count),
             min_data=data_in_leaves,
             A_parent_indices =  1:nrow(training_data_x), 
             m = ifelse(random_subset, sample(1:tree$d, 1), tree$d)
    )
    return(tree)
}

#' Generates new tree with two more children
#' @export
#' @param tree Original tree
#' @param index Index of leaf
#' @param k Depth of Algorithm
#' @param max_k Max amount of split depth (0 for no limit)
#' @param min_data Amount data that should be in each leaf
#' @param A_parent_indices All the data classified in leaf node
#' @param m 
new_tree <- function(tree, index, k, max_k, min_data, A_parent_indices, m) {
    A_parent_indices <- matrix(A_parent_indices, ncol=1)
    if(nrow(A_parent_indices) < min_data || k > max_k) return()
    min_values <- minimize_risk(tree, index, A_parent_indices, m) 
    tree$data[index, 2:3] <- c(min_values$j, min_values$s)
    child_indices <- tree$add_children(index, y1=min_values$y1, y2=min_values$y2)
    left_values <- min_values$left_values
    A1 <- left_values
    A2 <- setdiff(A_parent_indices, left_values)
    
    new_tree(tree, child_indices[1], k=k+1, max_k = max_k, min_data=min_data, A_parent_indices=A1, m=m)
    new_tree(tree, child_indices[2], k=k+1, max_k = max_k, min_data=min_data, A_parent_indices=A2, m=m)
    
}

#' Minimizes risk at a given index
minimize_risk <- function(tree, index, A_indices, m) {
    
    min_risk <- .Machine$integer.max
    min_j <- -1
    min_s <- -1
    min_c1_hat <- -1
    min_c2_hat <- -2
    min_left_values <- NA
    
    n <- nrow(A_indices)
    Y <- tree$training_data_y
    
    d <- ifelse(m==tree$d, tree$d, sample(1:tree$d, m))
    
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
            if (tree$type == 'regression') {
                c1_hat <- 1/length(left_values) * sum(Y[sorted_A_indices[left_values]])
                c2_hat <- 1/(n - length(left_values)) * sum(Y[sorted_A_indices[-left_values]])
                risk <- sum( (Y[sorted_A_indices[left_values]]  - c1_hat)**2 ) +
                        sum( (Y[sorted_A_indices[-left_values]] - c2_hat)**2 )
            }
            else {
                p1 <- sapply(1:tree$k, \(x) sum(Y[sorted_A_indices[left_values]] == x) / length(left_values) )
                p2 <- sapply(1:tree$k, \(x) sum(Y[sorted_A_indices[-left_values]] == x) / (n - length(left_values)) )
                c1_hat <- which.max(p1) 
                c2_hat <- which.max(p2)
                risk <- (length(left_values) * (1-p1[c1_hat])) + ((n-length(left_values)) * (1-p2[c2_hat]))
            }
            
            if(risk < min_risk) {
                min_risk <- risk
                min_j <- j
                min_s <- s
                min_c1_hat <- c1_hat
                min_c2_hat <- c2_hat
                # position in training_data matrix
                min_left_values <- sorted_A_indices[left_values]
            }
        }
    }
    
    y1 <- min_c1_hat
    y2 <- min_c2_hat
    
    return(list(j=min_j, s=min_s, y1=y1, y2=y2, left_values=min_left_values))
}

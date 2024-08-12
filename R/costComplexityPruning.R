
#' Cost-Complexity Pruning
#'
#' This function performs cost-complexity pruning on a decision tree using cross-validation to select the optimal complexity parameter (\eqn{\lambda}).
#'
#' @export
#' @param t The original decision tree to be pruned.
#' @param m Number of partitions to create for cross-validation. Default is 10.
#' @param lambda_min Minimum value for the complexity parameter \eqn{\lambda}. Default is 1.
#' @param lambda_max Maximum value for the complexity parameter \eqn{\lambda}. Default is 100.
#' @param lambda_step Step size for iterating over \eqn{\lambda} values. Default is 1.
#' @param plot Logical flag to indicate whether to plot the cross-validation error against \eqn{\lambda}. Default is FALSE.
#' @param print_progress Logical flag to indicate whether to print progress messages during execution. Default is FALSE.
#' @return A pruned decision tree based on the optimal \eqn{\lambda} value.
#' @examples
#' # Assuming `tree` is an initialized decision tree object
#' pruned_tree <- costComplexityPruning(tree, lambda_min=1, lambda_max=50, plot=TRUE)
costComplexityPruning <- function(t, m=10, lambda_min=1, lambda_max=100, lambda_step=1, plot=FALSE,  print_progress=FALSE){
    
    stopifnot("m need to be greater 1"= m>=2)
    Im <- make_partition(t$training_data_x, m)
    sequences <- list(0)
    
    if(print_progress)cat("creating pruning sequence ")
    for (i in 1:m){
        if(print_progress)cat(i, ",")
        sequences[[i]] <- (get_pruning_sequence(
                                greedy(matrix(t$training_data_x[Im!=i]) , 
                                       matrix(t$training_data_y[Im!=i]))))
    }
    
    if(print_progress) cat("\nall pruning sequences created\n")
 
    lambdas <- seq(lambda_min, lambda_max, lambda_step)
    CVs <- NULL
    if(print_progress) cat("checking lambda: ")
    
    for (i in seq_along(lambdas)){
        if(print_progress) cat(lambdas[i], ",")
        CVs[i] <- CV(lambdas[i], sequences, Im,t$training_data_x, t$training_data_y)
    }
    if(print_progress) cat("\n")
    if(plot) plot(lambdas, CVs)
    lambda <- lambdas[which.min(CVs)]
    if(print_progress) cat("chosen lambda: ", lambda, "\n")
    if(print_progress) cat("calculating resulting tree \n")

    return(choose_tp_lambda(lambda, get_pruning_sequence(t)))
}

#' Cross-Validation Error Calculation
#'
#' Calculates the cross-validation error for a given complexity parameter \eqn{\lambda} 
#' by pruning a decision trees with different training data and evaluating its performance on validation sets.
#'
#' @export
#' @param lambda The complexity parameter for pruning.
#' @param sequences A list of pruning sequences generated from different training data.
#' @param Im The partition indices for cross-validation.
#' @param training_data_x The training data x.
#' @param training_data_y The training data y.
#' @return The cross-validation error for the given \eqn{\lambda}.
CV <- function(lambda, sequences, Im, training_data_x, training_data_y){
    sum <- 0
    for (m in seq_along(sequences)){
        t <- choose_tp_lambda(lambda, sequences[[m]])
        inner_sum <- 0
        for (i in which(Im == m)){
            inner_sum <- inner_sum + L(t, training_data_y[i], t$f(training_data_x[i]))
        }
        sum <- sum + inner_sum
    }
    return((1/length(Im)) *sum)
}

#' Loss Function
#'
#' Computes the loss between the true label and the predicted label based on the type of tree (classification or regression).
#'
#' @export
#' @param t The decision tree object.
#' @param y The true label.
#' @param s The predicted label.
#' @return The computed loss value.
#' @examples
#' loss <- L(tree, y_true, y_pred)
L <- function(t, y, s){
    if (t$type == "classification"){
        return(ifelse(y!=s, 1, 0))
    }
    return((y-s)^2)
}

#' Create Cross-Validation Partitions
#'
#' Creates a partition of the training data into `m` parts for cross-validation purposes.
#'
#' @export
#' @param x The training data.
#' @param m The number of partitions to create. Default is 10.
#' @return A vector indicating the partition assignments for each observation.
make_partition <- function(x, m=10){
    n <- length(x)
    size <- round(n/m)
    part <- integer(n)
    for (i in 1:(m-1)){
        part[sample((1:n)[part==0], size = size, replace = FALSE)] <- i
    }
    part[part==0] <- m
    return(part)
}

#' Choose Optimal Subtree for Given \eqn{\lambda}
#'
#' Selects the subtree from the pruning sequence that minimizes the 
#' cost-complexity criterion for a given \eqn{\lambda}.
#'
#' @export
#' @param lambda The complexity parameter for pruning.
#' @param pruning_sequence A list of subtrees generated during the pruning process.
#' @return The subtree that minimizes the cost-complexity criterion.
choose_tp_lambda <- function(lambda, pruning_sequence){
    min_value <- Inf 
    tLambda <- NULL
    for(t in pruning_sequence){
        val <- t$calc_risk() + lambda * length(t$get_leaves())
        if(val<min_value){
            min_value <- val
            tLambda <- t$copy()
            }
    }
    return(tLambda)
}


#' Generate Pruning Sequence
#'
#' Generates a sequence of subtrees using weakest link pruning from the original tree.
#'
#' @export
#' @param t The original decision tree.
#' @return A list of subtrees representing the pruning sequence (t(0), ..., t(p)).
get_pruning_sequence <- function(t){
    possible_trees <- get_sub_trees(t)
    return(c(t, rec_pruning_sequence(t,  possible_trees)))
}


#' Recursive Pruning Sequence
#'
#' Should not be used by itself!
#' 
#' A helper function that recursively generates the pruning sequence for the given tree.
#'
#' @param t0 The current tree in the pruning process.
#' @param possible_trees A list of possible subtrees that can be generated from the current tree.
#' @return A list of subtrees in the pruning sequence.

rec_pruning_sequence <- function(t0, possible_trees){
    if(t0$is_leaf(1)) return()
    leaf_count_old <- length(t0$get_leaves())
    min_cost <- Inf
 
    for (t in possible_trees){
        leaf_count_new <- length(t$get_leaves())
        if(leaf_count_new==leaf_count_old) next
        
        cost <- (t$calc_risk() - t0$calc_risk()) / (leaf_count_old - leaf_count_new)
        
        if(cost < min_cost){
            min_cost<- cost
            tp <- t
        }
    }
    

    possible_trees <- get_remaining_subtrees(tp, possible_trees)
    return(c(list(tp), rec_pruning_sequence(tp, possible_trees)))
}


#' Generate Pruning Sequence
#'
#' Generates a sequence of subtrees using weakest link pruning from the original tree.
#'
#' @param t The original decision tree.
#' @return A list of subtrees representing the pruning sequence (t(0), ..., t(p)).
get_pruning_sequence_it <- function(t){
    possible_trees <- get_sub_trees(t)
    pruning_sequence <- list(t)
    
    t0 <- t
    next_leaf_count <- length(t0$get_leaves())
    
    while(!t0$is_leaf(1)){
        leaf_count_old <- next_leaf_count
        min_cost <- Inf
        for (t1 in possible_trees){
            leaf_count_new <- length(t1$get_leaves())
            if(leaf_count_new==leaf_count_old) next
            
            cost <- (t1$risk - t0$risk) / (leaf_count_old - leaf_count_new)
            
            if(cost < min_cost){
                min_cost<- cost
                tp <- t1
                next_leaf_count <- leaf_count_new
            }
        }
        pruning_sequence <- c(pruning_sequence, list(tp))
        t0 <- tp
        possible_trees <- get_remaining_subtrees(t0, possible_trees)
    }
    
    return(pruning_sequence)
}

#' Get Remaining Subtrees
#'
#' Filters the list of possible subtrees to include only those that are
#' subtrees of a specified tree.
#'
#' @param t0 The reference tree to compare against.
#' @param possible_trees A list of all possible subtrees.
#' @return A filtered list of subtrees that are subtrees of `t0`.
get_remaining_subtrees <- function(t0, possible_trees ){
    nodes <- c(1,  t0$data[!is.na(t0$data[, 'y']), 'index'])
    del_trees <- NULL
    for (i in seq_along(possible_trees)){
        if (!all(possible_trees[[i]]$data[!is.na(possible_trees[[i]]$data[, 'y']), 'index'] %in% nodes)){
            del_trees <- c(i, del_trees)
        }
    }
    possible_trees[del_trees] <- NULL
    return(possible_trees)
}



#' Generate Possible Subtrees
#'
#' Generates all possible subtrees of the original tree that share the same root.
#'
#' @export
#' @param t The original decision tree.
#' @return A vector of subtrees.
get_sub_trees <- function(t){
    t_sub <- Tree$new(t$training_data_x, t$training_data_y)
    t_sub$type <- t$type
    do.call(t_sub$set_values, c(list(1), as.list(t$get_root()[1, c('s', 'j', 'y')]), list(recalcRisk=FALSE)))
    if(is.na(t_sub$data[1, 'y'])) t_sub$data[1, 'y'] <- sum(t$training_data_x)/length(t$training_data_x) #set missing y value for root
    subtrees <- rec_sub_trees(t, t_sub)
    x_mask <- subtrees[[1]]$get_x_mask()
    for(s in subtrees) s$calc_risk(x_mask, force=TRUE)
    return(subtrees)
}

#' Recursive Subtree Generation
#'
#'Should not be used by itself!
#'
#' A helper function that recursively generates all possible subtrees of a given tree.
#'
#' @param t_original The original decision tree.
#' @param t_sub The current subtree in the generation process.
#' @return A list of subtrees generated from `t_original`.

rec_sub_trees <- function(t_original, t_sub){
    leaves <- t_sub$get_leaves()
    for (l in leaves){
        leafChildren  <- t_original$get_children(l)
        if(is.na(t_sub$get_s(l))) next #node marked as leaf
        if (all(is.na(leafChildren))) next
        
        t_one <- t_sub$copy()
        t_one$add_children(index=l, 
                           s1=leafChildren[[1]][1, 's'], j1=leafChildren[[1]][1, 'j'], y1=leafChildren[[1]][1, 'y'], 
                           s2=leafChildren[[2]][1, 's'], j2=leafChildren[[2]][1, 'j'], y2=leafChildren[[2]][1, 'y'],
                           recalcRisk = FALSE)
        t_sub$mark_leave(l, recalcRisk = FALSE)
        return (c(rec_sub_trees(t_original, t_one$copy()), rec_sub_trees(t_original, t_sub$copy())))
    }
    return(t_sub)
    
}



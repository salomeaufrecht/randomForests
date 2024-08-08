
#' Cost-Complexity Pruning
#'
#' This function performs cost-complexity pruning on a decision tree using cross-validation to select the optimal complexity parameter (\eqn{\lambda}).
#'
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
    Im <- makePartition(t$training_data_x, m)
    sequences <- list(0)
    
    if(print_progress)cat("creating pruning sequence ")
    for (i in 1:m){
        if(print_progress)cat(i, ",")
        sequences[[i]] <- (getPruningSequence(
                                greedy(matrix(t$training_data_x[Im!=i]) , 
                                       matrix(t$training_data_y[Im!=i]))))
    }
    
    if(print_progress) cat("\nall pruning sequences created\n")
    n <- length(t$training_data_x)
    lambdas <- seq(lambda_min, lambda_max, lambda_step)
    CVs <- NULL
    best_lambda <- NULL
    min_CV <- .Machine$integer.max
    if(print_progress) cat("checking lambda: ")
    for (i in seq_along(lambdas)){
        if(print_progress) cat(lambdas[i], ",")
        CVs[i] <- CV(lambdas[i], sequences, n, Im,t$training_data_x, t$training_data_y)
    }
    if(print_progress) cat("\n")
    if(plot) plot(lambdas, CVs)
    lambda <- lambdas[which.min(CVs)]
    if(print_progress) cat("chosen lambda: ", lambda, "\n")
    if(print_progress) cat("calculating resulting tree \n")

    return(chooseTpLambda(lambda, getPruningSequence(t)))
}

#' Cross-Validation Error Calculation
#'
#' Calculates the cross-validation error for a given complexity parameter \eqn{\lambda} 
#' by pruning a decision trees with different training data and evaluating its performance on validation sets.
#'
#' @param lambda The complexity parameter for pruning.
#' @param sequences A list of pruning sequences generated from different training data.
#' @param n The number of observations in the complete training data.
#' @param Im The partition indices for cross-validation.
#' @param training_data_x The training data.
#' @param training_data_y The training data.
#' @return The cross-validation error for the given \eqn{\lambda}.
CV <- function(lambda, sequences, n, Im, training_data_x, training_data_y){
    sum <- 0
    for (m in seq_along(sequences)){
        t <- chooseTpLambda(lambda, sequences[[m]])
        inner_sum <- 0
        for (i in (1:n)[Im==m]){
            inner_sum <- inner_sum + L(t, training_data_y[i], t$f(training_data_x[i]))
        }
        sum <- sum + inner_sum
    }
    return((1/n) *sum)
}

#' Loss Function
#'
#' Computes the loss between the true label and the predicted label based on the type of tree (classification or regression).
#'
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
#' @param x The training data.
#' @param m The number of partitions to create. Default is 10.
#' @return A vector indicating the partition assignments for each observation.
makePartition <- function(x, m=10){
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
#' @param lambda The complexity parameter for pruning.
#' @param pruningSequence A list of subtrees generated during the pruning process.
#' @return The subtree that minimizes the cost-complexity criterion.
chooseTpLambda <- function(lambda, pruningSequence){
    min_value <- .Machine$integer.max
    tLambda <- NULL
    for(t in pruningSequence){
        val <- t$calcRisk() + lambda * length(t$get_leaves())
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
#' @param t The original decision tree.
#' @return A list of subtrees representing the pruning sequence (t(0), ..., t(p)).
getPruningSequence <- function(t){
    possibleTrees <- getSubTrees(t)

    return(c(t, recPruningSequence(t,  possibleTrees)))
}


#' Recursive Pruning Sequence
#'
#' Should not be used by itself!
#' 
#' A helper function that recursively generates the pruning sequence for the given tree.
#'
#' @param t0 The current tree in the pruning process.
#' @param possibleTrees A list of possible subtrees that can be generated from the current tree.
#' @return A list of subtrees in the pruning sequence.

recPruningSequence <- function(t0, possibleTrees){
    if(t0$is_leaf(1)) return()
    leaf_count_old <- length(t0$get_leaves())
    min_cost <- .Machine$integer.max
 
    for (t in possibleTrees){
        leaf_count_new <- length(t$get_leaves())
        if(leaf_count_new==leaf_count_old) next
        
        cost <- (t$calcRisk() - t0$calcRisk()) / (leaf_count_old - leaf_count_new)
        
        if(cost < min_cost){
            min_cost<- cost
            tp <- t$copy()
        }
    }
    

    possibleTrees <- getRemainingSubtrees(tp, possibleTrees)
    return(c(list(tp), recPruningSequence(tp, possibleTrees)))
}

#' Get Remaining Subtrees
#'
#' Filters the list of possible subtrees to include only those that are
#' subtrees of a specified tree.
#'
#' @param t0 The reference tree to compare against.
#' @param possibleTrees A list of all possible subtrees.
#' @return A filtered list of subtrees that are subtrees of `t0`.
getRemainingSubtrees <- function(t0, possibleTrees ){
    nodes <- c(1,  t0$data[!is.na(t0$data[, 'y']), 'index'])
    delTrees <- NULL
    for (i in seq_along(possibleTrees)){
        if (!all(possibleTrees[[i]]$data[!is.na(possibleTrees[[i]]$data[, 'y']), 'index'] %in% nodes)){
            delTrees <- c(i, delTrees)
        }
    }
    possibleTrees[delTrees] <- NULL
    return(possibleTrees)
}



#' Generate Possible Subtrees
#'
#' Generates all possible subtrees of the original tree that share the same root.
#'
#' @param t The original decision tree.
#' @return A vector of subtrees.
getSubTrees <- function(t){
    t_sub <- Tree$new(t$training_data_x, t$training_data_y)
    t_sub$type <- t$type
    do.call(t_sub$set_values, c(list(1), as.list(t$get_root()[1, c('s', 'j', 'y')]), list(recalcRisk=FALSE)))
    if(is.na(t_sub$data[1, 'y'])) t_sub$data[1, 'y'] <- sum(t$training_data_x)/length(t$training_data_x) #set missing y value for root
    subtrees <- recSubTrees(t, t_sub)
    xMask <- subtrees[[1]]$getXMask()
    for(s in subtrees) s$calcRisk(xMask, force=TRUE)
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

recSubTrees <- function(t_original, t_sub){
    leaves <- t_sub$get_leaves()
    for (l in leaves){
        leafChildren  <- t_original$get_children(l)
        if(is.na(t_sub$getS(l))) next #node marked as leaf
        if (all(is.na(leafChildren))) next
        
        t_one <- t_sub$copy()
        t_one$add_children(index=l, 
                           s1=leafChildren[[1]][1, 's'], j1=leafChildren[[1]][1, 'j'], y1=leafChildren[[1]][1, 'y'], 
                           s2=leafChildren[[2]][1, 's'], j2=leafChildren[[2]][1, 'j'], y2=leafChildren[[2]][1, 'y'],
                           recalcRisk = FALSE)
        t_sub$makeLeaf(l, recalcRisk = FALSE)
        return (c(recSubTrees(t_original, t_one), recSubTrees(t_original, t_sub)))
    }
    return(t_sub)
    
}



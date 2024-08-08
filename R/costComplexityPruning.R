
#' this is an example documentation
#' 
#' there are more tags...
#' compile with devtools::document()
#' @export costComplexityPruning
#' @param x whatever
#' 
costComplexityPruning <- function(t, lambda_min=1, lambda_max=100, lambda_step=1, plot=FALSE, m=10, print_progress=FALSE){
    
    stopifnot("m need to be greater 1"= m>=2)
    Im <- makePartition(t$training_data_x, m)
    sequences <- NULL
    sequencesRisk <- NULL
    
    if(print_progress)cat("creating pruning sequence ")
    for (i in 1:m){
        if(print_progress)cat(i, ",")
        trees_risk <- (getPruningSequence(
                                greedy(matrix(t$training_data_x[Im!=i]) , 
                                       matrix(t$training_data_y[Im!=i]))))
        sequences[i] <- list(trees_risk$trees)
        sequencesRisk[[i]] <- trees_risk$risk
        rm(trees_risk)  
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
        CVs[i] <- CV(lambdas[i], sequences, sequencesRisk, n, Im,t$training_data_x, t$training_data_y)
    }
    if(print_progress) cat("\n")
    if(plot) plot(lambdas, CVs)
    lambda <- lambdas[which.min(CVs)]
    if(print_progress) cat("chosen lambda: ", lambda, "\n")
    if(print_progress) cat("calculating resulting tree \n")
    trees_risk_t <- getPruningSequence(t)
    return(chooseTpLambda(lambda, trees_risk_t$trees, trees_risk_t$risk))
}


CV <- function(lambda, sequences, sequencesRisk, n, Im, training_data_x, training_data_y){
    sum <- 0
    for (m in seq_along(sequences)){
        t <- chooseTpLambda(lambda, sequences[[m]], sequencesRisk[[m]])
        inner_sum <- 0
        for (i in (1:n)[Im==m]){
            inner_sum <- inner_sum + L(t, training_data_y[i], t$f(training_data_x[i]))
        }
        sum <- sum + inner_sum
    }
    return((1/n) *sum)
}

L <- function(t, y, s){
    if (t$type == "classification"){
        return(ifelse(y!=s, 1, 0))
    }
    return((y-s)^2)
}

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

chooseTpLambda <- function(lambda, pruningSequence, pruningSequenceRisk){
    min_value <- .Machine$integer.max
    tLambda <- NULL
    for(i in seq_along(pruningSequence)){
        t <- pruningSequence[[i]]$copy()
        val <- pruningSequenceRisk[[i]] + lambda * length(t$get_leaves())
        if(val<min_value){
            min_value <- val
            tLambda <- t$copy()
            }
    }
    return(tLambda)
}


#' Pruning Sequence
#' 
#' calculates trees t(0), ..., t(p) according to weakest link pruning
#' 
#' @param t original tree
#' @return list of trees t(0), ..., t(p)
getPruningSequence <- function(t){
    xMask <- getXMask(t)
    possibleTrees <- getSubTrees(t)
    possibleTreesRisk <- list(0)
    for (i in seq_along(possibleTrees)) possibleTreesRisk[[i]] <- calcRisk(possibleTrees[[i]], xMask)
    risk <- possibleTreesRisk[[1]]
    trees_risk <- recPruningSequence(t, risk, possibleTrees, possibleTreesRisk)
    return(list(trees=c(t, trees_risk$trees), 
                risk=c(risk, trees_risk$risk)) )
}


#' Recursive Pruning Sequence
#' 
#' Recursive helper function for getPruningSequence(t)
#' Not to be used other than in getPruningSequence(t)
#' 
#' @param t0 tree
#' @param xMask see getXMask(t)
#' @param possibleTrees list of subtrees of t0
recPruningSequence <- function(t0, t0_risk, possibleTrees, possibleTreesRisk){
    if(t0$is_leaf(1)) return()
    leaf_count_old <- length(t0$get_leaves())
    min_cost <- .Machine$integer.max
    tp_index <- NULL
    
    for (i in seq_along(possibleTrees)){
        risk_new <- possibleTreesRisk[[i]]
        leaf_count_new <- length(possibleTrees[[i]]$get_leaves())
        if(leaf_count_new==leaf_count_old) next
        cost <- (risk_new - t0_risk) / (leaf_count_old - leaf_count_new)
        if(cost < min_cost){
            min_cost<- cost
            tp_index <- i
        }
    }
    
    tp <- possibleTrees[[tp_index]]$copy()
    risk <- possibleTreesRisk[[tp_index]]
    remaining <- getRemainingSubtrees(tp, possibleTrees, possibleTreesRisk)
    possibleTrees <- remaining$trees
    possibleTreesRisk <- remaining$risk
    trees_risk <- recPruningSequence(tp, risk, possibleTrees, possibleTreesRisk)
    return(list(trees=c(list(tp), trees_risk$trees), 
                risk=c(risk, trees_risk$risk)) )
}

#' get remaining Subtrees
#' 
#' given a set of subtrees and a specific subtree t0 from this set. we now calculate 
#' a subset of the subtrees in which all subtrees are subtrees of t0
#' 
#' @param t0 given tree
#' @param possibleTrees set of all subtrees.
#' @return set of all remaining subtrees (as list)
#' 
getRemainingSubtrees <- function(t0, possibleTrees, possibleTreesRisk){
    nodes <- c(1,  t0$data[!is.na(t0$data[, 'y']), 'index'])
    delTrees <- NULL
    for (i in seq_along(possibleTrees)){
        if (!all(possibleTrees[[i]]$data[!is.na(possibleTrees[[i]]$data[, 'y']), 'index'] %in% nodes)){
            delTrees <- c(i, delTrees)
        }
    }
    possibleTrees[delTrees] <- NULL
    possibleTreesRisk[delTrees] <- NULL
    return(list(trees=possibleTrees, risk=possibleTreesRisk))
}

#' calculates Risk
#' 
#' calcualtes risk for a given tree
#' 
#' @param t tree
#' @param xMask vektor with TRUE/FALSE mask for every node (leaf) for trainig_data_x. See getXMask(t)
calcRisk <- function (t, xMask=getXMask(t)){
    risk <- 0
    leaves <- t$get_leaves()
    
    if (t$type == "classification"){
        for(l in leaves){
            risk <- risk+ sum((t$training_data_y[xMask[[l]]] != t$data[l, 'y']))#/ sum(xMask[[l]])
        }
    }
    else{
        for(l in leaves){
            risk <- risk+ sum((t$training_data_y[xMask[[l]]]-t$data[l, 'y'])^2)#/ sum(xMask[[l]])
        }
    }
    return(risk/length(t$training_data_x))
}



#' Mask for X values
#' 
#' assignes TRUE/FALSE values for every value from training_data_x for every node. TRUE when x is put into bucket of node (when following split points)
getXMask <- function(t0){
    xMask <- list(0) #which x values 'go this way'
    xMask[[1]] <- rep(TRUE, nrow(t0$training_data_x))
    parents <- t0$data[!is.na(t0$data[, 's']), 'index']
    parents <- parents[!parents %in% t0$get_leaves()]
    for(p in parents){
        child_indices <- t0$get_child_indices(p)
        xMask[[child_indices[1]]] <- t0$training_data_x < t0$data[p, 's'] & xMask[[p]]
        xMask[[child_indices[2]]] <- !xMask[[child_indices[1]]] & xMask[[p]]
    }
    return(xMask)
}

#' Get possible subtrees
#' 
#' calculates all possible subtrees with same root like given tree (needed for pruning)
#' @param t tree
#' @return vector of subtrees 
getSubTrees <- function(t){
    t_sub <- Tree$new(t$training_data_x, t$training_data_y)
    t_sub$type <- t$type
    do.call(t_sub$set_values, c(list(1), as.list(t$get_root()[1, c('s', 'j', 'y')])))
    if(is.na(t_sub$data[1, 'y'])) t_sub$data[1, 'y'] <- sum(t$training_data_x)/length(t$training_data_x)
    subtrees <- recSubTrees(t, t_sub)
    return(subtrees)
}

#' recursive helper function for getSubTree
#' @param t_original original tree
#' @param t_sub subtree currently working on (either append child or don't)
#' @return list of subtrees
recSubTrees <- function(t_original, t_sub){
    leaves <- t_sub$get_leaves()
    for (l in leaves){
        leafChildren  <- t_original$get_children(l)
        if(is.na(t_sub$getS(l))) next #node marked as leaf
        if (all(is.na(leafChildren))) next
        
        
        t_one <- t_sub$copy()
        
        t_one$add_children(index=l, 
                           s1=leafChildren[[1]][1, 's'], j1=leafChildren[[1]][1, 'j'], y1=leafChildren[[1]][1, 'y'], 
                           s2=leafChildren[[2]][1, 's'], j2=leafChildren[[2]][1, 'j'], y2=leafChildren[[2]][1, 'y'])
        t_sub$makeLeaf(l)
        return (c(recSubTrees(t_original, t_one), recSubTrees(t_original, t_sub)))
    }
    return(t_sub)
    
}



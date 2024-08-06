
#' this is an example documentation
#' 
#' there are more tags...
#' compile with devtools::document()
#' @export costComplexityPruning
#' @param x whatever
#' 
costComplexityPruning <- function(x) x


choosePLambda <- function(t0, lambda){
    pruningSequence <- getPruningSequence(t0)
    min_value <- .Machine$integer.max
    tLambda <- NULL
    for(t in pruningSequence){
        val <- calcRisk(t) + lambda * length(t$getLeaves())
        if(val<min_value) tLambda <- t
    }
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
    recPruningSequence(t, xMask, possibleTrees)
}


#' Recursive Pruning Sequence
#' 
#' Recursive helper function for getPruningSequence(t)
#' Not to be used other than in getPruningSequence(t)
#' 
#' @param t0 tree
#' @param xMask see getXMask(t)
#' @param possibleTrees list of subtrees of t0
recPruningSequence <- function(t0, xMask, possibleTrees){
    if(t0$is_leaf(1)) return()
    risk_old <- calcRisk(t0, xMask)
    leaf_count_old <- length(t0$getLeaves())
    min_cost <- .Machine$integer.max
    tp <- NULL
    
    for (t in possibleTrees){
        risk_new <- calcRisk(t, xMask)
        leaf_count_new <- length(t$getLeaves())
        if(leaf_count_new==leaf_count_old) next
        cost <- (risk_new - risk_old) / (leaf_count_old - leaf_count_new)
        if(cost < min_cost){
            min_cost<- cost
            tp <- t
        }
    }
    
    possibleTrees <- getRemainingSubtrees(tp, possibleTrees)
    return(c(tp, recPruningSequence(tp, xMask, possibleTrees)))
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
getRemainingSubtrees <- function(t0, possibleTrees){
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

#' calculates Risk
#' 
#' calcualtes risk for a given tree
#' 
#' @param t tree
#' @param xMask vektor with TRUE/FALSE mask for every node (leaf) for trainig_data_x. See getXMask(t)
calcRisk <- function (t, xMask){
    if (t$type == "classification"){
        leaves <- t$getLeaves()
        risk <- 0
        for(l in leaves){
            risk <- risk+ sum((t$training_data_x[xMask[[l]]] != t$data[l, 'y']))/ sum(xMask[[l]])
        }
        return(risk)
    }
    else{
        leaves <- t$getLeaves()
        risk <- 0
        for(l in leaves){
            risk <- risk+ sum((t$training_data_x[xMask[[l]]]-t$data[l, 'y'])^2)/ sum(xMask[[l]])
        }
        return(risk)
    }
}


#' Mask for X values
#' 
#' assignes TRUE/FALSE values for every value from training_data_x for every node. TRUE when x is put into bucket of node (when following split points)
getXMask <- function(t0){
    xMask <- list(0) #which x values 'go this way'
    xMask[[1]] <- rep(TRUE, nrow(t0$training_data_x))
    parents <- t0$data[!is.na(t0$data[, 's']), 'index']
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
#' @param t_sub subtree currently working on (eighter append child or don't)
#' @return list of subtrees
recSubTrees <- function(t_original, t_sub){
    leaves <- t_sub$getLeaves()
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



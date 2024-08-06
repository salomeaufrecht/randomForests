X <- 1:12
Y <- 1:12
#t <- greedy(    matrix(X, ncol=1),    matrix(Y, ncol=1))
#' this is an example documentation
#' 
#' there are more tags...
#' compile with devtools::document()
#' @export costComplexityPruning
#' @param x whatever
#' 
costComplexityPruning <- function(x) x

getPruningSequence <- function(t0){
    xMask <- getXMask(t0)
    
    
    recPruningSequence(t0, xMask)
    
}

recPruningSequence <- function(t0, xMask){
    if(t0$is_leaf(1)) return()
    risk_old <- calcRisk(t0, xMask)
    leaf_count_old <- length(t0$getLeaves())
    min_cost <- .Machine$integer.max
    tp <- NULL
    possibleTrees <- getSubTrees(t0)
    for (t in possibleTrees){
        risk_new <- calcRisk(t, xMask)
        leaf_count_new <- length(t$getLeaves())
        if(leaf_count_new==leaf_count_old) next
        cost <- (risk_new - risk_old) / (leaf_count_old - leaf_count_new)
        cat("t, cost, min_cost", cost, min_cost, "\n")
        if(cost < min_cost){
            min_cost<- cost
            tp <- t
        }
    }
    return(c(tp, recPruningSequence(tp, xMask)))
}

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
#' assignes TRUE/FALSE values for every x value for every node. TRUE when x is put into bucket of node when following split points
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

#' creates array with corresponding y value for each node (if cut at this node)
#' @param t tree
#' @return array with y values t index positions of nodes
buildPruningTree <- function(t){
    childNodes <- t$getLeaves()
    nodesValues <- NULL
    nodesValues[childNodes] <- t$data[childNodes, 'y']
    recPruningTree(t, childNodes, nodesValues)
}

#' recursive help function
#' @param t tree
#' @param childNodes array of indices currently seen as child nodes.
#' @param nodesValues array of y values 
recPruningTree <- function(t, childNodes, nodesValues){
    root <- FALSE
    nextChildren <- NULL
    
    for (child in childNodes){ #calc value for parent nodes
        parent <- t$get_parent_index(child)
        if(parent==0){
            root <- TRUE
            next
        }
        if(is.na(nodesValues[parent])){
            nodesValues[parent] <- sum(nodesValues[t$get_child_indices(parent)])/2
            nextChildren <- c(nextChildren, parent)
            root <- FALSE
        }
    }
    if(root) return(nodesValues)
    recPruningTree(t, nextChildren, nodesValues)
}


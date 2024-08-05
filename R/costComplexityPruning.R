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
    possibleTrees <- getSubTrees(t0)
}


getSubTrees <- function(t){
    t_sub <- Tree$new(t$training_data_x, t$training_data_y)
    do.call(t_sub$set_values, c(list(1), as.list(t$get_root()[1, c('s', 'j', 'y')])))
    subtrees <- recSubTrees(t, t_sub)
    return(subtrees)
}

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

cutNode <- function(t, nodeID){
    if(t$is_leaf(nodeID)) return 
    
}

calcErr <- function (tree, f, n){
    if (tree$type == "classification"){
        return (1/n * sum(1))
    }
    else{
        return(1/n * sum(2))
    }
}